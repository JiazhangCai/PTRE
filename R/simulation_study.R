# ==============================================================================
# PTRE — Simulation study
# ------------------------------------------------------------------------------
# Reproduces the simulation results reported in the revised manuscript.
#
# Three experiments:
#   (exp1) Effect of incubation-period noise on a BA scale-free network
#          — sweep sigma_log from 0.1 to 1.0
#   (exp2) Cross-network comparison (BA / ER / WS) at three noise levels
#   (exp3) Effect of contact-tracing detection probability p_detect
#
# Run from the repo root:
#
#   source("R/simulation_study.R")
#
# Outputs are written to output/sim_output/ as <exp>_raw.csv and
# <exp>_summary.csv.
# ==============================================================================

suppressPackageStartupMessages({
  library(igraph)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(TSCAN)
})

source("R/ptre_pipeline.R")

# ------------------------------------------------------------------------------
# 0. Configuration
# ------------------------------------------------------------------------------
set.seed(2026)

SIM_OUTPUT <- "output/sim_output"
dir.create(SIM_OUTPUT, showWarnings = FALSE, recursive = TRUE)

N_REP <- 30   # reduce for a quick smoke test, increase for the full study

# ==============================================================================
# PART 1: SIMULATION ENGINE
# ==============================================================================

# --- 1A. Generate a contact network ------------------------------------------
generate_network <- function(n, type = "BA", params = list()) {
  if (type == "ER") {
    mean_deg <- ifelse(is.null(params$mean_deg), 4, params$mean_deg)
    g <- erdos.renyi.game(n, p = mean_deg / (n - 1), type = "gnp",
                          directed = FALSE)
  } else if (type == "BA") {
    m <- ifelse(is.null(params$m), 2, params$m)
    g <- barabasi.game(n, m = m, directed = FALSE)
  } else if (type == "WS") {
    nei      <- ifelse(is.null(params$nei), 3, params$nei)
    p_rewire <- ifelse(is.null(params$p_rewire), 0.15, params$p_rewire)
    g <- watts.strogatz.game(1, n, nei, p_rewire)
  } else {
    stop("Unknown network type: ", type)
  }

  comps <- components(g)
  if (comps$no > 1) {
    gcc <- which.max(comps$csize)
    g   <- induced_subgraph(g, which(comps$membership == gcc))
  }
  V(g)$name <- paste0("N", seq_len(vcount(g)))
  g
}

# --- 1B. Simulate an SIR epidemic on the network -----------------------------
simulate_epidemic <- function(g, beta = 0.3,
                               gamma_shape = 2.56, gamma_rate = 0.64,
                               inc_meanlog = 1.63, inc_sdlog = 0.50,
                               max_infections = NULL, seed_node = NULL,
                               use_heterogeneous_susc = TRUE) {

  n <- vcount(g)
  if (is.null(max_infections)) max_infections <- floor(n * 0.6)

  node_names     <- V(g)$name
  status         <- setNames(rep("S", n), node_names)
  infection_time <- setNames(rep(NA_real_, n), node_names)
  parent         <- setNames(rep(NA_character_, n), node_names)
  generation     <- setNames(rep(NA_integer_, n), node_names)

  susceptibility <- if (use_heterogeneous_susc) {
    setNames(runif(n, 0.5, 1.5), node_names)
  } else {
    setNames(rep(1.0, n), node_names)
  }

  if (is.null(seed_node)) seed_node <- sample(node_names, 1)
  status[seed_node]         <- "I"
  infection_time[seed_node] <- 0
  generation[seed_node]     <- 0

  active_queue <- data.frame(node = seed_node, inf_time = 0,
                              stringsAsFactors = FALSE)
  transmission_edges <- data.frame(
    from = character(0), to = character(0),
    inf_time_from = numeric(0), inf_time_to = numeric(0),
    stringsAsFactors = FALSE
  )
  n_infected <- 1

  while (nrow(active_queue) > 0 && n_infected < max_infections) {
    active_queue <- active_queue[order(active_queue$inf_time), ]
    current      <- active_queue[1, ]
    active_queue <- active_queue[-1, , drop = FALSE]
    cur_node <- current$node
    cur_time <- current$inf_time

    nbrs <- neighbors(g, cur_node)$name
    susc <- nbrs[status[nbrs] == "S"]
    if (length(susc) == 0) next

    for (new_node in susc) {
      if (n_infected >= max_infections) break
      if (status[new_node] != "S") next
      eff_beta <- beta * susceptibility[new_node]
      if (runif(1) >= eff_beta) next

      gen_interval <- rgamma(1, shape = gamma_shape, rate = gamma_rate)
      new_inf_time <- cur_time + gen_interval

      status[new_node]         <- "I"
      infection_time[new_node] <- new_inf_time
      parent[new_node]         <- cur_node
      generation[new_node]     <- generation[cur_node] + 1
      n_infected <- n_infected + 1

      active_queue <- rbind(active_queue,
                             data.frame(node = new_node,
                                        inf_time = new_inf_time,
                                        stringsAsFactors = FALSE))
      transmission_edges <- rbind(transmission_edges, data.frame(
        from = cur_node, to = new_node,
        inf_time_from = cur_time, inf_time_to = new_inf_time,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Symptom onset = infection_time + LogNormal incubation period (integer days)
  infected_nodes <- node_names[!is.na(infection_time)]
  onset_time     <- setNames(rep(NA_real_, n), node_names)
  for (nd in infected_nodes) {
    inc_period <- rlnorm(1, meanlog = inc_meanlog, sdlog = inc_sdlog)
    onset_time[nd] <- floor(infection_time[nd] + inc_period)
  }

  list(
    infected_nodes     = infected_nodes,
    infection_time     = infection_time,
    onset_time         = onset_time,
    parent             = parent,
    generation         = generation,
    transmission_edges = transmission_edges,
    graph              = g
  )
}

# --- 1C. Generate observed contact-tracing records ---------------------------
# p_detect: probability of observing each true transmission edge
# p_extra : probability of observing each non-transmission contact edge
generate_contact_data <- function(epi, p_detect = 0.80, p_extra = 0.55) {
  g        <- epi$graph
  infected <- epi$infected_nodes

  g_inf <- induced_subgraph(g, infected)
  el    <- as_edgelist(g_inf)
  all_edges <- data.frame(n1 = el[, 1], n2 = el[, 2], stringsAsFactors = FALSE)

  trans_key <- paste(epi$transmission_edges$from,
                     epi$transmission_edges$to, sep = "|")
  k_fwd <- paste(all_edges$n1, all_edges$n2, sep = "|")
  k_rev <- paste(all_edges$n2, all_edges$n1, sep = "|")
  all_edges$is_transmission <- (k_fwd %in% trans_key) | (k_rev %in% trans_key)

  t_idx  <- which(all_edges$is_transmission)
  nt_idx <- which(!all_edges$is_transmission)
  det_t  <- if (length(t_idx) > 0) t_idx[runif(length(t_idx)) < p_detect] else integer(0)
  det_nt <- if (length(nt_idx) > 0) nt_idx[runif(length(nt_idx)) < p_extra] else integer(0)
  observed <- all_edges[c(det_t, det_nt), ]

  max_gen <- max(epi$generation[infected], na.rm = TRUE)
  case <- data.frame(
    id             = infected,
    infection_time = epi$infection_time[infected],
    onset_time     = epi$onset_time[infected],
    generation     = epi$generation[infected],
    stringsAsFactors = FALSE
  )

  n_inf    <- nrow(case)
  gen_frac <- case$generation / max(1, max_gen)

  # Demographic features (plausibly correlated with generation)
  case$age <- pmax(5, pmin(85,
      round(50 - 10 * case$generation + rnorm(n_inf, 0, 5))))
  sev_prob <- pmin(0.90, pmax(0.02,
      0.05 + 0.60 * gen_frac + 0.20 * (case$age > 60)))
  case$severity <- rbinom(n_inf, 1, sev_prob)

  occ_probs <- matrix(0, nrow = n_inf, ncol = 6)
  for (i in seq_len(n_inf)) {
    gf <- case$generation[i] / max(1, max_gen)
    if (gf < 0.2) {
      occ_probs[i, ] <- c(0.50, 0.25, 0.10, 0.05, 0.05, 0.05)
    } else if (gf < 0.5) {
      occ_probs[i, ] <- c(0.10, 0.20, 0.30, 0.20, 0.10, 0.10)
    } else {
      occ_probs[i, ] <- c(0.02, 0.08, 0.20, 0.20, 0.30, 0.20)
    }
  }
  case$occupation <- sapply(seq_len(n_inf),
                            function(i) sample(1:6, 1, prob = occ_probs[i, ]))
  case$n_contacts <- pmax(1,
      round(15 - 1.5 * case$generation + rpois(n_inf, 3)))

  # True direction for every observed edge
  observed$true_dir <- NA_character_
  for (i in seq_len(nrow(observed))) {
    k1 <- paste(observed$n1[i], observed$n2[i], sep = "|")
    k2 <- paste(observed$n2[i], observed$n1[i], sep = "|")
    if      (k1 %in% trans_key) observed$true_dir[i] <- "n1_to_n2"
    else if (k2 %in% trans_key) observed$true_dir[i] <- "n2_to_n1"
  }

  list(case_data     = case,
       observed_edges = observed,
       n_trans_detected = length(det_t),
       n_trans_total    = nrow(epi$transmission_edges))
}

# ==============================================================================
# PART 2: PTRE WRAPPER (SIMULATION-SIDE)
# ==============================================================================

sim_ptre <- function(case_data, observed_edges, mode = "full_w") {
  # Build contact graph + degree / out_degree / position features
  g <- graph_from_data_frame(
    observed_edges[, c("n1", "n2")],
    directed = FALSE, vertices = case_data$id
  )
  g <- simplify(g)
  case_data$degree <- as.integer(degree(g, v = case_data$id))

  if (mode %in% c("full_w", "full_wo")) {
    e_dir <- observed_edges[, c("n1", "n2")]
    onset_lut <- setNames(case_data$onset_time, case_data$id)
    e_dir$on1 <- onset_lut[e_dir$n1]
    e_dir$on2 <- onset_lut[e_dir$n2]
    swap <- !is.na(e_dir$on1) & !is.na(e_dir$on2) & e_dir$on1 > e_dir$on2
    tmp <- e_dir$n1[swap]
    e_dir$n1[swap] <- e_dir$n2[swap]
    e_dir$n2[swap] <- tmp

    g_dir <- graph_from_data_frame(e_dir[, c("n1", "n2")],
                                    directed = TRUE, vertices = case_data$id)
    case_data$out_degree <- as.integer(degree(g_dir, v = case_data$id, mode = "out"))

    case_data$position <- 0.5
    for (i in seq_len(nrow(case_data))) {
      nd <- case_data$id[i]
      ov <- case_data$onset_time[i]
      if (is.na(ov)) next
      d <- distances(g, v = nd)[1, ]
      d <- d[is.finite(d)]
      os <- onset_lut[names(d)]
      bef <- names(d)[!is.na(os) & os <= ov]
      aft <- names(d)[!is.na(os) & os >  ov]
      bef <- union(bef, nd); aft <- union(aft, nd)
      db <- if (length(bef) > 0) max(d[bef]) else 0
      da <- if (length(aft) > 0) max(d[aft]) else 0
      case_data$position[i] <- if ((db + da) > 0) db / (db + da) else 0.5
    }
  }

  feat_cols <- switch(
    mode,
    full_w    = c("degree", "out_degree", "onset_time", "severity", "position",
                  "occupation", "age", "n_contacts"),
    full_wo   = c("degree", "out_degree", "severity", "position",
                  "occupation", "age", "n_contacts"),
    feat_only = c("age", "severity", "occupation", "n_contacts"),
    stop("Unknown mode: ", mode)
  )
  anchor <- if (mode == "full_w") "onset_time" else "pc1"
  run_ptre(case_data, feat_cols, anchor_by = anchor, onset_col = "onset_time")
}

# ==============================================================================
# PART 3: EVALUATION (ONSET / DEGREE / PTRE)
# ==============================================================================

# Extends the core evaluate_direction() with a degree-heuristic baseline
# (degree_lut); compared to ptre_pipeline.R's version, this one also adds
# the degree predictor side-by-side.
sim_evaluate <- function(case_data, observed_edges,
                          pseudo_lut = NULL, degree_lut = NULL) {
  e <- observed_edges[!is.na(observed_edges$true_dir), , drop = FALSE]
  if (nrow(e) == 0) return(NULL)

  onset_lut <- setNames(case_data$onset_time, case_data$id)
  e$onset1    <- onset_lut[e$n1]
  e$onset2    <- onset_lut[e$n2]
  e$onset_gap <- abs(e$onset1 - e$onset2)

  e$onset_pred <- ifelse(e$onset1 < e$onset2, "n1_to_n2",
                    ifelse(e$onset1 > e$onset2, "n2_to_n1", "tie"))

  if (!is.null(degree_lut)) {
    e$deg1 <- degree_lut[e$n1]
    e$deg2 <- degree_lut[e$n2]
    e$net_only_pred <- ifelse(e$deg1 > e$deg2, "n1_to_n2",
                           ifelse(e$deg1 < e$deg2, "n2_to_n1", "tie"))
    e$net_only_correct <- e$net_only_pred == e$true_dir
  }

  if (!is.null(pseudo_lut)) {
    e$p1 <- pseudo_lut[e$n1]
    e$p2 <- pseudo_lut[e$n2]
    e <- e[!is.na(e$p1) & !is.na(e$p2), , drop = FALSE]

    e$ptre_raw  <- ifelse(e$p1 < e$p2, "n1_to_n2", "n2_to_n1")
    e$ptre_flip <- ifelse(e$p1 > e$p2, "n1_to_n2", "n2_to_n1")

    raw  <- mean(e$ptre_raw  == e$true_dir, na.rm = TRUE)
    flip <- mean(e$ptre_flip == e$true_dir, na.rm = TRUE)
    flip_flag <- as.integer(flip > raw)
    e$ptre_correct <- if (flip_flag == 1) e$ptre_flip == e$true_dir
                      else                e$ptre_raw  == e$true_dir
  }

  e$onset_correct <- e$onset_pred == e$true_dir
  e$gap_stratum   <- cut(e$onset_gap,
                          breaks = c(-Inf, 0, 1, 3, Inf),
                          labels = c("0_tie", "1_day", "2_3_days", "gt3_days"),
                          right = TRUE)

  strat <- e %>%
    group_by(gap_stratum) %>%
    summarise(
      n            = dplyr::n(),
      onset_acc    = mean(onset_correct[onset_pred != "tie"], na.rm = TRUE),
      onset_n      = sum(onset_pred != "tie"),
      net_only_acc = if (!is.null(degree_lut)) mean(net_only_correct[net_only_pred != "tie"], na.rm = TRUE) else NA_real_,
      ptre_acc     = if (!is.null(pseudo_lut)) mean(ptre_correct, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )

  overall <- data.frame(
    gap_stratum  = "All",
    n            = nrow(e),
    onset_acc    = mean(e$onset_correct[e$onset_pred != "tie"], na.rm = TRUE),
    onset_n      = sum(e$onset_pred != "tie"),
    net_only_acc = if (!is.null(degree_lut)) mean(e$net_only_correct[e$net_only_pred != "tie"], na.rm = TRUE) else NA_real_,
    ptre_acc     = if (!is.null(pseudo_lut)) mean(e$ptre_correct, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )

  list(
    edges         = e,
    strat_summary = dplyr::bind_rows(strat, overall)
  )
}

# ==============================================================================
# PART 4: EXPERIMENT DEFINITIONS
# ==============================================================================

exp1_scenarios <- expand.grid(
  network   = "BA",
  n         = 300,
  beta      = 0.3,
  inc_sdlog = seq(0.1, 1.0, by = 0.1),
  p_detect  = 0.80,
  p_extra   = 0.55,
  stringsAsFactors = FALSE
)

exp2_scenarios <- expand.grid(
  network   = c("BA", "ER", "WS"),
  n         = 300,
  beta      = 0.3,
  inc_sdlog = c(0.3, 0.5, 0.7),
  p_detect  = 0.80,
  p_extra   = 0.55,
  stringsAsFactors = FALSE
)

exp3_scenarios <- expand.grid(
  network   = "BA",
  n         = 300,
  beta      = 0.3,
  inc_sdlog = 0.5,
  p_detect  = seq(0.5, 1.0, by = 0.1),
  p_extra   = 0.55,
  stringsAsFactors = FALSE
)

# ==============================================================================
# PART 5: SINGLE-SCENARIO RUNNER
# ==============================================================================

run_single_scenario <- function(sc, rep_id, verbose = FALSE) {
  set.seed(2026 * 1000 + sc$scenario_id * 100 + rep_id)

  net_params <- list(mean_deg = 4, m = 2, nei = 3, p_rewire = 0.15)
  g  <- generate_network(sc$n, type = sc$network, params = net_params)
  ep <- simulate_epidemic(g, beta = sc$beta,
                          gamma_shape = 2.56, gamma_rate = 0.64,
                          inc_meanlog = 1.63, inc_sdlog = sc$inc_sdlog,
                          max_infections = floor(vcount(g) * 0.6),
                          use_heterogeneous_susc = TRUE)

  if (length(ep$infected_nodes) < 30 ||
      nrow(ep$transmission_edges) < 20) return(NULL)

  ct <- generate_contact_data(ep, p_detect = sc$p_detect, p_extra = sc$p_extra)
  if (nrow(ct$observed_edges) < 10) return(NULL)

  # Degree lookup for degree-heuristic baseline
  g_obs <- graph_from_data_frame(ct$observed_edges[, c("n1", "n2")],
                                  directed = FALSE, vertices = ct$case_data$id)
  g_obs <- simplify(g_obs)
  degree_lut <- setNames(as.integer(degree(g_obs, v = ct$case_data$id)),
                          ct$case_data$id)

  ptre_w  <- sim_ptre(ct$case_data, ct$observed_edges, mode = "full_w")
  ptre_wo <- sim_ptre(ct$case_data, ct$observed_edges, mode = "full_wo")
  ptre_f  <- sim_ptre(ct$case_data, ct$observed_edges, mode = "feat_only")

  ev_w  <- if (ptre_w$success)  sim_evaluate(ct$case_data, ct$observed_edges,
                                              ptre_w$pseudotime,  degree_lut) else NULL
  ev_wo <- if (ptre_wo$success) sim_evaluate(ct$case_data, ct$observed_edges,
                                              ptre_wo$pseudotime, degree_lut) else NULL
  ev_f  <- if (ptre_f$success)  sim_evaluate(ct$case_data, ct$observed_edges,
                                              ptre_f$pseudotime,  degree_lut) else NULL

  res <- data.frame(
    scenario_id  = sc$scenario_id,
    network      = sc$network,
    n            = sc$n,
    beta         = sc$beta,
    inc_sdlog    = sc$inc_sdlog,
    p_detect     = sc$p_detect,
    rep          = rep_id,
    n_infected   = length(ep$infected_nodes),
    n_trans_det  = ct$n_trans_detected,
    n_obs_edges  = nrow(ct$observed_edges),
    full_w_ok    = ptre_w$success,
    full_wo_ok   = ptre_wo$success,
    feat_only_ok = ptre_f$success,
    stringsAsFactors = FALSE
  )

  if (!is.null(ev_w)) {
    for (r in seq_len(nrow(ev_w$strat_summary))) {
      s  <- as.character(ev_w$strat_summary$gap_stratum[r])
      res[[paste0("n_",         s)]] <- ev_w$strat_summary$n[r]
      res[[paste0("onset_",     s)]] <- ev_w$strat_summary$onset_acc[r]
      res[[paste0("net_only_",  s)]] <- ev_w$strat_summary$net_only_acc[r]
      res[[paste0("ptre_w_",    s)]] <- ev_w$strat_summary$ptre_acc[r]
    }
  }
  if (!is.null(ev_wo)) {
    for (r in seq_len(nrow(ev_wo$strat_summary))) {
      s <- as.character(ev_wo$strat_summary$gap_stratum[r])
      res[[paste0("ptre_wo_", s)]] <- ev_wo$strat_summary$ptre_acc[r]
    }
  }
  if (!is.null(ev_f)) {
    for (r in seq_len(nrow(ev_f$strat_summary))) {
      s <- as.character(ev_f$strat_summary$gap_stratum[r])
      res[[paste0("feat_only_", s)]] <- ev_f$strat_summary$ptre_acc[r]
    }
  }
  res
}

# ==============================================================================
# PART 6: RUN AN EXPERIMENT
# ==============================================================================

run_experiment <- function(scenarios, exp_name, n_rep = N_REP, verbose = TRUE) {
  scenarios$scenario_id <- seq_len(nrow(scenarios))

  if (verbose) {
    cat(sprintf("\n========== %s ==========\n", exp_name))
    cat(sprintf("Scenarios: %d, Reps: %d, Total runs: %d\n",
                nrow(scenarios), n_rep, nrow(scenarios) * n_rep))
  }

  all_res <- list(); idx <- 0
  for (s in seq_len(nrow(scenarios))) {
    sc <- scenarios[s, ]
    if (verbose) cat(sprintf("\n Scenario %d/%d: net=%s, sdlog=%.1f, p_detect=%.2f\n",
                               s, nrow(scenarios), sc$network,
                               sc$inc_sdlog, sc$p_detect))
    for (rep in seq_len(n_rep)) {
      r <- run_single_scenario(sc, rep)
      if (!is.null(r)) {
        idx <- idx + 1
        all_res[[idx]] <- r
      }
      if (verbose && rep %% 10 == 0) cat(sprintf("  Completed %d/%d reps\n", rep, n_rep))
    }
  }

  if (length(all_res) == 0) {
    warning("No successful runs for ", exp_name)
    return(NULL)
  }

  results_df <- dplyr::bind_rows(all_res)
  write.csv(results_df,
            file.path(SIM_OUTPUT, paste0(exp_name, "_raw.csv")),
            row.names = FALSE)

  acc_cols <- grep("^(onset_|ptre_w_|ptre_wo_|feat_only_|net_only_)",
                    names(results_df), value = TRUE)
  acc_cols <- acc_cols[!grepl("ok$", acc_cols)]

  summary_df <- results_df %>%
    group_by(scenario_id, network, n, beta, inc_sdlog, p_detect) %>%
    summarise(
      n_runs          = dplyr::n(),
      mean_n_infected = mean(n_infected, na.rm = TRUE),
      mean_n_trans    = mean(n_trans_det, na.rm = TRUE),
      across(all_of(acc_cols), ~mean(.x, na.rm = TRUE),
             .names = "mean_{.col}"),
      .groups = "drop"
    )

  write.csv(summary_df,
            file.path(SIM_OUTPUT, paste0(exp_name, "_summary.csv")),
            row.names = FALSE)

  list(raw = results_df, summary = summary_df)
}

# ==============================================================================
# PART 7: EXECUTION
# ==============================================================================
# Comment out individual experiments if you only want to run part of the study.

exp1 <- run_experiment(exp1_scenarios, "exp1_sdlog_sweep_BA", n_rep = N_REP)
exp2 <- run_experiment(exp2_scenarios, "exp2_cross_network",  n_rep = N_REP)
exp3 <- run_experiment(exp3_scenarios, "exp3_pdetect_sweep",  n_rep = N_REP)

cat("\nSimulation study complete. Outputs in", SIM_OUTPUT, "\n")
