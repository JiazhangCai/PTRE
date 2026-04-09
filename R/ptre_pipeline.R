# ==============================================================================
# PTRE: Pseudo-Time Reconstruction for Epidemics
# Core pipeline functions (reusable helpers)
# ------------------------------------------------------------------------------
# This file defines the building blocks used by `analysis_covid.R` (empirical
# analysis on the Zhejiang COVID-19 dataset) and by `simulation_study.R`
# (controlled simulation study). Source it before running either script:
#
#   source("R/ptre_pipeline.R")
#
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(igraph)
  library(TSCAN)
  library(pROC)
})

# ------------------------------------------------------------------------------
# 1. Helpers: oriented accuracy / AUC for sign-ambiguous pseudo-time scores
# ------------------------------------------------------------------------------
# Pseudo-time is determined only up to a global sign flip. For classification-
# style evaluation we report the better of the two orientations (raw vs flipped)
# while remembering which orientation was selected so that the same convention
# can be applied across edge strata.

oriented_acc <- function(y, pred01) {
  acc_raw  <- mean(pred01 == y, na.rm = TRUE)
  acc_flip <- 1 - acc_raw
  list(
    acc_raw      = acc_raw,
    acc_flip     = acc_flip,
    acc_oriented = max(acc_raw, acc_flip),
    flip_flag    = as.integer(acc_flip > acc_raw)
  )
}

oriented_auc <- function(y, score) {
  keep <- !is.na(y) & !is.na(score)
  if (sum(keep) < 5 || length(unique(y[keep])) < 2) {
    return(list(auc_raw = NA_real_, auc_flip = NA_real_,
                auc_oriented = NA_real_, flip_flag = NA_integer_))
  }
  roc_raw <- tryCatch(pROC::roc(y, score,  quiet = TRUE), error = function(e) NULL)
  roc_flp <- tryCatch(pROC::roc(y, -score, quiet = TRUE), error = function(e) NULL)
  auc_raw <- if (!is.null(roc_raw)) as.numeric(pROC::auc(roc_raw)) else NA_real_
  auc_flp <- if (!is.null(roc_flp)) as.numeric(pROC::auc(roc_flp)) else NA_real_
  list(
    auc_raw      = auc_raw,
    auc_flip     = auc_flp,
    auc_oriented = max(auc_raw, auc_flp, na.rm = TRUE),
    flip_flag    = as.integer(auc_flp > auc_raw)
  )
}

# ------------------------------------------------------------------------------
# 2. Block-wise position feature
# ------------------------------------------------------------------------------
# position(i) ∈ [0, 1] is a depth-ratio summary derived from the undirected
# contact graph + onset ordering. It is computed block-wise so that memory
# stays O(block_size × n) instead of O(n²).

compute_position_block <- function(g, node_ids, onset_vals, block_size = 50) {
  n   <- length(node_ids)
  pos <- setNames(rep(NA_real_, n), node_ids)
  for (b in seq_len(ceiling(n / block_size))) {
    idx <- ((b - 1) * block_size + 1):min(b * block_size, n)
    blk <- node_ids[idx]
    dm  <- distances(g, v = blk, to = node_ids)
    if (length(blk) == 1) dm <- matrix(dm, nrow = 1,
                                        dimnames = list(blk, node_ids))
    rownames(dm) <- blk
    colnames(dm) <- node_ids
    for (i in seq_along(blk)) {
      nd <- blk[i]
      ov <- onset_vals[nd]
      if (is.na(ov)) next
      dr  <- dm[i, ]
      rc  <- is.finite(dr)
      oc  <- onset_vals[names(dr)]
      bef <- rc & !is.na(oc) & oc <= ov
      aft <- rc & !is.na(oc) & oc >  ov
      bef[nd] <- TRUE
      aft[nd] <- TRUE
      db <- if (any(bef)) max(dr[bef]) else 0
      da <- if (any(aft)) max(dr[aft]) else 0
      pos[nd] <- if ((db + da) > 0) db / (db + da) else 0.5
    }
    rm(dm)
  }
  pos
}

# ------------------------------------------------------------------------------
# 3. Core PTRE pipeline on an arbitrary feature table
# ------------------------------------------------------------------------------
# Inputs:
#   feat_df        data.frame — one row per case, must contain an `id` column
#                  plus all feature columns listed in `feat_cols`.
#   feat_cols      character vector of columns to use as PTRE input features.
#   anchor_by      "onset_time" to order clusters by mean onset (classic TSCAN
#                  choice), or "pc1" to order clusters by mean PC1 (used when
#                  onset time is not a feature).
#   onset_col      column in feat_df holding onset time, only used when
#                  anchor_by = "onset_time".
# Returns a named list with $success and (when successful) $pseudotime — a
# named numeric vector indexed by case id.

run_ptre <- function(feat_df, feat_cols,
                     anchor_by = c("onset_time", "pc1"),
                     onset_col = "onset_time") {

  anchor_by <- match.arg(anchor_by)
  if (!"id" %in% names(feat_df)) stop("feat_df must contain an 'id' column")

  missing_cols <- setdiff(feat_cols, names(feat_df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in feat_df: ", paste(missing_cols, collapse = ", "))
  }

  fm <- feat_df[, feat_cols, drop = FALSE]
  rownames(fm) <- feat_df$id
  fm <- fm[complete.cases(fm), , drop = FALSE]
  if (nrow(fm) < 10) {
    return(list(success = FALSE, reason = "Too few complete cases"))
  }

  # Scale features to z-scores so that features with large numerical range
  # (e.g. degree on BA networks) don't dominate PCA.
  fm_scaled <- as.data.frame(scale(as.matrix(fm)))
  fm_scaled[is.na(fm_scaled)] <- 0
  rownames(fm_scaled) <- rownames(fm)

  feat_t <- t(as.matrix(fm_scaled))

  tryCatch({
    lps     <- exprmclust(feat_t, reduce = TRUE)
    clu_ids <- sort(unique(lps$clusterid))

    if (anchor_by == "onset_time") {
      onset_lut <- setNames(feat_df[[onset_col]], feat_df$id)
      means <- sapply(clu_ids, function(cid) {
        nms <- names(lps$clusterid[lps$clusterid == cid])
        mean(onset_lut[nms], na.rm = TRUE)
      })
      mst_order <- clu_ids[order(means)]
    } else {
      pc1 <- lps$pcareduceres[, 1]
      means <- sapply(clu_ids, function(cid) {
        nms <- names(lps$clusterid[lps$clusterid == cid])
        mean(pc1[nms], na.rm = TRUE)
      })
      mst_order <- clu_ids[order(means)]
    }

    tsorder <- TSCANorder(lps, MSTorder = mst_order)
    pseudo_lut <- setNames(tsorder$Pseudotime, tsorder$sample_name)

    list(success   = TRUE,
         pseudotime = pseudo_lut,
         tsorder    = tsorder,
         lps        = lps,
         mst_order  = mst_order)
  }, error = function(e) {
    list(success = FALSE, reason = as.character(e$message))
  })
}

# ------------------------------------------------------------------------------
# 4. Edge-level direction evaluation
# ------------------------------------------------------------------------------
# Given a pseudo-time lookup and a set of edges with known reference directions,
# compute onset-based and pseudo-time-based directional accuracy, stratified by
# absolute onset gap.

evaluate_direction <- function(edges, pseudo_lut,
                                onset_lut,
                                gap_breaks = c(-Inf, 0, 1, 3, Inf),
                                gap_labels = c("0_tie", "1_day",
                                               "2_3_days", "gt3_days")) {

  stopifnot(all(c("n1", "n2", "true_dir") %in% names(edges)))
  edges <- edges[!is.na(edges$true_dir), , drop = FALSE]
  if (nrow(edges) == 0) return(NULL)

  edges$onset1    <- onset_lut[edges$n1]
  edges$onset2    <- onset_lut[edges$n2]
  edges$onset_gap <- abs(edges$onset1 - edges$onset2)

  # Onset-only prediction
  edges$onset_pred <- ifelse(
    edges$onset1 < edges$onset2, "n1_to_n2",
    ifelse(edges$onset1 > edges$onset2, "n2_to_n1", "tie")
  )

  # Pseudo-time prediction
  edges$pseudo1 <- pseudo_lut[edges$n1]
  edges$pseudo2 <- pseudo_lut[edges$n2]
  edges <- edges[!is.na(edges$pseudo1) & !is.na(edges$pseudo2), , drop = FALSE]

  edges$ptre_pred_raw  <- ifelse(edges$pseudo1 < edges$pseudo2,
                                  "n1_to_n2", "n2_to_n1")
  edges$ptre_pred_flip <- ifelse(edges$pseudo1 > edges$pseudo2,
                                  "n1_to_n2", "n2_to_n1")

  edges$onset_correct     <- edges$onset_pred     == edges$true_dir
  edges$ptre_correct_raw  <- edges$ptre_pred_raw  == edges$true_dir
  edges$ptre_correct_flip <- edges$ptre_pred_flip == edges$true_dir

  acc_raw  <- mean(edges$ptre_correct_raw,  na.rm = TRUE)
  acc_flip <- mean(edges$ptre_correct_flip, na.rm = TRUE)
  flip_flag <- as.integer(acc_flip > acc_raw)
  edges$ptre_correct <- if (flip_flag == 1) edges$ptre_correct_flip else edges$ptre_correct_raw

  edges$gap_stratum <- cut(edges$onset_gap, breaks = gap_breaks,
                            labels = gap_labels, right = TRUE)

  strat <- edges %>%
    group_by(gap_stratum) %>%
    summarise(
      n              = dplyr::n(),
      onset_acc      = mean(onset_correct[onset_pred != "tie"], na.rm = TRUE),
      onset_n_nontie = sum(onset_pred != "tie"),
      ptre_acc       = mean(ptre_correct, na.rm = TRUE),
      .groups = "drop"
    )

  overall <- data.frame(
    gap_stratum    = "All",
    n              = nrow(edges),
    onset_acc      = mean(edges$onset_correct[edges$onset_pred != "tie"], na.rm = TRUE),
    onset_n_nontie = sum(edges$onset_pred != "tie"),
    ptre_acc       = mean(edges$ptre_correct, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  list(
    edges         = edges,
    strat_summary = dplyr::bind_rows(strat, overall),
    flip_flag     = flip_flag
  )
}

# ------------------------------------------------------------------------------
# 5. Subsampling-based ordering stability
# ------------------------------------------------------------------------------
# Repeatedly subsample a fraction of edges from a contact graph and re-run PTRE
# to measure how stable the induced relative ordering is. Returns a numeric
# matrix of pseudo-time ranks (rows = cases, cols = replicates).

subsample_stability <- function(feat_df, edges, feat_cols,
                                  anchor_by = "onset_time",
                                  onset_col = "onset_time",
                                  frac = 0.80, n_rep = 50, seed = 42) {
  stopifnot(all(c("n1", "n2") %in% names(edges)))
  set.seed(seed)

  n_edges <- nrow(edges)
  ranks   <- matrix(NA_real_, nrow = nrow(feat_df), ncol = n_rep,
                    dimnames = list(feat_df$id, paste0("rep", seq_len(n_rep))))

  for (r in seq_len(n_rep)) {
    keep <- sample.int(n_edges, size = floor(frac * n_edges))
    sub_edges <- edges[keep, , drop = FALSE]

    # Rebuild graph and recompute degree-based features
    g_sub <- graph_from_data_frame(sub_edges[, c("n1", "n2")],
                                    directed  = FALSE,
                                    vertices  = feat_df$id)
    feat_sub <- feat_df
    feat_sub$degree <- as.integer(degree(g_sub, v = feat_df$id))

    out <- run_ptre(feat_sub, feat_cols,
                    anchor_by = anchor_by, onset_col = onset_col)
    if (!out$success) next

    pseudo <- out$pseudotime
    ord    <- rank(pseudo[feat_df$id])
    ranks[, r] <- ord
  }

  ranks
}
