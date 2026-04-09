# ==============================================================================
# PTRE — Figure generation
# ------------------------------------------------------------------------------
# Reads the summary CSV files written by simulation_study.R (under
# output/sim_output/) and analysis_covid.R (under output/covid_analysis/)
# and produces the figures used in the manuscript.
#
# Run from the repo root:
#
#   source("R/make_figures.R")
#
# Figures are written to figures/ as both PDF and PNG.
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

SIM_DIR <- "output/sim_output"
COV_DIR <- "output/covid_analysis"
FIG_DIR <- "figures"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Colour / style conventions (kept consistent with the manuscript figures)
# ------------------------------------------------------------------------------
method_colors <- c(
  "Onset"             = "#E69F00",
  "PTRE (w/ onset)"   = "#56B4E9",
  "PTRE (w/o onset)"  = "#009E73"
)
method_linetypes <- c(
  "Onset"             = "dashed",
  "PTRE (w/ onset)"   = "solid",
  "PTRE (w/o onset)"  = "solid"
)
method_shapes <- c(
  "Onset"             = 17,
  "PTRE (w/ onset)"   = 16,
  "PTRE (w/o onset)"  = 15
)

pub_theme <- theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 11),
    axis.title       = element_text(size = 12),
    plot.title       = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(size = 12, face = "bold")
  )

save_pair <- function(p, name, width, height) {
  ggsave(file.path(FIG_DIR, paste0(name, ".pdf")), p,
         width = width, height = height)
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p,
         width = width, height = height, dpi = 300)
}

# ==============================================================================
# FIGURE 1: Experiment 1 — sigma_log sweep on the BA network
# ==============================================================================

f1 <- file.path(SIM_DIR, "exp1_sdlog_sweep_BA_summary.csv")
if (file.exists(f1)) {
  df1 <- read.csv(f1)

  panel <- function(df, suffix, title) {
    df %>%
      select(inc_sdlog,
             Onset               = !!sym(paste0("mean_onset_",    suffix)),
             `PTRE (w/ onset)`   = !!sym(paste0("mean_ptre_w_",   suffix)),
             `PTRE (w/o onset)`  = !!sym(paste0("mean_ptre_wo_",  suffix))) %>%
      pivot_longer(-inc_sdlog, names_to = "Method", values_to = "Accuracy") %>%
      mutate(Method = factor(Method, levels = names(method_colors))) %>%
      ggplot(aes(x = inc_sdlog, y = Accuracy,
                 color = Method, linetype = Method, shape = Method)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey50") +
      scale_color_manual(values    = method_colors) +
      scale_linetype_manual(values = method_linetypes) +
      scale_shape_manual(values    = method_shapes) +
      scale_x_continuous(breaks = seq(0.1, 1.0, 0.1)) +
      scale_y_continuous(limits = c(0.4, 1.0), breaks = seq(0.4, 1.0, 0.1)) +
      pub_theme +
      labs(x = expression(sigma[log] ~ "(incubation-period variability)"),
           y = "Oriented accuracy",
           title = title)
  }

  p1a <- panel(df1, "1_day", "A) Pairs with 0-1 day onset gap")
  p1b <- panel(df1, "All",   "B) All pairs")

  save_pair(p1a, "sim_exp1_1day", 7, 5)
  save_pair(p1b, "sim_exp1_all",  7, 5)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    pc <- p1a + p1b + plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    save_pair(pc, "sim_exp1_combined", 12, 5)
  }
  cat("Experiment 1 figures written.\n")
} else {
  cat("Skipping exp1 figures — summary file not found:", f1, "\n")
}

# ==============================================================================
# FIGURE 2: Experiment 2 — cross-network comparison
# ==============================================================================

f2 <- file.path(SIM_DIR, "exp2_cross_network_summary.csv")
if (file.exists(f2)) {
  df2 <- read.csv(f2)

  plot_df <- df2 %>%
    select(network, inc_sdlog,
           Onset              = mean_onset_1_day,
           `PTRE (w/ onset)`  = mean_ptre_w_1_day,
           `PTRE (w/o onset)` = mean_ptre_wo_1_day) %>%
    pivot_longer(-c(network, inc_sdlog),
                 names_to = "Method", values_to = "Accuracy") %>%
    mutate(
      Method  = factor(Method, levels = names(method_colors)),
      network = factor(network, levels = c("BA", "ER", "WS"),
                        labels = c("Scale-free (BA)",
                                   "Random (ER)",
                                   "Small-world (WS)"))
    )

  p2 <- ggplot(plot_df, aes(x = network, y = Accuracy, fill = Method)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey50") +
    facet_wrap(~inc_sdlog, nrow = 1,
               labeller = labeller(inc_sdlog = function(x) paste0("\u03c3log = ", x))) +
    scale_fill_manual(values = method_colors) +
    scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.2)) +
    pub_theme +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 9)) +
    labs(x = "Network type",
         y = "Oriented accuracy (0-1 day gap)",
         title = "Cross-network comparison")

  save_pair(p2, "sim_exp2_crossnet", 10, 5)
  cat("Experiment 2 figure written.\n")
} else {
  cat("Skipping exp2 figure — summary file not found:", f2, "\n")
}

# ==============================================================================
# FIGURE 3: Experiment 3 — detection-probability sweep
# ==============================================================================

f3 <- file.path(SIM_DIR, "exp3_pdetect_sweep_summary.csv")
if (file.exists(f3)) {
  df3 <- read.csv(f3)

  plot_df <- df3 %>%
    select(p_detect,
           Onset              = mean_onset_1_day,
           `PTRE (w/ onset)`  = mean_ptre_w_1_day,
           `PTRE (w/o onset)` = mean_ptre_wo_1_day) %>%
    pivot_longer(-p_detect, names_to = "Method", values_to = "Accuracy") %>%
    mutate(Method = factor(Method, levels = names(method_colors)))

  p3 <- ggplot(plot_df, aes(x = p_detect, y = Accuracy,
                             color = Method, linetype = Method, shape = Method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey50") +
    scale_color_manual(values    = method_colors) +
    scale_linetype_manual(values = method_linetypes) +
    scale_shape_manual(values    = method_shapes) +
    scale_x_continuous(breaks = seq(0.5, 1.0, 0.1)) +
    scale_y_continuous(limits = c(0.4, 1.0), breaks = seq(0.4, 1.0, 0.1)) +
    pub_theme +
    labs(x = "Detection probability",
         y = "Oriented accuracy (0-1 day gap)",
         title = "Effect of case-detection rate (BA network)")

  save_pair(p3, "sim_exp3_pdetect", 7, 5)
  cat("Experiment 3 figure written.\n")
} else {
  cat("Skipping exp3 figure — summary file not found:", f3, "\n")
}

# ==============================================================================
# FIGURE 4: Empirical pseudo-time vs onset (with / without onset tracks)
# ==============================================================================

for (tr in c("with_onset", "without_onset")) {
  fp <- file.path(COV_DIR, paste0("node_pseudotime_", tr, ".csv"))
  if (!file.exists(fp)) {
    cat("Skipping pseudo-time plot for track", tr,
        "— file not found:", fp, "\n")
    next
  }
  dat <- read.csv(fp)
  if (!"onset_num" %in% names(dat)) {
    # Try to join with node file
    nf <- file.path("data", "data_all_nodes.csv")
    if (file.exists(nf)) {
      nodes <- read.csv(nf, stringsAsFactors = FALSE)
      if ("onset_date2" %in% names(nodes)) {
        nodes$onset_num <- as.numeric(nodes$onset_date2)
      }
      if ("Label" %in% names(nodes)) {
        nodes$id <- nodes$Label
      }
      dat <- dplyr::left_join(dat, nodes[, c("id", "onset_num")], by = "id")
    }
  }
  if (!"onset_num" %in% names(dat)) next

  p <- ggplot(dat[!is.na(dat$onset_num), ],
              aes(x = pseudotime, y = onset_num)) +
    geom_point(alpha = 0.5, size = 1.4) +
    geom_smooth(method = "loess", se = TRUE,
                color = "red", linewidth = 0.8, formula = y ~ x) +
    pub_theme +
    labs(x = "Pseudo-time",
         y = "Onset (days from first observed onset)",
         title = paste0("Pseudo-time vs onset (", tr, ")"))

  save_pair(p, paste0("covid_pseudotime_vs_onset_", tr), 7, 5)
}

cat("\nAll figures generated. Output directory:", FIG_DIR, "\n")
