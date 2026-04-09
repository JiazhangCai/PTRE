# ==============================================================================
# PTRE — empirical analysis on the Zhejiang COVID-19 contact-tracing dataset
# ------------------------------------------------------------------------------
# Reproduces the empirical results reported in the main manuscript.
# Run from the repo root:
#
#   source("R/analysis_covid.R")
#
# Inputs:
#   data/data_all_nodes.csv
#   data/data_all_edges.csv
#
# Outputs (written to output/covid_analysis/):
#   node_pseudotime_<track>.csv       — pseudo-time for each case
#   edge_predictions_all_<track>.csv  — edge-level predictions
#   method_comparison_<track>.csv     — stratified accuracy summary
#   rank_stability.csv                — normalized IQR of ranks under subsampling
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(igraph)
  library(ggplot2)
  library(TSCAN)
})

source("R/ptre_pipeline.R")

# ------------------------------------------------------------------------------
# 0. Configuration
# ------------------------------------------------------------------------------
DATA_DIR   <- "data"
OUT_DIR    <- "output/covid_analysis"
SEED       <- 42
N_SUBSAMP  <- 50
SUBSAMP_FR <- 0.80
BLOCK_SIZE <- 50

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(SEED)

# Two evaluation tracks:
#   with_onset    — onset_num used as a PTRE feature; MST anchored by mean onset
#   without_onset — onset_num removed from features; MST anchored by mean PC1
TRACKS <- list(
  with_onset = list(
    feat_cols = c("degree", "out_degree", "onset_num",
                  "severity2", "position", "occ_num", "age_year"),
    anchor_by = "onset_time",
    label     = "with_onset"
  ),
  without_onset = list(
    feat_cols = c("degree", "out_degree",
                  "severity2", "position", "occ_num", "age_year"),
    anchor_by = "pc1",
    label     = "without_onset"
  )
)

# ------------------------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------------------------
nodes <- read.csv(file.path(DATA_DIR, "data_all_nodes.csv"),
                  stringsAsFactors = FALSE)
edges <- read.csv(file.path(DATA_DIR, "data_all_edges.csv"),
                  stringsAsFactors = FALSE)

cat("Nodes:", nrow(nodes), "\n")
cat("Edges:", nrow(edges), "\n")

# Canonicalize column names
# The provided de-identified files use:
#   nodes:  Id, Label, degree, onset_date2, age_year, occ_num, severity2
#   edges:  from, to, Direction ("Directed"/"Undirected"), Target (true_dir)
# Adjust here if your local column names differ.
if (!"onset_num" %in% names(nodes) && "onset_date2" %in% names(nodes)) {
  nodes$onset_num <- as.numeric(nodes$onset_date2)
}
if (!"id2" %in% names(nodes) && "Label" %in% names(nodes)) {
  nodes$id2 <- nodes$Label
}

# ------------------------------------------------------------------------------
# 2. Build undirected contact graph
# ------------------------------------------------------------------------------
edges_clean <- edges %>%
  filter(!is.na(from), !is.na(to), from != to) %>%
  mutate(edge_id_undir = paste(pmin(from, to), pmax(from, to), sep = "|")) %>%
  distinct(edge_id_undir, .keep_all = TRUE)

g_undir <- graph_from_data_frame(edges_clean[, c("from", "to")],
                                  directed = FALSE,
                                  vertices = nodes$id2)

nodes$degree <- as.integer(degree(g_undir, v = nodes$id2))
cat("Undirected graph:", vcount(g_undir), "nodes /",
    ecount(g_undir), "edges\n")

# ------------------------------------------------------------------------------
# 3. Build provisional directed graph (for out_degree, position)
# ------------------------------------------------------------------------------
onset_lut <- setNames(nodes$onset_num, nodes$id2)
dir_edges <- edges_clean
dir_edges$on1 <- onset_lut[dir_edges$from]
dir_edges$on2 <- onset_lut[dir_edges$to]
swap <- !is.na(dir_edges$on1) & !is.na(dir_edges$on2) & dir_edges$on1 > dir_edges$on2
tmp <- dir_edges$from[swap]
dir_edges$from[swap] <- dir_edges$to[swap]
dir_edges$to[swap]   <- tmp

g_dir <- graph_from_data_frame(dir_edges[, c("from", "to")],
                                directed = TRUE,
                                vertices = nodes$id2)
nodes$out_degree <- as.integer(degree(g_dir, v = nodes$id2, mode = "out"))

# ------------------------------------------------------------------------------
# 4. Position feature (block-wise to keep memory O(block × n))
# ------------------------------------------------------------------------------
cat("Computing position feature...\n")
pos_vec <- compute_position_block(
  g_undir,
  node_ids   = nodes$id2,
  onset_vals = onset_lut,
  block_size = BLOCK_SIZE
)
nodes$position <- pos_vec[nodes$id2]

# ------------------------------------------------------------------------------
# 5. Run PTRE for each evaluation track
# ------------------------------------------------------------------------------
# Prepare a standardized feature data.frame for the pipeline
feat_df <- nodes %>%
  mutate(id = id2, onset_time = onset_num) %>%
  select(id, onset_time, degree, out_degree, any_of(
    c("onset_num", "severity2", "position", "occ_num", "age_year")
  ))

# Attach reference edge directions
ref_edges <- edges_clean %>%
  mutate(
    true_dir = case_when(
      !is.na(Direction) & Direction == "Directed" ~ "n1_to_n2",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(n1 = from, n2 = to) %>%
  select(n1, n2, true_dir)

track_results <- list()

for (tr_name in names(TRACKS)) {
  tr <- TRACKS[[tr_name]]
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Track:", tr$label, "\n")
  cat(strrep("=", 60), "\n")

  ptre <- run_ptre(feat_df, feat_cols = tr$feat_cols,
                   anchor_by = tr$anchor_by,
                   onset_col = "onset_time")

  if (!ptre$success) {
    warning("PTRE failed for track ", tr$label, ": ", ptre$reason)
    next
  }

  # Save node-level pseudo-time
  nt <- data.frame(
    id         = names(ptre$pseudotime),
    pseudotime = as.numeric(ptre$pseudotime),
    stringsAsFactors = FALSE
  )
  write.csv(nt,
            file.path(OUT_DIR, paste0("node_pseudotime_", tr$label, ".csv")),
            row.names = FALSE)

  # Evaluate on labeled edges
  eval_res <- evaluate_direction(ref_edges,
                                  pseudo_lut = ptre$pseudotime,
                                  onset_lut  = onset_lut)

  if (!is.null(eval_res)) {
    write.csv(eval_res$edges,
              file.path(OUT_DIR, paste0("edge_predictions_all_", tr$label, ".csv")),
              row.names = FALSE)
    write.csv(eval_res$strat_summary,
              file.path(OUT_DIR, paste0("method_comparison_", tr$label, ".csv")),
              row.names = FALSE)
    cat("\nStratified accuracy summary:\n")
    print(eval_res$strat_summary)
  }

  track_results[[tr$label]] <- list(ptre = ptre, eval = eval_res)
}

# ------------------------------------------------------------------------------
# 6. Subsampling stability (normalized IQR of ranks)
# ------------------------------------------------------------------------------
cat("\nRunning subsampling stability analysis (", N_SUBSAMP, "reps)...\n", sep = "")

ranks_mat <- subsample_stability(
  feat_df   = feat_df,
  edges     = ref_edges %>% select(n1, n2),
  feat_cols = TRACKS$with_onset$feat_cols,
  anchor_by = "onset_time",
  onset_col = "onset_time",
  frac      = SUBSAMP_FR,
  n_rep     = N_SUBSAMP,
  seed      = SEED
)

norm_iqr <- apply(ranks_mat, 1, function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  (quantile(x, 0.75) - quantile(x, 0.25)) / nrow(ranks_mat)
})

stability_df <- data.frame(
  id       = rownames(ranks_mat),
  norm_iqr = as.numeric(norm_iqr),
  stringsAsFactors = FALSE
)
write.csv(stability_df,
          file.path(OUT_DIR, "rank_stability.csv"),
          row.names = FALSE)

cat("\nDone. Outputs written to:", OUT_DIR, "\n")
