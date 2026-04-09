# PTRE: Pseudo-Time Reconstruction for Epidemics

**Integrating contact-network structure and individual-level features to refine transmission-direction inference in contact-tracing data**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://cran.r-project.org/)

## Overview

PTRE (Pseudo-Time Reconstruction for Epidemics) is an analytical pipeline that
constructs a one-dimensional **latent ordering** of confirmed cases from
contact-tracing records. It integrates individual-level attributes (age,
occupation, severity) with contact-network summaries (degree, out-degree,
position) and, when available, symptom-onset timing, in order to resolve
**transmission direction along ambiguous contact edges** — especially when
symptom onset dates are close, equal, or missing.

PTRE is not a probabilistic transmission model and does not attempt to
reconstruct individual infection events. Instead, it provides a descriptive
ordering that complements onset-based heuristics in regimes where onset
timing is uninformative.

The pipeline was originally motivated by and applied to a dataset of 695
laboratory-confirmed COVID-19 cases with 541 documented close-contact
relationships collected in Zhejiang Province, China (January–February 2020).
We additionally validate PTRE through a simulation study on three canonical
network topologies (Barabási–Albert, Erdős–Rényi, Watts–Strogatz) with known
ground-truth transmission directions.

## Method in brief

```
      Features (individual + network)
                 │
                 ▼
      Standardize → PCA (low-dim embedding Z)
                 │
                 ▼
      Cluster (Gaussian mixture / mclust)
                 │
                 ▼
      Minimum spanning tree on cluster centroids → backbone 𝓑
                 │
                 ▼
      Project each case onto 𝓑 and measure geodesic distance from an
      anchor cluster (cluster with smallest median onset) → pseudo-time τᵢ
                 │
                 ▼
      Pseudo-time induces a relative ordering of cases, which is
      compared with onset-based ordering and, where available, with
      high-confidence reference transmission directions.
```

The implementation uses the `TSCAN` package (Ji & Ji, 2016) as the
trajectory-inference engine, adapted here for the contact-tracing setting.

## Repository layout

```
PTRE/
├── R/
│   ├── ptre_pipeline.R      # Core PTRE helper functions (reusable)
│   ├── analysis_covid.R     # End-to-end empirical analysis on Zhejiang data
│   ├── simulation_study.R   # Simulation study on BA / ER / WS networks
│   └── make_figures.R       # Generate figures used in the manuscript
├── data/
│   ├── README.md            # Full field dictionary for all files below
│   ├── case.csv             # Raw case table (anonymized IDs; Chinese occupation strings)
│   ├── COVID19_edge.csv     # Raw edge / reference-direction table
│   ├── data_all_nodes.csv   # Compact node summary used by analysis_covid.R
│   └── data_all_edges.csv   # Compact edge summary (Directed / Undirected)
├── figures/                 # Selected figures used in the manuscript
├── output/                  # Empty — populated when scripts are run
├── CITATION.cff
├── LICENSE
└── README.md
```

## Requirements

- R ≥ 4.0
- The following R packages:
  - CRAN: `dplyr`, `tidyr`, `ggplot2`, `ggridges`, `igraph`, `pROC`, `glmnet`, `mclust`
  - Bioconductor: `TSCAN` (install via `BiocManager::install("TSCAN")`)

Install everything at once:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "ggridges",
                   "igraph", "pROC", "glmnet", "mclust", "BiocManager"))
BiocManager::install("TSCAN")
```

## Quick start

### 1. Empirical analysis on the Zhejiang COVID-19 dataset

```r
# from the repo root
source("R/analysis_covid.R")
```

This script:

1. Loads the de-identified node and edge files from `data/`.
2. Constructs the contact network and derives individual-level features
   (degree, out-degree, position, age, occupation, severity, onset day).
3. Runs the PTRE pipeline in two tracks: `with_onset` (onset time included
   as a feature) and `without_onset` (onset time excluded, used as a
   leakage-free evaluation).
4. Evaluates directional-prediction accuracy against the reference direction
   labels, stratified by onset gap (0 day, 1 day, 2–3 days, ≥3 days).
5. Writes intermediate objects and summary tables to `output/`.

### 2. Simulation study

```r
source("R/simulation_study.R")
```

This script regenerates the simulation results described in the manuscript:

- **Experiment 1:** BA networks, `inc_sdlog` swept from 0.1 to 1.0 (10 levels).
- **Experiment 2:** Cross-network comparison on BA / ER / WS at three
  noise levels.
- **Experiment 3:** Varying contact-tracing detection probability `p_detect`.

Each scenario is replicated 30 times by default. With 30 replicates the full
run takes roughly 30–60 minutes on a modern laptop; set `N_REP` at the top
of the script to adjust.

Outputs are written to `output/sim_output/` as `*_raw.csv` and
`*_summary.csv`.

### 3. Figures

```r
source("R/make_figures.R")
```

Reads the files produced by the analysis and simulation scripts and writes
the corresponding figures to `figures/`.

## Data availability

The `data/` directory contains the Zhejiang contact-tracing dataset used
in the empirical analyses, in two complementary forms:

- `case.csv` and `COVID19_edge.csv` — the raw case and edge tables with the
  full set of attributes (age, occupation, severity, travel history,
  reference-direction labels). All case identifiers have been anonymized.
- `data_all_nodes.csv` and `data_all_edges.csv` — compact de-identified
  node/edge summaries suitable for quick inspection and for the
  subsampling-stability analysis.

See `data/README.md` for the full field dictionary, the occupation
recoding map, and the rules used to construct reference-direction labels.
When using this data, please cite the accompanying manuscript (see
`CITATION.cff`).

## Simulation design (summary)

Synthetic epidemics are generated on three canonical network topologies:

| Topology | Key parameter | Rationale |
|---|---|---|
| Barabási–Albert (BA) | `m = 2` (preferential attachment) | Heavy-tailed degree — models super-spreader-rich contact patterns |
| Erdős–Rényi (ER) | mean degree ≈ 4 | Homogeneous contact structure as a null comparator |
| Watts–Strogatz (WS) | k = 6, rewiring p = 0.15 | Small-world structure with high clustering |

On each network we simulate an SIR-type process with:

- Transmission probability per contact β = 0.3
- Generation interval ~ Gamma(shape = 2.56, rate = 0.64)
- Incubation period ~ LogNormal(meanlog = 1.63, sdlog = σ<sub>log</sub>), with
  σ<sub>log</sub> varied from 0.1 to 1.0
- Heterogeneous susceptibility ~ Uniform(0.5, 1.5)
- Contact tracing with detection probability `p_detect = 0.80` and background
  contact noise `p_extra = 0.55`

For each replicate, we run three PTRE modes — `full_w` (with onset),
`full_wo` (without onset), `feat_only` (demographic features only) — plus
an onset-only baseline and a degree-heuristic (network-only) baseline, and
evaluate directional accuracy stratified by onset gap.

## Key simulation results

On 0- and 1-day onset-gap pairs, onset-only accuracy degrades sharply as
incubation-period noise grows. At σ<sub>log</sub> = 1.0, onset-only accuracy
falls to 55.7% on BA, 47.4% on ER (below random), and 56.0% on WS networks.
PTRE without onset retains 71.4% (BA), 64.3% (ER), and 62.0% (WS) accuracy —
a 5.9–16.8 percentage-point advantage over onset-only. Across 30 scenarios,
PTRE beats onset-only on 1-day-gap pairs in 22/30 scenarios (73.3%), and in
22/24 scenarios (91.7%) when σ<sub>log</sub> ≥ 0.3.

Full results and reproducibility scripts are in `R/simulation_study.R` and
`output/sim_output/`.

## Citation

If you use PTRE in your work, please cite:

> Cai J., *et al.* (2026). Pseudo-Time Reconstruction for Epidemics: a
> network-aware framework for refining transmission-direction inference
> under onset-time ambiguity. *(Manuscript under revision.)*

A machine-readable citation is provided in [`CITATION.cff`](CITATION.cff).

Please also cite the underlying trajectory-inference engine:

> Ji Z. and Ji H. (2016). TSCAN: Pseudo-time reconstruction and evaluation
> in single-cell RNA-seq analysis. *Nucleic Acids Research*, 44(13), e117.

## License

This project is released under the MIT License — see [`LICENSE`](LICENSE).

## Contact

Jiazhang Cai — caijiazhang1 [at] gmail [dot] com
