# Data

This directory contains the contact-tracing dataset used in the empirical
analysis, in two forms:

1. **Raw source files** (`case.csv`, `COVID19_edge.csv`) — used directly by
   `R/analysis_covid.R` when `DIRECTION_DATE = "onset_date"` and the full
   feature set (age, occupation, severity) is required.
2. **De-identified summary files** (`data_all_nodes.csv`, `data_all_edges.csv`)
   — a compact node/edge view suitable for quick inspection and for the
   subsampling-stability analysis.

All identifiers have been replaced with anonymized case labels (e.g.
`an_1124`, `bai_293`). Dates are kept at day resolution.

## Files

### `case.csv` — raw case table

One row per confirmed case. Key columns:

| Column              | Description                                                        |
|---------------------|--------------------------------------------------------------------|
| `newID`             | Internal study identifier.                                         |
| `id2`               | Anonymized case label (primary key used by the R scripts).        |
| `gender2`           | `female` / `male`.                                                 |
| `guardian_id`       | Guardian's case label if the case is a minor; otherwise `NA`.      |
| `age_year`          | Age in years.                                                      |
| `case_dignosis2`    | `confirmed_cases` for all records in this file.                    |
| `confirm_date`      | Laboratory confirmation date (UTC timestamp).                      |
| `onset_date`        | Reported symptom-onset date (UTC timestamp); may be `NA`.          |
| `severity2`         | `mild` / `severe` / `asymptomatic`.                                |
| `province2`, `city2`| Province and city of residence.                                    |
| `contact1_id` … `contact4_id` | Case labels of up to four documented close contacts.     |
| `wuhan`, `hubei_no_wuhan`, `no_hubei_wuhan` | Travel-history flags.                        |
| `self_quarantine`, `facility_quarantine`    | Quarantine status at diagnosis.              |
| `passive_case_finding`, `active_case_finding` | How the case was detected.                 |
| `cluster`           | Cluster identifier if the case is part of a known cluster.         |
| `family_index2`     | Family-cluster index.                                              |
| `index`             | Row index (1 … N).                                                 |
| `occupation`        | Occupation string (Chinese). Recoded to integer `occ_num` in      |
|                     | `R/ptre_pipeline.R` via the mapping:                               |
|                     | 农民 → farmer, 商业服务 → business, 家务及待业 → unemployed,       |
|                     | 工人 → worker, 离退人员 → retired, 其他 → others.                  |

### `COVID19_edge.csv` — raw edge / reference-direction table

One row per documented contact-tracing record. Columns mirror `case.csv`
plus the transmission-direction fields:

| Column                 | Description                                                                |
|------------------------|----------------------------------------------------------------------------|
| `id2`                  | Anonymized case label (same key as `case.csv`).                            |
| `contact1_id` … `contact3_id` | Anonymized labels of contacts for this case.                         |
| `confirmed_parent_id`  | If non-empty, this column records a high-confidence inferred infector of   |
|                        | `id2` based on epidemiological investigation. The pair                     |
|                        | `(confirmed_parent_id → id2)` is used as a reference direction label      |
|                        | in the evaluation subset.                                                  |
| `family_index2`, `cluster`, `notes2` | Cluster / investigation metadata.                            |
| (other columns)        | Same meaning as in `case.csv`.                                             |

Reference-direction labels (rows where `confirmed_parent_id` is not empty)
were constructed according to the eligibility criteria in the manuscript
(Methods — "Reference direction labels and evaluation framework"):

1. Both endpoints are confirmed cases with a documented close-contact link.
2. Either (a) the onset gap is at least 3 days, or (b) independent
   epidemiological evidence (cluster membership, travel history, contact
   investigation notes) supports a specific direction.

These labels are used **solely for evaluation**. They are not used to train
or tune PTRE in any way.

### `data_all_nodes.csv` — de-identified node summary

Compact node table used by `R/analysis_covid.R` for the subsampling-
stability analysis and the figures.

| Column        | Description                                                             |
|---------------|-------------------------------------------------------------------------|
| `Id`          | Numeric row identifier (1 … N).                                         |
| `Label`       | Anonymized case label (matches `id2` in `case.csv`).                    |
| `degree`      | Number of documented close contacts (raw undirected degree).            |
| `onset_date2` | Symptom-onset day expressed as days since the earliest observed onset.  |

### `data_all_edges.csv` — de-identified edge summary

Compact edge table.

| Column      | Description                                                                        |
|-------------|------------------------------------------------------------------------------------|
| `from`      | Case label of one endpoint of the edge.                                            |
| `to`        | Case label of the other endpoint.                                                  |
| `Direction` | `"Directed"` if the pair has a high-confidence reference transmission direction (from → to); `"Undirected"` otherwise. |
| `Target`    | Numeric endpoint identifier (for plotting compatibility with Gephi / igraph).      |
| `Source`    | Numeric endpoint identifier (for plotting compatibility with Gephi / igraph).      |

## Reproducibility notes

- Running `R/analysis_covid.R` with the full raw files (`case.csv` +
  `COVID19_edge.csv`) reproduces the pseudo-time ordering of cases (up to a
  sign ambiguity), the demographic / occupation / severity-specific signal
  analyses, the subsampling-stability analysis, and the stratified accuracy
  comparison against onset-only prediction.
- Running `R/analysis_covid.R` with only the de-identified node/edge files
  reproduces the ordering and stability analyses but not the demographic
  signal analyses.

## Ethics & attribution

The dataset was collected as part of the public-health response to the
COVID-19 outbreak in Zhejiang Province, China (January–February 2020). All
identifiers have been anonymized prior to release. When using this data,
please cite the accompanying manuscript (see `CITATION.cff` in the
repository root).
