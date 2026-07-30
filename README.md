# Education and Employment Exits During COVID-19: Evidence from Brazil — Replication Package

**Authors:** Francisco Cavalcanti, Fredie Didier, Gustavo Gonzaga

Replication package for the manuscript prepared for submission to **Labour
Economics**. The paper uses the rotating panel of Brazil's *PNAD Contínua* to
measure how the education gradient in the risk of leaving employment moved
through the COVID-19 pandemic, and decomposes that gradient into a composition
component (where graduates and non-graduates work) and a within-cell component
(the risk they face inside the same kind of job).

Every table, figure and number quoted in the manuscript is produced by the R
pipeline in `analysis/code`. Nothing numeric is typed into the LaTeX by hand.

## Repository layout

```
build/code/                  Micro-data construction
  00_master_build.R            runs 10-12 in order
  10_download_pnadc_quarters.R one cached parquet per PNADC quarter
  11_build_panels.R            stage-3 panel identification per rotation group
  12_build_main_data.R         t -> t+1 transitions + matching diagnostics
  legacy/                      superseded Stata + R build, kept for provenance
analysis/code/
  00_master_analysis.R         runs everything below, in order
  _config.R                    <-- THE ONLY FILE TO EDIT (paths per machine)
  _functions.R, _figures.R     estimation, margins, decomposition, plotting
  01_prepare_analysis_data.R … 09_paper_numbers.R, 99_session_info.R
  legacy/                      superseded R + Stata scripts, kept for provenance
analysis/input/matching/     matching diagnostics (committed CSVs)
analysis/output/             tables/, figures/, logs/ (committed); estimates/ (git-ignored cache)
latex/                       paper.tex, refs.bib, highlights.txt
dictionary/                  build_dictionary.R + variable_dictionary.xlsx
status.sh                    progress of a full rebuild
```

## Data

The microdata are **not** in this repository. They live in a shared (Dropbox)
folder:

```
<DROPBOX>/build/input/pnadc_panels/Panel_01..13.parquet   stage-3 rotation groups (step 11)
<DROPBOX>/build/output/main_data.parquet                  person-quarter transitions (step 12)
<DROPBOX>/build/output/analysis_sample.parquet            matched origins: estimation sample
<DROPBOX>/build/output/analysis_origins.parquet           all origins, for the attrition model
<DROPBOX>/build/output/main_data.dta                      previous vintage source (kept)
```

The estimation sample spans **2012Q1–2024Q4** (52 quarters, 13 rotation groups,
≈8.0 million person-quarter origins). Step 10 also writes a per-quarter download cache under
`<DROPBOX>/build/input/pnadc_quarters/`; it exists only to feed step 11, is safe
to delete once the thirteen panels are in place, and is not currently kept.

### Data availability

The underlying microdata are public and free from IBGE, *Pesquisa Nacional por
Amostra de Domicílios Contínua*:
<https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html>.
Nothing used here is restricted. Respondents are linked across interviews with
**datazoom.social** (PUC-Rio), `build_pnadc_panel(panel = "advanced_3")`.

### Getting the data — two options

- **Option A — rebuild from the raw microdata (currently the available path).**
  Run the build below; it downloads every PNADC quarter from IBGE and
  reconstructs the panels. No external download of a pre-built file is needed.
- **Option B — pre-built dataset.** On acceptance, a frozen snapshot of this
  repository together with the pre-built `main_data.parquet` will be archived on
  Zenodo with a permanent DOI. Once available, place `main_data.parquet` in
  `<DROPBOX>/build/output/` and skip step 2.

If Dropbox is set to *online-only*, a file has to be materialised before R can
read it, e.g. `cat main_data.parquet > /dev/null`.

## Reproducing the results

Clone the repository and enter it:

```bash
git clone https://github.com/FredieDidier/Monografia.git
```

1. **Set the data paths.** Open `Monografia.Rproj` (or otherwise set the working
   directory to the repository root), then add your machine to the two
   `switch()` blocks at the top of `analysis/code/_config.R`: `ROOT` (this
   repository) and `DROPBOX` (the data folder above). If your username is not
   listed, `ROOT` falls back to the working directory and `DROPBOX` to
   `build/output` inside the repository.

2. **Build the micro-data** (skip if `main_data.parquet` already exists):

   ```bash
   Rscript "build/code/00_master_build.R"
   ```

   Downloads ~12 GB from IBGE, runs the stage-3 panel identification and writes
   `main_data.parquet`. Every step is idempotent: completed quarters and
   rotation groups are skipped, so an interrupted run resumes where it stopped.
   `./status.sh` reports progress.

3. **Run the analysis** (writes every table and figure to `analysis/output/`):

   ```bash
   Rscript "analysis/code/00_master_analysis.R"
   ```

4. **Compile the paper:**

   ```bash
   cd latex && latexmk -pdf paper.tex
   ```

   `paper.tex` reads figures from `../analysis/output/figures` and `\input`s the
   table fragments and `numbers.tex` from `../analysis/output/tables`, so the
   analysis must have been run at least once. `latexmk -C` cleans auxiliary
   files.

## Environment and runtime

Tested with **R 4.4.2** on macOS (arm64) and **TeX Live 2024** with
`elsarticle`. Install the R dependencies with:

```r
install.packages(c("data.table", "arrow", "readstata13", "fixest", "marginaleffects", "fwildclusterboot", "ggplot2", "scales", "haven", "pbapply", "sandwich", "openxlsx"))
```

| Package | Version | Used by |
|---|---|---|
| `data.table` | 1.18.2.1 | build + all analysis |
| `arrow` | 21.0.0 | parquet I/O |
| `fixest` | 0.12.1 | all regressions / event studies |
| `marginaleffects` | 0.24.0 | validation of the closed-form margins (`08`) |
| `fwildclusterboot` | 0.14.3 | wild cluster bootstrap (`03`) |
| `ggplot2` / `scales` | 4.0.2 / 1.4.0 | all figures |
| `readstata13` / `haven` | 0.11.0 / 2.5.5 | legacy vintage only |
| `PNADcIBGE`, `datazoom.social` | — | build only (download + panel identification) |

- **Runtime** on a 16 GB laptop: the build takes several hours (dominated by the
  downloads and the stage-3 fuzzy matching), the analysis two to three
  (fixed-effect regressions on ≈8.0 million person-quarters). Both cache
  aggressively — the build skips completed quarters and groups, the analysis
  caches every fitted model under `analysis/output/estimates` — so a second run
  rebuilds only tables and figures in minutes. Delete the cache directory to
  force full re-estimation. `fixest` uses OpenMP where available; the macOS CRAN
  build used here is single-threaded, which is the main reason for the long
  runtime. Results are unchanged on a multi-threaded build.
- **Random seeds.** A single global seed, `20260615`, set in `_config.R`, governs
  every stochastic routine: the wild cluster bootstrap (9,999 replications), the
  sup-*t* multiplier bootstrap (10,000 draws) and the decomposition cluster
  bootstrap (500 replications). Exact package versions, the RNG kind and the
  seeds of the run that produced the current results are written to
  `analysis/output/logs/session_info.txt`.

## Replication package and archiving

This GitHub repository is the browsable, developing version of the replication
code. On acceptance a **frozen snapshot** — the full code together with the
pre-built `main_data.parquet` — will be deposited on **Zenodo**, which mints a
permanent DOI. That Zenodo record is the citable, archival replication package
referenced in the paper's data-availability statement; this repository mirrors
it for convenience.

## Program-to-output inventory

Exhibit numbers are the numbers as they appear in the paper (main text 1–8 and
Figs. 1–7; appendix A–B).

### Main text

| Exhibit | Script | Output file |
|---|---|---|
| Table 1 — Descriptive statistics | `02_table_descriptive_statistics.R` | `tables/tab_descriptives.tex` |
| Table 2 — Adjusted margins by education × period | `03_main_estimation.R` | `tables/tab_main_margins.tex` |
| Table 3 — Specification ladder | `03_main_estimation.R` | `tables/tab_spec_ladder.tex` |
| Table 4 — Heterogeneity by segment | `05_heterogeneity.R` | `tables/tab_heterogeneity.tex` |
| Table 5 — Composition vs within-cell decomposition | `06_decomposition.R` | `tables/tab_decomposition.tex` |
| Table 6 — Panel retention and balance | `07_attrition.R` | `tables/tab_attrition.tex` |
| Table 7 — Inverse-retention weighting | `07_attrition.R` | `tables/tab_ipw.tex` |
| Table 8 — Robustness | `08_robustness.R` | `tables/tab_robustness.tex` |
| Fig. 1 — Adjusted exit probability by education | `04_figures_main.R` | `figures/fig_levels.pdf` |
| Fig. 2 — College minus non-college gap | `04_figures_main.R` | `figures/fig_gap.pdf` |
| Fig. 3 — Formal and informal employment | `05_heterogeneity.R` | `figures/fig_formal_informal.pdf` |
| Fig. 4 — By labour-market position | `05_heterogeneity.R` | `figures/fig_by_position.pdf` |
| Fig. 5 — Decomposition | `06_decomposition.R` | `figures/fig_decomposition.pdf` |
| Fig. 6 — Within demographic groups | `05_heterogeneity.R` | `figures/fig_by_demographics.pdf` |
| Fig. 7 — Retention into the following quarter | `07_attrition.R` | `figures/fig_retention.pdf` |

### Appendix

| Exhibit | Script | Output file |
|---|---|---|
| Table A.1 — Stage-3 matching performance | `02_table_descriptive_statistics.R` | `tables/tab_matching.tex` |
| Table B.1 — Variance-estimator sensitivity | `03_main_estimation.R` | `tables/tab_vcov_sensitivity.tex` |
| Table B.2 — Sample by quarter | `02_table_descriptive_statistics.R` | `tables/tab_sample_by_quarter.tex` |

### Other output

| Item | Script | Output file |
|---|---|---|
| LaTeX macros for every number quoted in the text | `09_paper_numbers.R` | `tables/numbers.tex` |
| Sample-integrity diagnostics | `01_prepare_analysis_data.R` | `logs/01_prepare_diagnostics.txt` |
| Closed-form margins validation | `08_robustness.R` | `logs/08_margins_validation.txt` |
| Attrition breakdown calculation | `07_attrition.R` | `logs/07_attrition_breakdown.txt` |
| Software environment, seeds, package versions | `99_session_info.R` | `logs/session_info.txt` |
| Variable dictionary (workbook) | `dictionary/build_dictionary.R` | `dictionary/variable_dictionary.xlsx` |

Elsevier requires article highlights as a separate file at submission;
`latex/highlights.txt` holds them (3–5 bullets, ≤85 characters each).

## Citation

> Cavalcanti, F., Didier, F., Gonzaga, G. *Education and Employment Exits During
> COVID-19: Evidence from Brazil.* Working paper.
> Code: <https://github.com/FredieDidier/Monografia>

Underlying microdata: IBGE, *Pesquisa Nacional por Amostra de Domicílios
Contínua*, 2012–2024. Panel links: Data Zoom, PUC-Rio.
