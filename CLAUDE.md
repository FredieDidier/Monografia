# CLAUDE.md — Monografia Project Guide

Working notes for this repository. The user-facing replication instructions live
in `README.md`; everything that is background, provenance, method detail or a
lesson learned lives here.

## What the paper is

*Education and Employment Exits During COVID-19: Evidence from Brazil* —
Cavalcanti, Didier and Gonzaga. **Target journal: Labour Economics** (elsarticle,
`review,3p,authoryear`).

The rotating panel of *PNAD Contínua* is used to track workers employed at
quarter *t* and ask whether they are still employed at *t+1*. The object of
interest is the **education gradient in the exit hazard** and how it moved
through the pandemic, decomposed into a **composition** component (where
graduates and non-graduates work) and a **within-cell** component (the risk they
face inside the same kind of job).

**Headline pattern** (adjusted margins, from `numbers.tex`):

| Period | Non-college | College | Gap (college − non-college) |
|---|---|---|---|
| Pre-pandemic (…–2019Q4) | 10.0% | 9.1% | −0.9 pp |
| Onset (2020Q1) | 13.6% | 10.0% | −3.5 pp |
| Mid-pandemic (2020Q2–2021Q3) | 5.6% | 7.4% | **+1.8 pp** |
| Post-pandemic (2021Q4–) | 9.7% | 8.7% | −1.0 pp |

The story is the **mid-pandemic reversal**: graduates, normally *less* likely to
leave employment, became *more* likely for six quarters. Exit rates fall for
everyone in the mid-pandemic (hiring and separations both collapse); the
gradient is what flips. The reversal appears in 7 quarters pointwise
(2013Q3–2021Q3 range of significant quarters) and 6 under the sup-*t*
simultaneous band, so it is not a multiple-comparisons artefact.

Sample: ≈8.0 million person-quarter origins, 3.0 million individuals, 1.67
million households, 39,270 PSUs, 52 quarters (2012Q1–2024Q4). Raw exit rate
9.4% overall (10.7% non-college, 4.0% college); 20% college, 40% informal;
E→U 2.9 pp and E→N 6.5 pp of the 9.4.

## Stack

| Layer | Tools |
|---|---|
| Download | R (`PNADcIBGE::get_pnadc`; `datazoom.social` for stage-3 panels — [github.com/datazoompuc/datazoom.social](https://github.com/datazoompuc/datazoom.social)) |
| Build & analysis | R (`data.table`, `arrow`, `fixest`, `marginaleffects`, `fwildclusterboot`, `ggplot2`) |
| Writing | LaTeX — `elsarticle`, compiled locally |

The paper is **no longer maintained on Overleaf**: `latex/paper.tex` in this
repository is the source of truth.

## Data

Paths are resolved in `analysis/code/_config.R` from `Sys.info()[["user"]]`:
`ROOT` (this repository) and `DROPBOX` (the data folder). Add a machine to both
`switch()` blocks before its first run. Nothing under `DROPBOX` is ever
committed; `.gitignore` blocks `*.parquet`, `*.dta`, `*.rds`, `*.RData`.

```
<DROPBOX>/build/input/pnadc_panels/Panel_01..13.parquet   stage-3 rotation groups (step 11)
<DROPBOX>/build/output/main_data.parquet                  person-quarter transitions (step 12)
<DROPBOX>/build/output/analysis_sample.parquet            matched origins: estimation sample
<DROPBOX>/build/output/analysis_origins.parquet           all origins, matched or not
<DROPBOX>/build/output/main_data.dta                      legacy vintage source (3.5 GB, kept)
```

- `<DROPBOX>/build/input/pnadc_quarters/` is a transient download cache (~0.6 GB)
  that only feeds step 11. It has been deleted; re-running step 10 recreates it
  if the panels ever need rebuilding. Because step 12 reads the **panels**, not
  the quarter cache, the pipeline runs end to end without it.
- **Legacy vintage.** `DATA_VINTAGE`/`MONOGRAFIA_VINTAGE=legacy` makes `_config.R`
  point at `analysis_sample_legacy.parquet` and skip step 01. That file is
  **not currently in the Dropbox folder** — only its source `main_data.dta` is —
  so the legacy path needs the file regenerated before it will run.
- If Dropbox is online-only, materialise a file before reading:
  `cat main_data.parquet > /dev/null`.
- `analysis/input/` holds **only** the committed matching CSVs. No parquet
  belongs there: `ANALYSIS_PQ` and `ORIGINS_PQ` always resolve under `DIR_DATA`
  (Dropbox), including in `dictionary/build_dictionary.R`.
  
### Panel identification (stage 3)

`build_pnadc_panel(panel = "advanced_3")`, the datazoom.social **stage-3**
identification, runs three passes:

1. link on household × sex × full date of birth (the classical rule, Ribas and
   Soares 2008);
2. **donate birth dates** across a respondent's interviews, so a missing or
   mistyped date in one quarter no longer breaks the chain;
3. resolve fragmented sequences with a **graph-theoretic fuzzy match**, taking
   connected components over candidate links.

Pass 3 is what recovers respondents whose recorded birth date drifts between
interviews. Matched rows carry `id_rs3`; unmatched rows carry `NA` and the build
**keeps them** — that is what makes retention directly observable. Overall match
rate 86.8% over 11.5 million origins.

### What the stage-3 rebuild recovered

The previous vintage was assembled from a pre-built file that had already
discarded three things. Each recovery answers a specific referee point, and all
three are now in place — do not reintroduce the old workarounds.

| Recovered | Why it matters |
|---|---|
| **Destination state at *t+1*** | The old build collapsed unemployment and non-participation into one "non-employed" category. E→U and E→N are now separate outcomes with their own panels. |
| **Unmatched origins** | The old build kept matched pairs only, so the *t*→*t+1* retention rate was not computable and `07_attrition.R` had to use a one-step-removed proxy. It is now **directly observed** via `matched_next`. |
| **`UPA`, `Estrato`, `V1016`** | Sampling unit and stratum are ordinary columns instead of substrings of an ID, and PNADC's own interview counter identifies scheduled panel exits exactly (a worker in interview 5 *cannot* be matched forward — that is not attrition). |

### The two analysis files

`01_prepare_analysis_data.R` derives the analysis variables from
`main_data.parquet` and splits it in two. Splitting here rather than filtering in
each script keeps the estimation sample unambiguous: anything read from
`analysis_sample.parquet` has an observed outcome.

| File | Contents | Used by |
|---|---|---|
| `analysis_sample.parquet` | matched origins only | estimation (02–06, 08, 09) |
| `analysis_origins.parquet` | every origin, matched or not | attrition (07), part of 09 |

Sanity checks go to `analysis/output/logs/01_prepare_diagnostics.txt`.

## Key variables

Constructed in `01_prepare_analysis_data.R`.

| Variable | Definition | Source |
|---|---|---|
| `exit` | 1 if employed at *t* and not employed at *t+1* | `VD4001`/`VD4002` at *t+1* |
| `exit_to_unemployment` | employed at *t*, unemployed at *t+1* | `VD4001==1 & VD4002==2` |
| `exit_to_nonpart` | employed at *t*, out of the labour force at *t+1* | `VD4001==2` |
| `exit_to_informal` | informally employed at *t+1* (destination state, not a transition) | position at *t+1* |
| `matched_next` | 1 if the stage-3 algorithm links the worker into *t+1* | `id_rs3` |
| `interview` | Interview number within the household's rotation, 1–5 | `V1016` |
| `college` | 1 if completed tertiary education | `VD3004 == 7` |
| `formal` | 1 if formally employed (PNAD employment category) | `position` 3, 5, 7, 9 |
| `position_grp` | Formal/informal × private employee, self-employed, employer, public | `position` 3–10 |
| `female` / `black_brown` / `white` / `race5` / `nonwhite` | Demographics | `V2007`, `V2010` |
| `urban` | 1 if urban household | `V1022 == 1` |
| `age`, `age_sq` | Age in years and its square | `V2009` |
| `hours` | Usual weekly hours in the main job | `V4039` |
| `income`, `log_income` | Monthly labour income and `log(1 + income)` | `VD4017` |
| `temporary`, `social_security`, `signed_card` | Job-contract flags | `V4025`, `V4032`, `V4029` |
| `tenure` | <1m, 1–11m, 1–2y, 2y+ | `V4040` |
| `occupation` | Ten COD major groups + "not reported" | `V4010` |
| `sector` | Agriculture, industry, construction, trade, services + "not reported" | `VD4010` |
| `state` | Federation unit | `UF` |
| `w` | Person survey weight | `V1028` |
| `psu`, `strata` | Primary sampling unit and stratum | `UPA`, `Estrato` |
| `household`, `pid`, `panel_grp` | Household, individual, rotation-group IDs | `id_dom`, `id_rs3`, `V1014` |

Missing occupation and sector are kept as an explicit **"not reported"**
category rather than dropped, so no observations are lost when they enter as
fixed effects. Occupation comes from IBGE's [Classificação de Ocupações para
Pesquisas Domiciliares
(COD)](https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/metodologia/anexos/anexo_7_ocupacao_cod.pdf)
in `V4010`; its **leading digit is the major group**, so the ten groups follow
directly — the previous build enumerated every four-digit code by hand into
those same ten groups.

`dictionary/build_dictionary.R` regenerates `variable_dictionary.xlsx` by
reading row counts and column types **from the parquet files themselves**, so
the dictionary cannot drift from the data. Run it after the build and after 01.

## The build, step by step

```bash
Rscript "build/code/00_master_build.R"
```

| Step | What it does | Notes |
|---|---|---|
| `10_download_pnadc_quarters.R` | `get_pnadc()` per quarter → `treat_pnadc()` → prune to ~69 columns → parquet | ~12 GB downloaded, ~1–2 min/quarter; output is a transient cache |
| `11_build_panels.R` | stage-3 identification, one rotation group at a time | identifiers only, merged back on a row key |
| `12_build_main_data.R` | *t* → *t+1* transitions, destination state, matching diagnostics | one panel at a time; reads the panels, not the quarter cache |

Every step is **idempotent**: completed quarters and groups are skipped, so an
interrupted run resumes where it stopped. A transient download failure is
retried up to five times with a growing pause; a quarter IBGE has not published
yet is recognised and not retried. `./status.sh` prints progress of a full
rebuild (downloads / panels / main_data / analysis / exhibits).

**Two memory lessons, both learned the hard way and both worth keeping:**

- `get_pnadc()`'s `vars` argument only ever **adds** columns; it cannot restrict
  the download. Pruning has to happen right after each quarter is cleaned.
- `load_pnadc()` downloads a whole multi-year window, binds it, and only then
  splits by rotation group — which neither fits in 16 GB nor avoids downloading
  most quarters three times. Caching each quarter once, then feeding **one group
  at a time** to `build_pnadc_panel()`, gives identical identification (the
  algorithm only ever compares rows within the data it is given) at a fraction of
  the memory.

The earlier Stata + R build is kept under `build/code/legacy/` for provenance
only; it is the source of the `main_data.dta` vintage and should not be run.

## Method notes

**Adjusted margins.** Coefficients on education indicators in a model with
continuous covariates and fixed effects are *not* adjusted group means. The
paper reports survey-weighted average predictive margins by education × quarter.
Because education enters only through its own indicator and its quarter
interactions, and quarter is among the absorbed fixed effects, these have the
closed form

```
Delta_q = delta + beta_q                    (the gap, exact)
m_gq    = ybar_q + (g - p_q) * Delta_q      (the levels)
```

where `ybar_q` is the weighted exit rate and `p_q` the weighted college share in
quarter *q*. `08_robustness.R` verifies this against
`marginaleffects::avg_predictions()` on a random subsample; the two agree to
about 3e-15. **Do not replace the closed form with `marginaleffects` in the main
path** — it is the reason the pipeline finishes at all on this sample size.

**Reference quarter.** `REF_QUARTER = 2019Q4` — the last quarter whose *t+1*
outcome (2020Q1) is still measured before the shock, so every reported contrast
is a deviation from the immediate pre-pandemic gap. Periods: onset `Q_ONSET =
2020Q1`; mid-pandemic `Q_MID = 2020Q2–2021Q3`; post-pandemic 2021Q4 onwards.

**Inference.** Default variance: **two-way clustered by PSU and year-quarter**.
`tab_vcov_sensitivity` reports clustering by PSU, household, individual,
individual × year-quarter, and year-quarter alone. Because 51 quarterly
contrasts are reported, **sup-*t* simultaneous bands** are computed by
multiplier bootstrap (10,000 draws, critical value ≈2.71) alongside the
pointwise intervals. **Wild cluster bootstrap** *p*-values clustered by
year-quarter are reported for the two pandemic contrasts under the preferred
specification; at 9,999 replications on ~8 million rows each takes roughly 40
minutes, which is why they are cached in `estimates/wcb_pvalues.rds`.

Two-way clustering is not guaranteed positive semi-definite. `fixest` applies
the Cameron–Gelbach–Miller eigenvalue correction and emits a note when it does;
linear combinations are additionally floored at zero variance in the reporting
helpers. The single-way alternatives in `tab_vcov_sensitivity` are free of this
issue and give the same conclusions.

**Decomposition.** The raw gap is split into composition and within-cell
components over cells defined by **formality × sector × occupation** (and, as a
robustness check, additionally by labour-market position). Standard errors come
from a cluster bootstrap drawing exponential multipliers at the **PSU ×
quarter** level (500 replications).

**Attrition.** Retention is **directly observed** — `matched_next` is one
exactly when the algorithm links the worker into *t+1*, which is precisely the
selection that generates the estimation sample. `07_attrition.R` reads
`analysis_origins.parquet` and produces (a) retention by quarter, education and
formality, (b) standardised differences matched vs unmatched, (c)
inverse-retention-probability weights and re-estimation, (d) a breakdown
calculation: how differently would unmatched workers have to behave to overturn
the mid-pandemic reversal? Interview 5 is a **scheduled panel exit**, not
attrition, and is identified exactly by `V1016`.

**Seeds.** One global seed, `SEED = 20260615` in `_config.R`. From version 0.13
`fwildclusterboot::boottest()` no longer takes a `seed` argument and draws from
the ordinary R streams, so `wcb_pvalue()` sets **both** `set.seed()` and
`dqrng::dqset.seed()` before each call.

## Matching diagnostics

Computed by `12_build_main_data.R` over the full panel population and
**committed** under `analysis/input/matching/`, so `tab_matching` builds without
the micro-data:

| File | Contents |
|---|---|
| `stage3_matching.csv` | share of individuals and households linked across at least *k* interviews |
| `stage3_match_by_interview.csv` | share of employed workers matched into *t+1*, by interview |

`household_transition.csv`, `individual_transition.csv`, `transition_summary.csv`
and `matching_retention.csv` are the same diagnostics for the **previous**
algorithm, kept so the two vintages can be compared.

## Conventions

- All code and comments in **English**; `data.table` throughout.
- Repository paths from `ROOT`, data paths from `DROPBOX` — never hard-code a
  path inside a script; add the machine to `_config.R` instead.
- **Survey weights (`V1028` → `w`) in every specification.** PNADC is not
  self-weighting; there is no unweighted headline table (an unweighted variant
  exists only as a robustness row).
- Outputs go to `analysis/output/{tables,figures,logs}` and are **committed**;
  `estimates/` is a git-ignored model cache. Tables are `.tex` fragments for
  `\input{}`.
- **No number is typed into the LaTeX by hand.** Anything quoted in the text
  comes from `09_paper_numbers.R` as a macro in `numbers.tex`.
- Figures: colour-blind-safe palette that also reads in grayscale
  (`COL_NOCOLLEGE` blue, `COL_COLLEGE` red) — Elsevier prints black and white
  unless colour is paid for. Shared theme in `_figures.R` / `theme_paper()`;
  legends sit under the plot and are set larger than the axis text because the
  figures are reduced substantially in print.
- Every fitted model is cached under `analysis/output/estimates`; delete the
  directory to force full re-estimation. Step 01 is skipped when
  `analysis_sample.parquet` is **newer** than `main_data.parquet` — checking mere
  existence once meant a rebuilt `main_data.parquet` was silently ignored and the
  whole analysis ran on a stale sample.
- `legacy/` directories (under `build/code` and `analysis/code`) are provenance,
  not live code.

## Status

- Build, analysis and manuscript all run end to end on the stage-3 vintage; the
  three referee-flagged gaps (E→U vs E→N, unmatched origins, `UPA`/`Estrato`/
  `V1016`) are closed.
- Open: Zenodo deposit on acceptance (README promises it).