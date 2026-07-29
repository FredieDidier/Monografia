# Education and Employment Exits During COVID-19: Evidence from Brazil

Replication package for Cavalcanti, Didier and Gonzaga, *Education and Employment
Exits During COVID-19: Evidence from Brazil*, prepared for submission to
**Labour Economics**.

The paper uses the rotating panel of Brazil's *PNAD Contínua* to measure how the
education gradient in the risk of leaving employment moved through the COVID-19
pandemic, and decomposes that gradient into a composition component (where
graduates and non-graduates work) and a within-cell component (the risk they
face inside the same kind of job).

Everything in the manuscript — every table, every figure and every number quoted
in the text — is produced by the R pipeline in `analysis/code`. Nothing numeric
is typed into the LaTeX by hand.

---

## 1. Quick start

Build the micro-data from IBGE (once, several hours):

```bash
Rscript "build/code/00_master_build.R"
```

Run the analysis:

```bash
Rscript "analysis/code/00_master_analysis.R"
```

Compile the paper:

```bash
cd latex && latexmk -pdf paper.tex
```

The build downloads every PNAD Contínua quarter, runs the datazoom.social
stage-3 panel identification and writes `main_data.parquet` to Dropbox. The
analysis writes into `analysis/output/{tables,figures,estimates,logs}`, and the
paper reads from there.

Timings on a 16 GB laptop: the build takes several hours (dominated by the
downloads and the stage-3 fuzzy matching), the analysis two to three
(fixed-effect regressions on millions of person-quarters). Both cache
aggressively — the build skips completed quarters and groups, the analysis
caches every fitted model under `analysis/output/estimates` — so a second run
rebuilds only tables and figures in minutes. Delete the cache directory to force
full re-estimation.

---

## 2. Repository layout

```
Monografia/
├── README.md
├── build/code/                  Micro-data construction, see §4
│   ├── 00_master_build.R            Runs 10-12 in order
│   ├── 10_download_pnadc_quarters.R One cached parquet per PNADC quarter
│   ├── 11_build_panels.R            Stage-3 identification per rotation group
│   ├── 12_build_main_data.R         t -> t+1 transitions + matching diagnostics
│   └── legacy/                      Superseded Stata + R build, kept for provenance
├── analysis/
│   ├── code/
│   │   ├── 00_master_analysis.R     Runs everything below, in order
│   │   ├── _config.R                Paths, seeds, plotting theme, parameters
│   │   ├── _functions.R             Estimation, margins, decomposition, inference
│   │   ├── 01_prepare_analysis_data.R
│   │   ├── 02_table_descriptive_statistics.R
│   │   ├── 03_main_estimation.R
│   │   ├── 04_figures_main.R
│   │   ├── 05_heterogeneity.R
│   │   ├── 06_decomposition.R
│   │   ├── 07_attrition.R
│   │   ├── 08_robustness.R
│   │   ├── 09_paper_numbers.R       LaTeX macros for every number in the text
│   │   ├── 99_session_info.R
│   │   ├── _figures.R               Shared plotting theme and helpers
│   │   └── legacy/                 Superseded scripts, kept for provenance
│   ├── input/matching/              Matching diagnostics (committed CSVs)
│   └── output/
│       ├── tables/                  .tex fragments \input by the paper
│       ├── figures/                 .pdf (for LaTeX) and .png
│       ├── estimates/               Cached models and estimate CSVs (git-ignored)
│       └── logs/                    Diagnostics, session info, seeds
└── latex/
    ├── paper.tex                    elsarticle manuscript
    ├── refs.bib
    └── highlights.txt               Elsevier highlights, submitted separately
```

---

## 3. Data

### 3.1 Source and panel identification

Microdata come from *IBGE*'s **Pesquisa Nacional por Amostra de Domicílios
Contínua** (*PNAD Contínua*), quarterly, 2012Q1 onwards. The survey is public
and distributed by IBGE; nothing here is restricted.

Respondents are linked across interviews with **datazoom.social**
(`build_pnadc_panel(panel = "advanced_3")`), the PUC-Rio package's **stage 3**
identification. It runs three passes:

1. link on household × sex × full date of birth (the classical rule, following
   Ribas and Soares 2008);
2. **donate birth dates** across a respondent's interviews, so a missing or
   mistyped date in one quarter no longer breaks the chain;
3. resolve fragmented sequences with a **graph-theoretic fuzzy match**, taking
   connected components over candidate links.

Pass 3 is what recovers respondents whose recorded birth date drifts between
interviews. Matched rows carry `id_rs3`; unmatched rows carry `NA`, and the
build keeps them (see §3.4).

### 3.2 Where the data live

Microdata are **not** in this repository. They sit in a shared Dropbox folder:

```
Dropbox/Education_and_Job_Loss_During_COVID_19/
└── build/
    ├── input/
    │   └── pnadc_panels/Panel_NN.parquet     Stage-3 rotation groups (step 11)
    └── output/
        ├── main_data.parquet                 Person-quarter transitions (step 12)
        ├── analysis_sample.parquet           Matched origins: estimation sample
        ├── analysis_origins.parquet          All origins, for the attrition model
        ├── analysis_sample_legacy.parquet    Previous vintage, for comparison
        └── main_data.dta                     Previous vintage source (kept)
```

Step 10 also writes a per-quarter download cache under `input/pnadc_quarters/`.
That cache exists only to feed step 11 and is safe to delete once the thirteen
`Panel_NN.parquet` files are in place — it is roughly 0.6 GB and nothing
downstream reads it. It has been removed from this project. Re-running step 10
recreates it if the panels ever need rebuilding.

Nothing in this list is committed to git. `main_data.dta` and
`analysis_sample_legacy.parquet` are the earlier vintage and are retained
deliberately: `_config.R` can be pointed at them with
`MONOGRAFIA_VINTAGE=legacy` to reproduce the results circulated before the
stage-3 rebuild.

`analysis/code/_config.R` resolves both `ROOT` (this repository) and `DROPBOX`
from the current username. **Add your machine there before the first run.** If
your username is not listed, `ROOT` falls back to the working directory and
`DROPBOX` to `build/output` inside the repository.

If Dropbox is set to *online-only*, a file has to be materialised before R can
read it, e.g. `cat main_data.parquet > /dev/null`.

### 3.3 The analysis file

`main_data.parquet` has one row per worker **employed at the interview date in
quarter t** — matched into *t+1* or not. Each row carries the destination state
at *t+1* (employed formal, employed informal, unemployed, out of the labour
force) and a `matched_next` flag.

`analysis/code/01_prepare_analysis_data.R` derives the analysis variables and
splits it in two:

| File | Contents | Used by |
|---|---|---|
| `analysis_sample.parquet` | matched origins only | estimation (02–06, 08) |
| `analysis_origins.parquet` | every origin, matched or not | attrition (07) |

Splitting here rather than filtering in each script keeps the estimation sample
unambiguous: anything read from `analysis_sample.parquet` has an observed
outcome. Sanity checks are logged to
`analysis/output/logs/01_prepare_diagnostics.txt`.

### 3.4 What the rebuild recovers

The previous vintage was assembled from a pre-built file that had already
discarded three things. Rebuilding from the published microdata restores all
three, and each answers a specific point in the referee report:

| Recovered | Why it matters |
|---|---|
| **Destination state at *t+1*** | The old build collapsed unemployment and non-participation into one "non-employed" category. E→U and E→N are now separate outcomes with their own panels in Table 1. |
| **Unmatched origins** | The old build kept matched pairs only, so the *t*→*t+1* retention rate was not computable and `07_attrition.R` had to use a one-step-removed proxy. It is now directly observed. |
| **`UPA`, `Estrato`, `V1016`** | Sampling unit and stratum are ordinary columns instead of substrings of an ID, and the survey's own interview counter identifies scheduled panel exits exactly. |

### 3.5 Variable dictionary

Constructed in `01_prepare_analysis_data.R`.

| Variable | Definition | Source |
|---|---|---|
| `exit` | 1 if employed at *t* and not employed at *t+1* | `VD4001`/`VD4002` at *t+1* |
| `exit_to_unemployment` | 1 if employed at *t* and unemployed at *t+1* | `VD4001==1 & VD4002==2` |
| `exit_to_nonpart` | 1 if employed at *t* and out of the labour force at *t+1* | `VD4001==2` |
| `exit_to_informal` | 1 if informally employed at *t+1* (destination state, not a transition) | position at *t+1* |
| `matched_next` | 1 if the stage-3 algorithm links the worker into *t+1* | `id_rs3` |
| `interview` | Interview number within the household's rotation, 1–5 | `V1016` |
| `college` | 1 if completed tertiary education | `VD3004 == 7` |
| `formal` | 1 if formally employed, PNAD classification by employment category | `position` 3, 5, 7, 9 |
| `position_grp` | Formal/informal × private employee, self-employed, employer, public sector | `position` 3–10 |
| `female` | 1 if woman | `V2007 == 2` |
| `black_brown` | 1 if *preta* or *parda* | `V2010 ∈ {2, 4}` |
| `white` | 1 if *branca* | `V2010 == 1` |
| `race5` | White / Black / Asian / Brown / Indigenous / Not reported | `V2010` |
| `urban` | 1 if urban household | `V1022 == 1` |
| `age`, `age_sq` | Age in years and its square | `V2009` |
| `hours` | Usual weekly hours in the main job | `V4039` |
| `income`, `log_income` | Monthly labour income and `log(1 + income)` | `VD4017` |
| `temporary` | 1 if temporary job | `V4025` |
| `social_security` | 1 if contributes to social security | `V4032` |
| `signed_card` | 1 if signed work card | `V4029` |
| `tenure` | Time in current job: <1m, 1–11m, 1–2y, 2y+ | `V4040` |
| `occupation` | Ten COD major groups + "not reported" | `V4010` |
| `sector` | Agriculture, industry, construction, trade, services + "not reported" | `VD4010` |
| `state` | Federation unit | `UF` |
| `w` | Person survey weight | `V1028` |
| `psu`, `strata` | Primary sampling unit and stratum | `UPA`, `Estrato` |
| `household`, `pid`, `panel_grp` | Household, individual and rotation-group identifiers | `id_dom`, `id_rs3`, `V1014` |
| `nonwhite` | 1 if the respondent does not report *branca* | `V2010 != 1` |

Missing occupation and sector are kept as an explicit "not reported" category
rather than dropped, so that no observations are lost when they enter as fixed
effects. Occupation comes from IBGE's [Classificação de Ocupações para Pesquisas
Domiciliares (COD)](https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/metodologia/anexos/anexo_7_ocupacao_cod.pdf),
recorded in `V4010`. Its leading digit is the major group, so the ten groups
follow directly — the previous build enumerated every four-digit code by hand
into those same ten groups.

---

## 4. The build, step by step

```bash
Rscript "build/code/00_master_build.R"
```

| Step | What it does | Notes |
|---|---|---|
| `10_download_pnadc_quarters.R` | `get_pnadc()` per quarter → `treat_pnadc()` → prune to ~69 columns → parquet | ~12 GB downloaded, ~1–2 min per quarter; output is a transient cache (§3.2) |
| `11_build_panels.R` | stage-3 identification, one rotation group at a time | identifiers only, merged back on a row key |
| `12_build_main_data.R` | *t* → *t+1* transitions, destination state, matching diagnostics | one panel at a time; reads the panels, not the quarter cache |

Because step 12 reads only the panels, the pipeline can be re-run end to end
after the quarter cache has been deleted, as long as `input/pnadc_panels/`
survives. Deleting the panels too means starting from the downloads.

Every step is **idempotent**: completed quarters and groups are skipped, so an
interrupted run resumes where it stopped. A quarter whose download fails
transiently is retried up to five times with a growing pause; a quarter IBGE has
not published yet is recognised and not retried.

Two memory notes, both learned the hard way and both worth keeping:

- `get_pnadc()`'s `vars` argument only ever **adds** columns; it cannot restrict
  the download. Pruning has to happen right after each quarter is cleaned.
- `load_pnadc()` downloads a whole multi-year window, binds it, and only then
  splits by rotation group — which neither fits in 16 GB nor avoids downloading
  most quarters three times. Caching each quarter once, then feeding one group
  at a time to `build_pnadc_panel()`, gives identical identification (the
  algorithm only ever compares rows within the data it is given) at a fraction
  of the memory.

The earlier Stata + R build is kept under `build/code/legacy/` for provenance.

**Known limitations of the current build.** Three things the referee report asks
for are not recoverable from `main_data.dta` as it stands, and all three are
fixed by the same rebuild:

1. **E→U versus E→N.** The build collapses the destination state at *t+1* into a
   single `"Non-Employed"` category, so exits into unemployment cannot be
   separated from exits into non-participation. The distinction is economically
   important and is flagged as a limitation in the paper. Fixing it means
   carrying `position` (not `position_names`) into the lead in
   `05_main_data.R`.
2. **Unmatched origins.** Only matched pairs survive the build, so the direct
   *t*→*t+1* retention rate cannot be computed; the paper uses an observable
   one-quarter-ahead retention proxy instead (§5).
3. **`UPA` as a column.** `01_cleaning_paineis.R` renames `UPA` and then drops
   it in the `select()`. The analysis recovers it from `idind` (§3.4) instead,
   which is exact, but keeping the column would be cleaner.

The rebuild also requires the quarter-pair panels for 2022Q4–2024Q3, which are
not currently in the Dropbox folder (only 2012Q1–2022Q3 are).

---

## 5. What each analysis script does

| Script | Output |
|---|---|
| `01_prepare_analysis_data.R` | `analysis_sample.parquet` and `analysis_origins.parquet`; integrity checks on quarter coverage, match rates and missing values |
| `02_table_descriptive_statistics.R` | Tables `tab_descriptives`, `tab_sample_by_quarter`, `tab_matching` |
| `03_main_estimation.R` | Event study for each destination (any exit, E→U, E→N), adjusted margins, `tab_main_margins`, `tab_spec_ladder`, `tab_vcov_sensitivity` |
| `04_figures_main.R` | `fig_levels`, `fig_gap` |
| `05_heterogeneity.R` | `fig_formal_informal`, `fig_by_demographics`, `fig_by_position`, `tab_heterogeneity` |
| `06_decomposition.R` | `fig_decomposition`, `tab_decomposition` |
| `07_attrition.R` | `fig_retention`, `tab_attrition`, `tab_ipw`, breakdown calculation |
| `08_robustness.R` | `tab_robustness`; validation of the closed-form margins against `marginaleffects` |
| `09_paper_numbers.R` | `numbers.tex`, the LaTeX macros used in the manuscript text |
| `99_session_info.R` | `session_info.txt` with package versions, RNG kind and seeds |

### Method notes

**Adjusted margins.** Coefficients on education indicators in a model with
continuous covariates and fixed effects are not adjusted group means. The paper
reports survey-weighted average predictive margins by education × quarter.
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
about 3e-15.

**Inference.** Default variance: two-way clustered by primary sampling unit and
year-quarter. `tab_vcov_sensitivity` reports clustering by PSU, household,
individual, individual × year-quarter and year-quarter alone. Because 51
quarterly contrasts are reported, sup-*t* simultaneous bands are computed by
multiplier bootstrap alongside the pointwise intervals. Wild cluster bootstrap
*p*-values clustered by year-quarter are reported for the two pandemic contrasts
under the preferred specification; at 9,999 replications on 7.9 million rows
each takes roughly 40 minutes, which is why they are cached in
`estimates/wcb_pvalues.rds`.

Two-way clustering is not guaranteed positive semi-definite. `fixest` applies
the Cameron–Gelbach–Miller eigenvalue correction and emits a note when it does;
linear combinations are additionally floored at zero variance in the reporting
helpers. The single-way alternatives in `tab_vcov_sensitivity` are free of this
issue and give the same conclusions.

**Decomposition.** The raw gap is split into composition and within-cell
components over cells defined by formality × sector × occupation (and, as a
robustness check, additionally by labour market position). Standard errors come
from a cluster bootstrap drawing exponential multipliers at the PSU × quarter
level.

**Attrition.** The analysis file keeps matched pairs only, so the *t*→*t+1*
match rate is not computable from it. What is observable is whether a worker who
is an origin in quarter *t* is also an origin in *t+1*; restricting to the first
three interviews of each rotation panel makes a further match mechanically
possible. That indicator drives the retention table, the inverse-retention
weights and the re-estimation.

---

## 6. Matching diagnostics

Matching performance is computed by `12_build_main_data.R` over the full panel
population and committed under `analysis/input/matching/`:

| File | Contents |
|---|---|
| `stage3_matching.csv` | share of individuals and households linked across at least *k* interviews |
| `stage3_match_by_interview.csv` | share of employed workers matched into *t+1*, by interview |

Because they are committed, `tab_matching` builds without the micro-data. The
`*_transition.csv` and `matching_retention.csv` files in the same directory are
the corresponding diagnostics for the **previous** algorithm, kept so the two
vintages can be compared.

---

## 7. Software

Developed and run under:

- R 4.4.2 on macOS (Darwin 25.5.0), x86_64
- `data.table`, `arrow`, `readstata13`, `fixest`, `marginaleffects`,
  `fwildclusterboot`, `ggplot2`, `scales`, `haven`, `pbapply`
- TeX Live 2024 with `elsarticle`

Install the R dependencies with:

```r
install.packages(c("data.table", "arrow", "readstata13", "fixest", "marginaleffects", "fwildclusterboot", "ggplot2", "scales", "haven", "pbapply"))
```

Exact versions of the run that produced the current results are written to
`analysis/output/logs/session_info.txt`.

`fixest` uses OpenMP when available. The macOS CRAN build used here is
single-threaded, which is the main reason for the long runtime; on a
multi-threaded build the pipeline is considerably faster and results are
unchanged.

### Seeds

A single global seed, `20260615`, set in `_config.R`, governs every stochastic
routine:

| Routine | Replications |
|---|---|
| Wild cluster bootstrap (`fwildclusterboot`) | 9,999 |
| sup-*t* multiplier bootstrap | 10,000 |
| Decomposition cluster bootstrap | 500 |

From version 0.13, `fwildclusterboot::boottest()` no longer takes a `seed`
argument and draws from the ordinary R streams, so `wcb_pvalue()` sets both
`set.seed()` and `dqrng::dqset.seed()` before each call.

---

## 8. Building the manuscript

The paper is a single `elsarticle` document. It is no longer maintained on
Overleaf: `latex/paper.tex` in this repository is the source of truth, and the
PDF is produced locally.

```bash
cd latex && latexmk -pdf paper.tex
```

`paper.tex` reads figures from `../analysis/output/figures` and `\input`s table
fragments and `numbers.tex` from `../analysis/output/tables`, so the analysis
must have been run at least once. To clean auxiliary files: `latexmk -C`.

Elsevier requires article highlights as a separate file at submission;
`latex/highlights.txt` holds them (3–5 bullets, ≤85 characters each).

---

## 9. Citation

> Cavalcanti, F., Didier, F., Gonzaga, G. *Education and Employment Exits During
> COVID-19: Evidence from Brazil.* Working paper.
> Code: <https://github.com/FredieDidier/Monografia>

Underlying microdata: IBGE, *Pesquisa Nacional por Amostra de Domicílios
Contínua*, 2012–2024. Panel links: Data Zoom, PUC-Rio.
