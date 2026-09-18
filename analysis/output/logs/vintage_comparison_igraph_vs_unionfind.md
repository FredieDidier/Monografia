# Vintage comparison: datazoom.social igraph (a031bea, built 2026-07-29) vs union-find (3cf4aa6, built 2026-09-17)

Both vintages: same 58 cached quarters 2012Q1–2026Q2 (the July build already had 2026Q1; September adds 2026Q2), same build scripts. The only change is the package: PR #99 replaced `igraph::components()` in stage 3 by a capacity-constrained union-find and fixed `V2008 == NA` (always NA) in the basic identification. The igraph panels and outputs were archived, compared, and deleted on 2026-09-18.

## Stage-3 coverage by rotation group (share of person-quarters with id_rs3, %)

| Group | Window (igraph) | igraph | union-find | Δ |
|---|---|---|---|---|
| 1 | 20121–20124 | 97.9 | 97.8 | -0.1 |
| 2 | 20121–20141 | 99.1 | 97.9 | -1.2 |
| 3 | 20132–20152 | 98.7 | 97.0 | -1.7 |
| 4 | 20143–20163 | 97.9 | 95.3 | -2.6 |
| 5 | 20154–20174 | 95.2 | 90.7 | -4.5 |
| 6 | 20171–20191 | 95.5 | 91.5 | -4.0 |
| 7 | 20182–20202 | 96.7 | 93.9 | -2.8 |
| 8 | 20193–20213 | 97.0 | 94.3 | -2.7 |
| 9 | 20204–20224 | 94.3 | 89.6 | -4.7 |
| 10 | 20221–20241 | 95.3 | 90.8 | -4.5 |
| 11 | 20232–20252 | 97.4 | 94.5 | -2.9 |
| 12 | 20243–20261 | 97.9 | 95.6 | -2.3 |
| 13 | 20254–20261 | 94.4 | 93.8 | -0.6 |

Group 3 dissected (2,866,552 rows in both):

| | igraph | union-find |
|---|---|---|
| `id_ind` (basic) non-NA | 97.33% | 94.59% |
| `id_rs1` non-NA | 98.12% | 96.34% |
| `id_rs2` = `id_rs3` non-NA | 98.69% | 96.95% |
| ids with two rows in one quarter | 179 | 0 |
| stage-2 ids merged by stage 3 | 13,008 (1.66%) | 14,243 (1.88%) |
| rows re-assigned by stage 3 | 2.00% | 2.18% |
| mean quarters per id, rs2 → rs3 | 3.62 → 3.68 | 3.67 → 3.74 |
| ids with 5 quarters | 50.1% | 51.6% |

Rows with a missing date of birth (5–10% of group 4, 10–12% of group 5): id_rs3 coverage among them fell from 67% → 26% (group 4) and 57% → 16% (group 5). That is the whole coverage loss; the union-find is neutral on coverage.

## Step 12 / step 01 diagnostics

| | igraph | union-find |
|---|---|---|
| Origins | 11,491,554 | 11,491,554 |
| Matched to t+1 (incl. interview 5) | 0.6963 | 0.6757 |
| Match rate, interviews 1–4 | 85.8 / 86.8 / 87.2 / 87.4 | 83.2 / 84.2 / 84.6 / 84.9 |
| Estimation sample | 8,002,128 | 7,764,610 |
| Individuals / households / PSUs | 3,024,050 / 1,667,341 / 39,270 | 2,922,141 / 1,620,032 / 39,251 |
| Weighted exit rate (E→U, E→N) | 0.0938 (0.0290, 0.0648) | 0.0937 (0.0289, 0.0647) |
| College share / informal share | 0.1970 / 0.3972 | 0.1985 / 0.3970 |
| Mid-pandemic retention, college / non-college | 0.876 / 0.841 | 0.855 / 0.807 |

## Paper macros that changed (numbers.tex)

57 of 162 macros changed.

| Macro | igraph | union-find |
|---|---|---|
| `Nobs` | 8,002,128 | 7,764,610 |
| `Nind` | 3,024,050 | 2,922,141 |
| `Npsu` | 39,270 | 39,251 |
| `Nhh` | 1,667,341 | 1,620,032 |
| `MatchRateAll` | 86.8 | 84.2 |
| `OnsetGap` | -0.034 | -0.035 |
| `OnsetGapPP` | -3.4 | -3.5 |
| `OnsetLevNoCol` | 0.135 | 0.136 |
| `MidGap` | 0.019 | 0.018 |
| `MidGapPP` | 1.9 | 1.8 |
| `PostGapLo` | -0.012 | -0.011 |
| `PostGapHi` | -0.007 | -0.006 |
| `PostLevNoCol` | 0.097 | 0.096 |
| `PostLevCol` | 0.088 | 0.087 |
| `MidGapAbsPP` | 1.9 | 1.8 |
| `OnsetRelNoCol` | 36 | 37 |
| `SuptCrit` | 2.72 | 2.73 |
| `GapOnsetQ` | -0.034 | -0.035 |
| `GapMaxRevQ` | 2020Q3 | 2021Q2 |
| `InformalMidGap` | 0.021 | 0.020 |
| `MenPreGap` | -0.004 | -0.003 |
| `WomenMidGap` | 0.029 | 0.028 |
| `WomenPreGap` | -0.010 | -0.009 |
| `NonWhiteMidGap` | 0.023 | 0.022 |
| `InfPrivMidGap` | 0.031 | 0.029 |
| `InfPrivPreGap` | -0.013 | -0.012 |
| `FormPrivMidGap` | 0.000 | 0.001 |
| `FormSelfMidGap` | 0.008 | 0.007 |
| `DecOnsetComp` | -0.042 | -0.045 |
| `DecOnsetWithin` | -0.048 | -0.046 |
| `DecOnsetCompShare` | 46 | 49 |
| `DecOnsetWithinShare` | 54 | 51 |
| `DecPostTotal` | -0.069 | -0.068 |
| `DecPostCompShare` | 50 | 51 |
| `DecPostWithinShare` | 50 | 49 |
| `DecCompShrink` | 45 | 44 |
| `RetOverall` | 0.873 | 0.851 |
| `RetDiffPre` | 0.014 | 0.021 |
| `RetDiffMid` | 0.035 | 0.048 |
| `RetDiffOnset` | 0.074 | 0.077 |
| `RetColMid` | 0.876 | 0.855 |
| `RetNoColMid` | 0.841 | 0.807 |
| `BaseMidGap` | 0.019 | 0.018 |
| `IpwMidGapFour` | 0.0189 | 0.0188 |
| `BaseMidGapFour` | 0.0186 | 0.0184 |
| `PlaceboTau` | 0.0274 | 0.0271 |
| `BaseOverlapMidGap` | 0.019 | 0.018 |
| `OverlapInfMidGap` | -0.008 | -0.007 |
| `BaseOverlapInfMidGap` | 0.021 | 0.020 |
| `OverlapOffSupport` | 17.4 | 17.0 |
| `OverlapInfOffSupport` | 32.2 | 31.6 |
| `OverlapEssRatio` | 31 | 32 |
| `TrimShare` | 27 | 28 |
| `TippingFloor` | 0.014 | 0.013 |
| `RealocNonCollege` | 0.0005 | 0.0004 |
| `MidGapEU` | 0.002 | 0.001 |
| `WcbOnset` | 0.172 | 0.149 |

Every macro not listed is byte-identical, including the pre-pandemic gap, the trimmed and overlap-weighted gaps, the placebo count and the decomposition shares of the pre-pandemic gap.
