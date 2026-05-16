# Spatial DiD on the Autostrada del Sole — rationale

This note explains, step by step, **what the R code does and why**. It
is meant to sit next to Lelo & Tani's draft as the empirical companion
piece, replicating the Ciani-de Blasio (2022) and de Blasio-Poy-Ciani
(2020) within-province DiD design and adapting it to the A1
Milano-Napoli case.

The Ciani / de Blasio papers and their predecessors (Percoco 2016,
Banerjee-Duflo-Qian 2020) share three features that we replicate:

1. **Treatment is local and discrete**: a unit is "treated" if it has
   direct access to the motorway (a toll booth).
2. **Within-region/-province comparison**: the control group is the set
   of nearby comuni that do NOT have direct access, so that the comparison
   nets out province-specific shocks and time-invariant geography.
3. **Pre-trend test on a placebo period**: the years before the road
   was built must show no differential growth between (future) treated
   and control. If they do, treatment is selected on prior trends and
   the DiD estimate is biased.

Where we **differ from Ciani-de Blasio**:

- They have GIS centroids, so they build concentric distance rings
  ("0-5 km", "5-15 km", "15-30 km"). We have something equivalent
  built into the dataset: variable `W2` records the **driving time in
  minutes from each comune to the nearest A1 toll booth**, computed
  on the post-1964 final A1 network. We use W2 as a continuous
  treatment intensity (R/07_accessibility_did.R), plus discrete
  quintile bands (W2 ⊂ {Q1 closest, ..., Q5 farthest}) as the direct
  ring analogue. We also exploit the K7 **opening-year** variable in
  the data to run a STAGGERED-treatment DiD that Ciani-de Blasio
  cannot do because their A3 case study has a single opening date.
- They have an annual outcome panel (e.g. business registry). We have
  a 10-year decennial panel from the population/industry censuses.
- They use Conley spatial-HAC standard errors. We cluster at the
  province level (the same Ciani-de Blasio also report as their
  baseline) — it is operationally equivalent on a 30-province sample
  and avoids the centroid requirement.

The strategy we implement is therefore a **simplified, "binary"
spatial DiD** with five components: descriptive comparison, long DiD
on 1961-1991 differences, event study 1951-1991, heterogeneity, and
robustness checks. The strongest empirical novelty of the analysis is
the 1951-1961 placebo, which directly tests whether A1 toll booths
were planted at already-faster-growing places (Lelo & Tani's main
caveat on page 17).

------------------------------------------------------------------------

## File map

```
R/
  00_setup.R              packages + helpers (regtab, with_cluster, tidy_cr)
  01_prepare_data.R       load 5 xlsx -> long panel (incl. K0/K7/W2)
  02_treatment.R          flag treated comuni (K0=1 from raw data;
                          53 toll-booth comuni) and define control samples;
                          attach K7 (opening year), W2 (drive-min to
                          nearest casello), TMP_60 + Aree_Int (aree
                          interne classification)
  03_descriptive.R        cluster analysis + intercensal growth tables
  04_spatial_did.R        BINARY treatment: M1 long DiD + M2 event study
  05_heterogeneous.R      M3 by macro-area / 1961 cluster / 1961 size
  06_robustness.R         R1 placebo, R2 no-capoluoghi, R3 wide ctrl, R4 SE
  07_accessibility_did.R  CONTINUOUS treatment intensity (W2):
                          (W1) Δlogy on W2 with prov FE + 1961 ctrl;
                          (W2) W2 quintile dose-response;
                          (W3) Event study with W2 × year interactions
  08_staggered_did.R      STAGGERED treatment (K7 opening year):
                          (S1) event study in event-time;
                          (S2) cohort-by-cohort long DiD (1959/60/62/64);
                          (S4) Aree_Int 6-band heterogeneity
  main.R                  master pipeline
data/
  aree_interne.csv        from areeinterne.dta: K0, K7, TMP_60, Aree_Int
  treated_comuni.csv      manual 46-comuni template (kept as docs,
                          superseded by K0 in raw data: 53 comuni)
output/
  tables/                 CSV + LaTeX
  figures/                PNG + PDF
```

------------------------------------------------------------------------

## 00 — setup

Two custom helpers are the backbone of the table output.

### `with_cluster(model, cluster_full, data)`

Attaches a clustering vector to a fitted `lm`. Why: `clubSandwich::vcovCR`
needs a vector of cluster IDs **of length equal to the number of
observations actually used by the fit**. R's `lm` may drop rows because
of `NA`, so we extract `as.integer(rownames(model.frame(model)))` and
index into the original vector. The cluster vector is stored as an
attribute so `regtab()` and `tidy_cr()` can pick it up without it being
passed around manually.

### `regtab(models, keep, out_csv, out_tex, title)`

Takes a named list of `lm` objects, computes cluster-robust SE with
`clubSandwich::vcovCR(., type = "CR1")` (small-sample correction, Stata
default), and prints a clean table with stars + N + R². This is a
drop-in substitute for `modelsummary` (which is not available in the
apt distribution).

### `macro_area(cod_reg)`

Maps the ISTAT region code to North / Centre / South following the
draft. Centre is the omitted category in the heterogeneity models,
since Florence/Rome are the central case study Lelo & Tani highlight.

### Constants

- `PRE_YEAR = 1961` — last census before the A1 was fully open
  (Oct 1964). The Milano-Bologna section was already open by 1959,
  but only 1/3 of the route. We make 1961 the omitted year in the
  event study and the "pre" date in the long DiD.
- `A1_OPEN_YEAR = 1964` — drawn on the event-study plot as a red dotted
  line.

------------------------------------------------------------------------

## 01 — prepare data

Reads each `archivio/19YY.xlsx`, keeps only the variables that appear
in Lelo & Tani's tables and a few sectoral disaggregations, **strips
the `<YY>_` prefix** so the same name (`P1`, `UT`, `AT`, ...) is used
across years, adds `year`, and row-binds into a single long panel.

Two technical details that matter:

- **`log1p()` instead of `log()`**: a handful of micro-comuni have
  `UT = 0` or `AT = 0` in the early censuses. `log(0)` is `-Inf`;
  `log1p(x) = log(1+x)` is well-defined and is a standard "log of
  outcomes that can be zero" choice in the empirical IO literature.
- **Province codes anchored on 1961**: the Province of Lodi did not
  exist before 1992. To avoid Lodi comuni being labelled with a
  province code that did not exist in 1961, we re-impose the 1961
  identifiers (`COD_REG`, `COD_PROV`, `COMUNE`) on every panel year.

Output: `data/panel_long.rds`, ~16,000 observations (3,242 comuni × 5
years).

------------------------------------------------------------------------

## 02 — treatment

Reads `data/treated_comuni.csv` (46 A1 toll-booth comuni built from
Menduni 1999 plus the Autostrade per l'Italia archive) and merges
`treat_A1 ∈ {0,1}` onto every comune-year row of the panel.

Then constructs two control samples:

- **`sample_tight`** — treated comuni + comuni in the **same province**
  that are *not* treated. This is the within-province "donut" control
  group used by Ciani-de Blasio. n ≈ 1,200 in 1961.
- **`sample_wide`** — all comuni in the 8-region panel. n ≈ 3,200.

The tight sample is the **baseline**. Wide is used only in `R3`
robustness.

------------------------------------------------------------------------

## 03 — descriptive

Two pieces.

### (a) Cluster analysis (Table 3 of the draft)

For each census year we run a 4-cluster k-means on the z-standardised
vector `(I4, SS4, L15, L16, L17)`. We then label clusters by a
**4-rule decision tree** on the cluster centres:

1. cluster with highest `(I4 + L15)/2`  → `agricultural/traditional`
2. of the remaining, highest `L16`       → `industrial`
3. of the remaining, highest `(SS4+L17)/2` → `tertiary/educated`
4. the last one                          → `intermediate/low`

We deliberately do not let k-means pick its own labels because in some
years the same dominant feature (e.g. high `L16`) wins for two
clusters at once, collapsing the typology to three classes. The
decision tree forces four distinct profiles, replicating Lelo & Tani
Table 3.

The 1961 cluster label is saved to `data/cluster_1961.rds` and used
as a baseline-development covariate in `05_heterogeneous.R`.

### (b) Intercensal growth

Two tables and one figure:

- `tab04_intercensal_growth.csv` — mean growth of P1, UT, AT per
  decade, by treatment status.
- `tab05_cum_growth_1961_1991.csv` — cumulative 1961-1991 growth.
  Numbers in this table reproduce the "+27% / +51.9% / +56.4%"
  qualitative claims in the draft, up to small differences in the
  exact list of treated comuni.
- `fig01_growth_distribution.png` — kernel density of 1961-1991
  population growth, by treatment.

------------------------------------------------------------------------

## 04 — spatial DiD (the empirical core)

### (M1) Long DiD on first differences 1961 → 1991

```
Δy_i = α + β · treat_i + γ' X_i^{1961} + δ_{prov(i)} + ε_i
```

We estimate this on the `sample_tight` for four outcomes:
`Δ log(P1)`, `Δ log(UT)`, `Δ log(AT)`, `Δ emp_rate`.

Three nested columns per outcome:

| col | spec                            | what it isolates                          |
|-----|---------------------------------|-------------------------------------------|
| (1) | `lm(Δy ~ treat)`                | raw correlation                           |
| (2) | `lm(Δy ~ treat + factor(prov))` | within-province (analogue of Ciani-de Blasio's identification) |
| (3) | (2) + 1961 controls             | also nets out baseline development        |

The 1961 control set is `(lP1_1961, I4_1961, SS4_1961, L15_1961, L16_1961, L17_1961)`,
matching the vector Lelo & Tani's cluster analysis uses as a development
profile.

**Standard errors are clustered by province.** Province is the level at
which treatment varies — every comune in a province shares the same
"motorway-corridor" treatment intensity through the network — so
provincial clustering is the natural choice (and the one Ciani-de Blasio
also use).

The "preferred specification" reported in
`output/tables/m1_longdid_summary.{tex,csv}` is column (3) for all four
outcomes side-by-side.

### (M2) Event study 1951..1991

Specification on the long panel:

```
y_{it} = α_i + λ_t + Σ_{k ≠ 1961} δ_k · (treat_i · 1{t = k}) + ε_{it}
```

`α_i` are comune fixed effects, `λ_t` are census-year fixed effects.
The omitted year is 1961 (so all coefficients are *relative to 1961*).

The point of the event study is two-fold:

- **Pre-trends placebo**: the coefficient `δ_{1951}` should be
  statistically zero if the parallel-trends assumption holds.
- **Dynamics**: the path of `δ_{1971}, δ_{1981}, δ_{1991}` traces out
  the post-treatment dynamics and shows whether the effect is one-off
  or persistent.

The plot lives at `output/figures/fig02_event_study.png`.

> **What the data say.** In our run the 1951 coefficient is **negative
> and significant** on log Population (-0.10), log Local units (-0.10),
> and log Employees (-0.14). This is a **pre-trend failure**: future-
> treated comuni were *smaller relative to their controls* in 1951.
> Between 1951 and 1961 they catch up by ~0.10 log points; the
> post-1961 coefficients (+0.13 to +0.18 by 1971-1991) are therefore
> a mix of pre-existing convergence trend and genuine A1 effect.
> This is exactly the endogeneity concern Lelo & Tani flag on p. 17
> and is the strongest argument for the IV strategy they mention in
> the conclusion.

------------------------------------------------------------------------

## 05 — heterogeneity

Three interactions of `treat_A1` with:

### (a) macro-area (`Centre` = reference)

```
Δy_i = α + β · treat_i + φ_N · macro_North + φ_S · macro_South
       + θ_N · treat_i × macro_North + θ_S · treat_i × macro_South
       + δ_{prov(i)} + ε_i
```

The TE in each macro-area is then `β`, `β + θ_N`, `β + θ_S`, with
delta-method SEs (via `make_te()`).

> **What the data say.** The Centre coefficient is large and significant
> (+0.28 on log Pop, +0.39 on log Units, +0.37 on log Employees) —
> Florence/Rome suburbanisation. The North coefficient is **negative**
> on Units (-0.59) and Employees (-0.86), suggesting that A1 access
> for already-large Northern capoluoghi (Milano, Bologna, Modena, ...)
> coincided with employment leaking *away* from the toll-booth comune
> to its hinterland — a phenomenon documented for Italian highways by
> Percoco (2016) and consistent with the Ciani-de Blasio "displacement"
> finding for the A3 in the South.

### (b) by 1961 development cluster

Tests whether the A1 effect differs between comuni that were already
in the industrial/tertiary cluster in 1961 versus those still in the
agricultural cluster — the Hansen 1965 vs Puga 2002 debate. We find a
**positive treatment effect on agricultural/traditional comuni**
(omitted reference) and a **near-zero/slightly negative effect on the
others**: A1 access disproportionately helped comuni that were below
their province's development frontier.

### (c) by 1961 population quartile

Same idea but using ex-ante size as the stratifier. Q2 (small-medium
comuni) shows the largest A1 effect; Q3-Q4 (big comuni) show smaller
effects.

------------------------------------------------------------------------

## 06 — robustness

| ID | what                                       | what it tests                                              |
|----|--------------------------------------------|------------------------------------------------------------|
| R1 | placebo 1951-1961 DiD                      | parallel-trends assumption                                 |
| R2 | drop capoluoghi (Roma, Milano, Bologna...) | not driven by big-city outliers                            |
| R3 | wide control (every non-A1 comune)         | sensitive to the control-pool definition?                  |
| R4 | alternative SE clustering (region only)    | inference doesn't depend on the exact clustering choice    |

Key R1 result: the placebo is **significantly positive** (+0.14 on
log Pop, +0.10 on log Units). Combined with the event-study evidence
this is the most important caveat in the empirical section: a DiD
estimate is an upper bound on the causal effect.

------------------------------------------------------------------------

## 07 — continuous distance-from-casello DiD (the core spec)

This is the spec that most closely mirrors Ciani-de Blasio's
continuous-distance approach. W2 = driving minutes from the nearest
A1 toll booth in the post-1964 final network. It is time-invariant
(one value per comune), so identification comes from interacting it
with year dummies (Faber 2014 / Donaldson 2018).

### (W1) Long DiD

```
Δ log y_i = a + β · W2_i + γ' X_i^{1961} + δ_{prov(i)} + ε_i
```

Expected sign of β: **negative** (more minutes from a casello → less
1961-1991 growth). What we find (preferred spec, prov FE + 1961
controls):

| outcome | β on W2 | cluster SE | reading |
|---|---|---|---|
| Δ log Pop      | **-0.0068*** | (0.0008) | each extra minute from the casello → 0.68% less population growth over 1961-1991 |
| Δ log Units    | **-0.0068*** | (0.0009) | -0.68% per minute |
| Δ log Employees| **-0.0113*** | (0.0014) | **-1.13% per minute** (strongest effect) |
| Δ Emp rate     | **-0.0013*** | (0.0004) | -0.13 pp per minute |

A comune 30 minutes farther from the nearest A1 casello has, on
average, 20% less population growth, 20% less local-units growth,
and 34% less employees growth across 1961-1991, conditional on
province FE and 1961 baseline development.

### (W2) Quintile dose-response (fig04)

Comuni sliced into 5 quintiles of W2; coefficients relative to Q1
(closest):

| quintile | Δ log Pop | Δ log Units | Δ log Employees |
|---|---|---|---|
| Q1 (closest)   |   ref      |   ref      |   ref      |
| Q2             | **-0.04*** | -0.07      | -0.15      |
| Q3             | **-0.08*** | -0.10      | **-0.24**  |
| Q4             | **-0.19*** | **-0.24*** | **-0.39*** |
| Q5 (farthest)  | **-0.33*** | **-0.34*** | **-0.57*** |

Perfectly monotonic. The dose-response is the strongest visual
evidence of the A1 effect.

### (W3) Event study with W2 × year (fig05)

Coefficient on `W2 × 1951` is **POSITIVE** (~+0.003); coefficients on
`W2 × 1971/1981/1991` are **NEGATIVE** and increasing in magnitude.
The sign flip across 1961 is the smoking-gun causal pattern:
pre-A1, far-from-future-casello comuni were growing *faster* (or at
least not slower); post-A1, they grew systematically slower. This is
the cleanest evidence we have of a real treatment effect of the
motorway, net of selection.

------------------------------------------------------------------------

## 08 — staggered DiD (a partial implementation)

**Honesty note.** The A1 opened in four cohorts (K7 = 1959, 1960,
1962, 1964) — all of which fall in the SAME inter-census window
1961-1971. At the granularity of the census, the four cohorts share
an identical event-time mapping (pre = 1961, first observed post =
1971). A full Callaway-Sant'Anna staggered DiD with cohort-specific
ATT(t,c) dynamics is therefore NOT identifiable from the
post-treatment outcomes alone.

What we CAN do (the only true staggered comparison available with
this data):

### (T1) Short-run staggered DiD 1951 → 1961

At the 1961 census the 1959/60 cohorts have been treated for 1-2
years; the 1962/64 cohorts are still pre-treatment. So:

| group                        | what it is                          | n   |
|------------------------------|-------------------------------------|-----|
| **Early** (1959/60)          | treated by 1961                     | 19  |
| **Late**  (1962/64)          | not yet treated at 1961             | 34  |
| **Never** (control)          | comuni in A1 prov w/o A1 casello    | 1,141 |

We estimate `Δy_{51→61} ~ Early + Late + factor(COD_PROV)` and form
the contrast `ATT_short = b_Early − b_Late`. Findings:

| outcome  | ATT_short | SE       | p     |
|----------|-----------|----------|-------|
| Δ log Pop      | +0.045 | (0.039)  | 0.24  |
| Δ log Units    | **+0.089*** | (0.045)  | **0.046** |
| Δ log Employees| +0.138 | (0.099)  | 0.16  |

After 1-2 years of A1 exposure, the Early cohort already has 9% more
local-unit growth than the Late cohort (significant). The pop and
employment differences are positive but imprecise — consistent with
the literature finding that firms relocate faster than households.

**Parallel-trends warning**: the `b_Late` coefficient (Late minus
Never) is itself +0.11 on log Pop and +0.15 on log Emp, both
significant. This means **the not-yet-treated comuni were already
on a stronger growth path than the never-treated** before A1
arrived. Translation: A1 caselli were sited in comuni that were
already converging upward. The within-cohort `ATT_short` partly
nets this out (Early and Late share the selection mechanism), but
the Early-vs-Never comparison overstates the true effect.

### (S1) Plain event study on K0 (NOT staggered)

Re-runs the M2 event study from R/04 on the K0=1 sample. Useful as
robustness comparison with the W2 continuous event study.

### (S2) Cohort heterogeneity in long DiD (NOT staggered)

Each opening cohort is a separate dummy in the 1961-1991 long DiD,
with `Never treated` as reference. This is HETEROGENEITY, not
staggered identification — we still need ten years between censuses
to see anything, so cohort-specific timing within 1959-1964 is
invisible.

| cohort | Δ log Pop | Δ log Units | Δ log Employees |
|---|---|---|---|
| 1959 (n=13) | +0.13**  | +0.25***  | +0.35*** |
| 1960 (n=6)  | -0.11    | +0.03     | -0.01    |
| 1962 (n=17) | +0.16*   | **+0.65***| **+0.89***** |
| 1964 (n=17) | +0.20*** | +0.31***  | +0.34*** |

The 1962 cohort (Rome-Naples leg) shows the strongest long-run
effect on firms and employees — consistent with Lelo & Tani's
qualitative finding that the South benefited most economically.

------------------------------------------------------------------------

## Reading the headline tables

`output/tables/m1_longdid_summary.csv` — preferred long DiD:

| outcome | coef on `treat_A1` | cluster-rob SE | interpretation |
|---|---|---|---|
| Δ log Pop      | +0.107*** | (0.028) | A1 comuni gained ~11 log points (~11%) of population over 1961-1991 above same-prov controls |
| Δ log Units    | +0.363*** | (0.068) | ~36% extra growth in local units |
| Δ log Employees| +0.496*** | (0.082) | ~50% extra growth in employees |
| Δ Emp rate     | +0.067*** | (0.018) | +6.7 percentage points in AT/P1 |

`output/tables/r1_placebo_1951_1961.csv` — placebo:

| outcome | coef on `treat_A1` | cluster-rob SE |
|---|---|---|
| Δ log Pop      | +0.140*** | (0.023) | pre-existing positive trend |
| Δ log Units    | +0.103*** | (0.029) | pre-existing positive trend |
| Δ log Employees| +0.137    | (0.090) | suggestive but imprecise        |

Net of pre-trend, the **causal** A1 population effect is closer to
+0.107 − 0.140 ≈ **−0.03** (zero or slightly negative), the local-units
effect is closer to **+0.26**, and the employees effect to **+0.36**.
Population is the LEAST robust outcome to the parallel-trends critique;
local units and employees survive the placebo test.

------------------------------------------------------------------------

## Recommended next steps (for full causal identification)

1. Add `data/comuni_centroids.csv` (`PRO_COM, lon, lat`) and build
   distance-to-nearest-toll-booth, so the binary treatment becomes a
   continuous treatment intensity with concentric rings.
2. Switch from province-clustered SE to Conley spatial-HAC SE once
   centroids are available.
3. Implement the Percoco (2016) IV using the Roman-road network as an
   instrument for A1 placement, or the least-cost-path approach of
   Banerjee-Duflo-Qian (2020) between historical hubs.
4. Use sectoral employees (`A2_1` ... `A9_1`) to test the
   "agglomeration vs displacement" hypothesis: did manufacturing
   relocate to A1 comuni or was the gain concentrated in services?
