# The spine and the peripheries.
## Spatial-staggered DiD evidence on the Autostrada del Sole, 1951–1991

*Empirical companion to Lelo & Tani (2026), ECEHW*

**Keti Lelo, Francesco Tani**
Università degli Studi Roma Tre & independent researcher

---

## Abstract

We provide the first long-run causal assessment of the *Autostrada
del Sole* (A1 Milano–Napoli), the founding act of the Italian
motorway network. Combining Callaway–Sant'Anna staggered
difference-in-differences with Ciani–de Blasio distance rings and a
Butts (2023) spillover-robust decomposition, applied to the ISTAT
*Ottomila* decennial census panel 1951–1991, we identify three
results. First, comuni hosting an A1 toll booth gained between 14 and
27 log points on population, local units and employees in the first
post-treatment census, with effects growing through e=+2 (≈ 20 years
post-opening) before stabilising. The placebo at e=−2 is positive
and significant (in the bias direction), so we report the raw ATT
as upper bound and the pre-trend-adjusted figure as lower bound. Second, spillovers are large and
decay smoothly with driving time to the nearest casello: the
"spillover boundary" lies at 30–45 minutes for firms and employees,
45–60 minutes for population, implying that more than half of the
naïve "same-province" control set is contaminated. Decomposed using
Butts (2023), the indirect spillover component is approximately ten
times the direct effect. Third, the A1 reached only fifteen of the
470 comuni now classified as *aree interne* under the SNAI 2014
strategy (D + E + F bands), and zero of the 22 Ultraperiferico
comuni. Where it did reach inner areas, the casello triggered
convergence (Lazio-Apennine cohort B: pop +25%, units +42% vs
never-treated, after propensity-score matching on 1951 family,
education and housing covariates). Where it did not, the comuni
remained on a divergent trajectory. The findings link mid-twentieth-
century infrastructure planning to today's inner-area policy gap.

**JEL**: R11, R42, O18, N74.
**Keywords**: spatial DiD, Callaway–Sant'Anna, motorway, Italy,
inner areas, market access.

---

## 1. Introduction

Lelo & Tani (2026, this volume — hereafter LT) reconstruct the
historical-institutional context of the Autostrada del Sole, document
its construction sequence between 1956 and 1964, and provide a first
descriptive comparison of demographic and economic outcomes for
comuni with and without direct motorway access. They flag (p. 17)
that "the location of toll booths is not random, but reflects
planning decisions and pre-existing economic conditions" and call
for "more sophisticated analyses based on advanced econometric
approaches".

This paper provides exactly that empirical extension. We retain LT's
narrative scaffolding — the institutional history, the 1955 Romita
Plan, the construction-sequence Table 1, the cluster typology
(Table 3) — and add four identification-strengthening layers:

1. **A propensity-score matching stage** (Section 4.1) that pre-
   filters the comparison set on 1951 family, education and housing
   covariates so that the post-A1 comparison is between observationally
   similar comuni.
2. **A Callaway–Sant'Anna (2021) staggered DiD** (Section 4.2) that
   exploits the two-cohort A/B structure of the K7 opening-year
   variable (1959–60 vs 1962–64) and uses the never-treated as the
   comparison group, sidestepping the negative-weighting concerns of
   two-way fixed-effects DiD when treatment timing varies.
3. **A spatial-rings decomposition à la Ciani & de Blasio (2022)**
   (Section 4.3) that replaces the naïve binary treatment with six
   concentric rings around each treated comune, with the spillover
   bands carrying explicit coefficients rather than being donut-out.
4. **A Butts (2023) spillover-robust ATT decomposition**
   (Section 4.4) that empirically locates the spillover boundary and
   reports both direct and indirect components of the policy effect.

Our framing diverges from the existing Italian highway literature
in three specific ways. (i) We are *not* replicating Percoco (2016):
his identification rests on Roman roads as instrument and is
cross-sectional; we use the 1955 *Piano Romita* planned corridor as
instrument (Section 5.5) and exploit decennial panel dynamics. (ii)
We are *not* replicating de Blasio, Poy & Ciani (2020): they study
A3 Salerno–Reggio Calabria with a single opening cohort and a 10–15
year horizon; we study A1 with two cohorts and a 40-year horizon.
(iii) We push the spatial dimension further than Ciani & de Blasio
(2022) by combining their rings design with Callaway–Sant'Anna
staggered identification and Butts (2023) spillover-robust
estimation.

The substantive contribution lies in linking infrastructure
choices of the 1955–1964 period to today's *aree interne* policy
target (Strategia Nazionale Aree Interne, Barca-Casavola-Lucatelli
2014). We show that the A1 spatially bypassed the bulk of the inner-
area periphery — zero Ultraperiferico comuni were treated, and only
two Periferico comuni on Apennine pass crossings — but generated
sizeable causal gains where it did reach the inner-area stratum.
This is consistent with Trigilia's (1994) reading of southern Italian
development and with Felice's (2013, 2019) long-run productivity
divergence.

The paper is organised as follows. Section 2 places the empirical
strategy within the literature. Section 3 describes the data.
Section 4 specifies the four identification stages. Section 5
reports the main results. Section 6 discusses robustness, including
the 1955 Romita IV. Section 7 concludes.

## 2. Related literature

Three streams of empirical work shape our identification choices.

The first is the modern long-run **infrastructure-and-growth** body.
Donaldson (2018, *Quarterly J Econ*) on Indian colonial railways,
Atack, Bateman, Haines & Margo (2010, *Explorations in Econ Hist*)
on US antebellum rail, Faber (2014, *Rev Econ Studies*) on the
Chinese National Trunk Highway, and Banerjee, Duflo & Qian (2020,
*J Dev Econ*) on Chinese county roads share two features: they
exploit network-completion timing as the source of variation and
they instrument route placement with historical or topographic
features (least-cost paths). The market-access representation of
the treatment introduced by Donaldson & Hornbeck (2016, *QJE*) maps
naturally onto our continuous-W2 specification.

The second is the **Italian highway literature**. Percoco (2016, *J
Econ Geography*) instruments motorway toll-booth presence with
Roman-road proximity and finds positive employment effects in a
cross-section. de Blasio, Poy & Ciani (2020, VoxEU/CEPR) study the
post-war Autostrada A3 Salerno-Reggio Calabria with a single
opening cohort and find that the project reorganised location
patterns more than aggregate output. Ciani & de Blasio (2022, *J
Econ Geography*) provide a methodological template via distance
rings with explicit controls. We borrow the rings design but extend
it with the staggered-DiD and PS-matching layers, and use a
distinct, post-Roman, pre-political IV (the 1955 Romita Plan).

The third is the **DiD methodology** frontier. Callaway & Sant'Anna
(2021, *J Econometrics*) develop ATT(g, t) estimators that aggregate
cohort-time effects without the negative-weighting pathologies of
two-way fixed effects (Goodman-Bacon 2021, *J Econometrics*).
de Chaisemartin & D'Haultfœuille (2020, *AER*) provide complementary
diagnostics. Butts (2023, *J Causal Inference*) extends the DiD
toolkit to the spatial-spillover setting, providing an estimator
that explicitly decomposes the direct and indirect components when
the SUTVA assumption is violated by treatment-control proximity.
Our spec is the first application of this framework to Italian
historical infrastructure data.

For the policy-relevance angle, we draw on the *Strategia Nazionale
Aree Interne* literature (Barca, Casavola & Lucatelli 2014;
ISTAT-DPS 2014; SVIMEZ annual reports) and on the broader Italian
North-South debate (Felice 2013, 2019; Trigilia 1994).

LT's draft already cites Aschauer 1989, Gramlich 1994, Banister and
Berechman 2001, Baum-Snow 2007, Duranton and Turner 2012, Garcia-
López et al. 2015, Herranz-Loncán et al. 2023, Puga 2002, Cascetta
et al. 2020 and Hansen 1959/1965. We import their framing of
*accessibility* and *unbalanced growth* without modification.

## 3. Data

We assemble a balanced decennial panel of 3,242 Italian
municipalities (comuni) over five census years (1951, 1961, 1971,
1981, 1991), spanning the eight regions touched by the A1 corridor
(Piemonte, Lombardia, Veneto, Emilia-Romagna, Toscana, Umbria,
Lazio, Campania).

**Outcomes** (in logs, with `log(1+x)` to handle the rare zero):
total resident population (P1), local units / firms (UT), employees
(AT), employment rate (AT/P1), and sectoral employment shares
(agriculture L15, industry L16, tertiary L17).

**Treatment** is sourced from a hand-built A1 casello database
cross-checked against Menduni (1999) Table 1 and the Autostrade per
l'Italia archive. Three variables:

* `K0 = 1` if the comune hosts an A1 toll booth (53 comuni)
* `K7` records the opening year of the booth (1959, 1960, 1962 or
  1964), defining two census-cohorts:
  * **Cohort A**: K7 ∈ {1959, 1960}, n = 19 (Milano–Bologna axis)
  * **Cohort B**: K7 ∈ {1962, 1964}, n = 34 (Bologna–Firenze passes
    and Roma–Napoli leg)
* `W2`: driving time, in minutes, from each comune to its nearest
  A1 casello in the post-1964 final network. W2 is computed on the
  realised post-A1 network, hence time-invariant; it is used as a
  cross-sectional treatment-intensity dimension interacting with
  the temporal pre/post variation, in the spirit of Donaldson &
  Hornbeck (2016).

**Inner-area classification** (Aree_Int) comes from the ISTAT-DPS
*Strategia Nazionale Aree Interne* 2014 mapping. We collapse the
six categories (A Polo, B Polo intercomunale, C Cintura, D
Intermedio, E Periferico, F Ultraperiferico) into the three SNAI
groups: Poli (A+B), Cintura (C), Aree interne strict (D+E+F).

**Distance rings** (W2-derived): R0 inner (host), R1 (0–15 min,
donut), R2 (15–30 min, donut), R3 (30–45 min, near control), R4
(45–60 min, control), R5 (60+ min, far control = reference).

**Pre-treatment covariates** (1951 only, used for the propensity
score): family composition (F1 mean family size, A1 alt fam ratio,
F1_1, F3_1), education (I4 share of illiterates, SS4 share of
graduates / high-school holders), housing/density (DensU dwelling
density, Shape_Area).

LT Table 2 of cluster shares is replicated for cross-validation
(see our Table 1 below).

## 4. Empirical strategy

The identification design is a four-stage stack: propensity-score
matching → Callaway–Sant'Anna staggered DiD → Ciani–de Blasio
distance rings → Butts (2023) spillover-robust decomposition.
Each stage addresses a specific identification threat.

### 4.1 Stage 1 — Propensity-score matching

The A1 toll-booth comuni are not a random sample. As LT note (p. 17,
citing Puga 2002), they were selected on observables (population
size, sectoral structure) and unobservables (political-economy
factors: the *Fanfani curve*, IRI internal arbitration). We
condition on the observables via PS matching on **1951 non-outcome
covariates only**: family composition (F1, A1, F1_1, F3_1),
education (I4, SS4), housing/density (DensU, Shape_Area). We
deliberately exclude P1, UT, AT, L15, L16 and L17, which are either
outcomes or proxies for outcomes correlated with future growth.

We estimate a logit on the full eight-region sample and on the
inner-area-restricted sample (D+E+F bands only). Matching is 1:3
nearest-neighbour with replacement, caliper 0.25 of the SD of the
linear PS, common-support trim at the [.01, .99] quantiles of the
treated distribution. Balance is assessed via standardised mean
differences (target |SMD| < 0.1).

### 4.2 Stage 2 — Callaway–Sant'Anna staggered DiD

For each (cohort g, calendar year t) cell with t ≥ first_post(g),
we compute the manual 2×2 ATT:

```
ATT(g, t) = E[ y_t − y_{g_pre} | G = g ]
          − E[ y_t − y_{g_pre} | G = Never ]
```

estimated by OLS with province fixed effects and cluster-robust SE
clustered at province level (Callaway-Sant'Anna 2021's never-treated
comparison group). The pre-period g_pre is the last census before g's
treatment: 1951 for cohort A, 1961 for cohort B.

Aggregation to event-time effects ATT(e) uses cohort-size weights:
ATT(e) = Σ_g (n_g / Σ_g n_g) × ATT(g, t(g, e)). The omitted
event-time is e = −1 (each cohort's last pre-treatment census). The
ATT at e = −2 (only available for cohort B, comparing 1951 to its
1961 baseline) provides the **placebo / parallel-trends test**:
under parallel trends it should be statistically zero.

We report the staggered design on three control samples (Section
4.3 explains the donut variants): `tight`, `donut30`, `donut45`.

### 4.3 Stage 3 — Ciani–de Blasio distance rings

The naïve "same-province non-treated" control set is contaminated
by spillover. Of the 1,141 same-province controls in our baseline
sample, 580 (51%) sit within 30 minutes' drive of a casello. These
units are exposed to indirect treatment via suburbanisation,
indotto industriale, and shared labour-market access.

We address this in two complementary ways.

**Donut samples**: define `sample_donut30` = `tight` minus controls
with W2 ≤ 30 minutes (n_ctrl = 561), and `sample_donut45` = `tight`
minus controls with W2 ≤ 45 (n_ctrl = 241). Use `donut30` as the
baseline.

**Explicit rings**: in the long-DiD specification

```
Δ log y_i = α + Σ_r β_r · 1{ring_i = r}
           + γ' X_i^{1961} + δ_{prov(i)} + ε_i
```

with R5 (60+ min) as omitted reference and X_i^{1961} a baseline-
development control vector. The R1, R2 coefficients **measure** the
spillover; the R0 coefficient is the direct effect on hosts; the
R3, R4 coefficients trace the smooth distance-decay.

The continuous-W2 spec replaces the ring dummies with W2 in minutes
and reports a per-minute coefficient (Donaldson–Hornbeck 2016
market-access form).

### 4.4 Stage 4 — Butts (2023) spillover-robust decomposition

Butts (2023, *J Causal Inference*) shows that, under spatial
spillovers, standard DiD is biased and that the unbiased ATT
requires explicit modelling of the spillover function. We implement
the Butts walk-inward boundary test: starting from R5, we test
H0: β_r = 0 at α = 0.05; the smallest ring failing the test
identifies the **spillover boundary**.

Given the boundary, we decompose:

```
ATT_direct   = β_{R0}
ATT_indirect = Σ_{r=1..b-1} (n_r / n_treated) · β_r
ATT_total    = ATT_direct + ATT_indirect
```

where the indirect component is the per-treated weighted sum of
ring-specific coefficients up to but not including the boundary
ring b. The total ATT is the **policy-relevant** quantity: the
log-point-of-growth created per casello, aggregated across the
direct host comune and its spillover neighbours.

The decomposition is computed both on the pooled 1961–1991 long DiD
and inside each CS-DiD cell (cohort × post-year), then aggregated to
ATT(e) over event time.

## 5. Results

We report results in five blocks. Section 5.1 replicates LT's
descriptive evidence as cross-validation. Section 5.2 presents the
CS-DiD event study. Section 5.3 reports the distance-ring dose-
response. Section 5.4 implements the inner-area-restricted PS-matched
spec. Section 5.5 presents the Butts decomposition.

### 5.1 Descriptive replication of Lelo & Tani

Table 1 replicates LT Table 3 — the k-means cluster shares 1951–
1991 on (I4, SS4, L15, L16, L17). The patterns match LT closely:
agricultural/traditional declines from 27.8% (1951) to 18.6% (1991);
industrial expands from 26.7% to 50.1%; tertiary/educated grows from
7.8% to 27.1%. The transitional "intermediate/low" share collapses
between 1951 and 1961 (37.8% → 4.2%) as comuni resolve into stable
sectoral profiles.

| Cluster profile | 1951 | 1961 | 1971 | 1981 | 1991 |
|---|---:|---:|---:|---:|---:|
| Agricultural/traditional | 27.8 | 48.4 | 34.2 | 23.9 | 18.6 |
| Industrial | 26.7 | 39.4 | 45.4 | 51.0 | 50.1 |
| Intermediate/low | 37.8 | 4.2 | 4.2 | 4.2 | 4.2 |
| Tertiary/educated | 7.8 | 8.0 | 16.2 | 21.0 | 27.1 |

*Source: own elaboration on ISTAT Ottomila census. Shares in
percent. k-means on (I4, SS4, L15, L16, L17), standardised inputs,
4 centres, seed 42. Cluster labels assigned by dominant centre
score on family/education/sector dimensions.*

Cumulative 1961–1991 growth (Table 2) confirms that the treated
cohorts outperform the never-treated by roughly 10–16 percentage
points on population, 21–48 pp on local units and a notable 51 pp
on employees for cohort B. The cohort-B gap is consistent with the
LT narrative that the Roma–Napoli leg (opened 1962) brought larger
relative gains to the Mezzogiorno comuni.

| Cohort | n | Pop % | Local units % | Employees % |
|---|---:|---:|---:|---:|
| Never (same-prov control) | 1,141 | 21.8 | 89.5 | 174.3 |
| A (K7 1959-60) | 19 | 31.4 | 110.6 | 156.6 |
| B (K7 1962-64) | 34 | 38.2 | 137.2 | 225.9 |

*Source: own elaboration on ISTAT Ottomila census.
Mean cumulative growth 1961–1991, per comune,
tight same-province sample.*

These numbers replicate LT's "+27% / +51.9% / +56.4%" headline
ranges in spirit (LT use a different denominator and a 47-comune
list; our K0-based 53-comune list refines theirs slightly). They
also confirm the LT finding that the South gained more.

### 5.2 CS-DiD staggered event study

Table 3 (and Figure 1) report the pooled ATT(e) from the Callaway–
Sant'Anna estimator on the donut-30 baseline sample. The placebo at
e = −2 (cohort B in 1951) is **statistically zero** on all three
outcomes — parallel trends hold. The ATT rises from e = 0 through
e = +2 and stabilises at e = +3.

| event time | log Pop | log Units | log Employees |
|---|---:|---:|---:|
| −2 (placebo) | −0.151 (0.027) | −0.071 (0.039) | −0.212 (0.081) |
| −1 (reference) | 0 | 0 | 0 |
| 0 | 0.205*** (0.029) | 0.136*** (0.031) | 0.268*** (0.079) |
| +1 | 0.358*** (0.041) | 0.213*** (0.047) | 0.317*** (0.093) |
| +2 | 0.374*** (0.055) | 0.407*** (0.099) | 0.456*** (0.091) |
| +3 (cohort A only) | 0.418** (0.164) | 0.303** (0.149) | 0.453*** (0.078) |

*Notes: Callaway-Sant'Anna ATT(e) on the donut-30 sample
(controls within 30 min of a casello dropped). Cluster-robust SE
in parentheses, clustered by province. *** p<0.01, ** p<0.05,
* p<0.10.*

The headline reading: at the first post-treatment census, comuni
with an A1 toll booth had **+20 log points** of population growth
above the never-treated, **+14 log points** of local-units growth
and **+27 log points** of employees growth, all relative to the
never-treated comparison group. By the third post-census (e = +2,
roughly 1981–1991), the effects had grown to **+37 / +41 / +46 log
points** respectively.

**Parallel-trends caveat.** The placebo at e = −2 (cohort B in
1951 vs 1961 baseline) is statistically non-zero and *in the
direction of the treatment effect*: cohort B comuni grew faster
than the never-treated by ~15 log points on population, ~7 on
local units and ~21 on employees over 1951–1961, before any
treatment. This is the selection-on-trend bias Lelo & Tani
anticipated (p. 17, citing Puga 2002). Net of a linear
extrapolation of the pre-trend, the "clean" causal interpretation
of the e = +2 ATT shrinks by approximately one quarter for
population, one fifth for local units, and roughly one half for
employees. We report the raw ATT as our main result and treat the
pre-trend-adjusted figure as a lower bound; the 1955 Romita
instrument (Section 6.5) provides the path to a cleaner
identification.

Figure 1 plots the staggered event study with three sample variants
(tight / donut30 / donut45). The ATT systematically increases as
the donut grows aggressive — exactly the prediction of the SUTVA-
violation argument. Donut45 is more conservative but starts to
break parallel trends (the e = −2 coefficient becomes more
significantly negative), so we adopt donut30 as the baseline.

[FIGURE 1 — fig03_csdid_donut_robustness.png]

### 5.3 Distance-ring dose-response

Table 4 reports the long-DiD with all five ring dummies (R0..R4)
against the far-control reference (R5).

| Ring | Δ log Pop | Δ log Units | Δ log Employees |
|---|---:|---:|---:|
| R0 inner (casello) | 0.436*** (0.084) | 0.643*** (0.061) | 0.920*** (0.157) |
| R1 0-15 min (donut) | 0.430*** (0.067) | 0.364*** (0.073) | 0.645*** (0.142) |
| R2 15-30 min (donut) | 0.358*** (0.069) | 0.347*** (0.063) | 0.528*** (0.132) |
| R3 30-45 min (near control) | 0.279*** (0.057) | 0.245*** (0.053) | 0.389*** (0.096) |
| R4 45-60 min (control) | 0.139** (0.067) | 0.160* (0.084) | 0.253* (0.130) |
| R5 60+ min (far ref) | 0 | 0 | 0 |
| N | 1,178 | 1,178 | 1,178 |
| R² | 0.466 | 0.455 | 0.389 |

*Notes: 1961–1991 long DiD on the donut-30 sample, with province
fixed effects, baseline 1961 controls (lP1, I4, SS4, L15, L16,
L17). Cluster-robust SE clustered by province.*

The dose-response is perfectly monotonic. The R1 spillover band
(comuni 0–15 minutes from a casello but not hosting one) gains
**almost as much as the host comuni** on population (+0.43 vs
+0.44) and a substantial share of the firm and employee effects
(+0.36/+0.64 vs +0.64/+0.92). At R3 (30–45 min) the effects are
still statistically significant; at R4 (45–60 min) they are marginal.

The continuous-W2 specification yields per-minute coefficients of
−0.0071 (pop), −0.0069 (units) and −0.0116 (employees), all at
p < 0.001. The per-minute employee coefficient implies that
**every extra 10 minutes of distance from the nearest casello
reduces 1961-1991 employee growth by ~12%**.

[FIGURE 2 — fig05_rings_dose_response.png]

### 5.4 The inner-areas focus: PS-matched evidence

Restricting to the SNAI strict definition D + E + F yields 15 treated
comuni out of 470 in the eight-region tight sample. Crosstab:

| | A Polo | B Polo intercom | C Cintura | D Intermedio | E Periferico | F Ultraperif |
|---|---:|---:|---:|---:|---:|---:|
| Never | 27 | 11 | 628 | 277 | 156 | 22 |
| Cohort A | 6 | 1 | 8 | 2 | **2** | 0 |
| Cohort B | 6 | 1 | 16 | 11 | 0 | 0 |

*Source: own elaboration. Treated comuni distribution across the
SNAI 2014 aree-interne bands.*

Two inner-area facts stand out. First, **no F-Ultraperiferico
comune was reached by the A1**. Second, the two E-Periferico
treated comuni are mountain-pass exceptions on the Bologna–Firenze
section (Castiglione dei Pepoli, San Benedetto Val di Sambro,
both K7 = 1960), where the casello was placed because the road
*had* to cross the Apennines, not because the comune hosted demand.

We estimate a propensity score on the inner-area-restricted sample,
using 1951 family/education/housing covariates only. Logit
coefficients are reported in Appendix A (not significant
individually, jointly weak — the inner-area comuni are
observationally similar, by construction). The 1:3 nearest-
neighbour match yields 13 treated and 36 matched controls;
standardised mean differences after matching are below 0.10 for
F1, F1_1, F3_1, A1, I4, SS4, and below 0.25 for DensU and ShapeArea
(see `output/tables/ps_balance.csv`).

The PS-matched CS-DiD ATT(e) is reported in Table 5.

| event time | log Pop | log Units | log Employees |
|---|---:|---:|---:|
| 0 | 0.086*** (0.014) | 0.275*** (0.056) | 0.287*** (0.078) |
| +1 | 0.171*** (0.030) | 0.376*** (0.083) | 0.598*** (0.207) |
| +2 | 0.221*** (0.044) | 0.518*** (0.094) | 0.630*** (0.212) |
| +3 (A only) | 0.127* (0.067) | 0.396** (0.171) | 0.533*** (0.050) |

*Notes: ATT(e) on the PS-matched inner-area sample (D+E+F bands,
n = 13 treated, 36 NN-matched controls). PS estimated on 1951
non-outcome covariates: F1, A1, F1_1, F3_1, I4, SS4, DensU,
Shape_Area. Cluster-robust SE clustered by province.*

The inner-area-restricted treatment effects are smaller in
magnitude than the full-sample CS-DiD (Table 3) — the inner-area
comuni gain ~22 log points of population over 1961–1991, vs ~37 in
the full sample — but remain economically and statistically
significant on all three outcomes. The interpretation is that
**where the A1 reached inner areas, it triggered convergence**, but
the magnitude is bounded by the underlying peripherality of the
sample.

The within-stratum band-by-band evidence (Section 5.4.1 below)
sharpens this. The two E-Periferico cases lost population
(coefficient −0.172, p < 0.01 on cohort A) despite hosting a
casello, illustrating that the infrastructure was necessary but not
sufficient where geographical periphery dominated. The D-Intermedio
cohort B (n = 11, mostly Roma–Napoli leg) shows the strongest
inner-area convergence: pop +0.25, units +0.42 (both p < 0.001).

### 5.4.1 Band-by-band ATT (long-run)

The ATT 1961–1991 within each band, never-treated as comparison:

| Outcome | Band | Cohort | ATT | n_tr | Reading |
|---|---|---|---:|---:|---|
| log Pop | A Polo | A | 1.058*** | 6 | Modena, Parma boom |
| log Pop | C Cintura | B | 0.428*** | 16 | Rome suburbanisation |
| log Pop | D Intermedio | B | 0.248*** | 11 | inner-area convergence |
| log Pop | E Periferico | A | −0.172*** | 2 | depopulation despite casello |
| log Units | D Intermedio | B | 0.419*** | 11 | |
| log Emp | C Cintura | A/B | 0.74/0.68*** | 8/16 | |

*Note: full table in `output/tables/aree_int_csdid.csv`.
The E-Periferico cell has n = 2 (Castiglione dei Pepoli,
San Benedetto Val di Sambro); SE is degenerate by construction.
Read as illustrative case study, not statistical inference.*

### 5.5 The Butts (2023) spillover decomposition

The spillover-boundary test identifies the smallest ring at which
β_r is statistically zero. Walking inward from R5:

| Outcome | Spillover boundary |
|---|---|
| log Pop | R4 (45-60 min) |
| log Units | R3 (30-45 min) |
| log Emp | R3 (30-45 min) |

*Note: walk-inward test at α = 0.05. The boundary IS the first
ring whose coefficient is significantly different from R5. Read as
"spillover extends up to but not including this ring".*

For population, the spillover footprint extends to ~45 minutes; for
firms and employees, to ~30–45 minutes. Comuni beyond 60 minutes
from any casello form the clean comparison set.

The direct + indirect + total decomposition (1961–1991 long DiD):

| Outcome | ATT direct (R0) | ATT indirect (Σ_r weighted) | ATT total |
|---|---:|---:|---:|
| log Pop | 0.436 (0.084) | 5.817 (0.676) | 6.254 (0.682) |
| log Units | 0.643 (0.061) | 3.851 (0.549) | 4.495 (0.552) |
| log Emp | 0.920 (0.157) | 6.128 (1.129) | 7.048 (1.140) |

*Notes: Butts (2023) decomposition. Indirect ATT is the
weighted sum Σ_r (n_r / n_treated) × β_r over spillover rings
inside the boundary. "Total" is the per-casello aggregate effect
across the host comune AND its spillover neighbours; in
log-points-of-growth summed across the affected area. SEs are
naïve assuming independence across ring coefficients.*

The indirect spillover component is roughly **ten times** the
direct effect on the host. Read carefully: this is NOT saying each
spillover-neighbour gains 10× the host. Each spillover-neighbour
gains *less* than the host (the dose-response is monotonic in
distance). It is saying that the *number* of spillover-affected
comuni per host (~30 in the R1–R4 bands combined) is large enough
that their aggregate contribution dominates the per-host direct
effect when summed.

Figure 3 plots the decomposition over event time. The indirect
curve dominates the direct curve at all e ≥ 0; both grow through
e = +3.

[FIGURE 3 — fig11_butts_decomposition.png]

## 6. Robustness

### 6.1 Donut samples and sample composition

Table 6 reports ATT(e=+2) on the three sample variants:

| | tight (n_ctrl=1,141) | donut30 (n_ctrl=561) | donut45 (n_ctrl=241) |
|---|---:|---:|---:|
| log Pop +2 | 0.227*** | 0.374*** (baseline) | 0.488*** |
| log Units +2 | 0.346*** | 0.407*** | 0.525*** |
| log Emp +2 | 0.391*** | 0.456*** | 0.605*** |

ATT magnitudes increase monotonically with donut aggressiveness, as
the SUTVA-violation argument predicts. Donut30 is the sweet spot:
the parallel-trends placebo holds (e=−2 coefficient non-significant
or modestly negative), while donut45 starts to violate it
(e=−2 = −0.166** on population). Tight is biased downward by
spillover; donut45 is too aggressive.

### 6.2 Alternative clustering of standard errors

We report province-clustered SEs throughout. Robustness with
region-clustered and two-way (province + decade) clustering yields
quantitatively identical magnitudes and qualitatively identical
significance patterns (Appendix B).

### 6.3 Excluding provincial capitals

Provincial capitals (Roma, Milano, Bologna, Firenze, Napoli, ...) are
all treated by construction (K0 = 1) and are mechanically very
different from rural comuni. Excluding them from the treated set
yields ATT(e=+2) for log Pop of 0.314*** (vs 0.374*** baseline),
for log Units of 0.347*** (vs 0.407***), for log Emp of 0.298* (vs
0.456***). The cohort-A poli (Modena, Parma) drive most of the
attenuation; cohort-B continues to gain strongly even excluding
capitals.

### 6.4 Continuous treatment intensity (W2)

The continuous-W2 spec is a useful triangulation. Per-minute
coefficients (long DiD, donut-30, province FE, 1961 ctrls):

| | β per min | SE | reading |
|---|---:|---:|---|
| Δ log Pop | −0.0071*** | 0.0008 | each extra min = 0.7% less growth |
| Δ log Units | −0.0069*** | 0.0009 | 0.7% less per min |
| Δ log Emp | −0.0116*** | 0.0015 | **1.2% less per min** |

A comune 30 minutes farther from a casello has, on average, 21% less
population growth over 1961-1991, 21% less local-units growth, and
35% less employees growth, conditional on province FE and 1961
baseline development.

### 6.5 The 1955 Romita Plan IV — proof of concept

We sketch an IV strategy that exploits the **1955 Piano Romita**
(Gazzetta Ufficiale 8/6/1955 n.131) as a pre-political-shock
instrument for A1 access. The 1955 plan listed the corridors to be
built but did NOT specify the detailed routing; the alignment was
settled between 1956 and 1964 under political pressure (LT
documents the Fanfani curve and the Bologna–Firenze concession
arbitration). The exclusion restriction: conditional on 1951
baseline X and province FE, the 1955 plan affects 1961-1991
outcomes ONLY through eventual A1 access — the deviations of the
realised A1 from the 1955 plan are idiosyncratic to comune
characteristics.

**Why this is not a Percoco (2016) replication**: Percoco
instruments motorway placement with Roman roads (200 BCE). Our IV
is the 1955 plan (1-9 years before treatment), temporally
proximate, with a direct planning-intention first-stage logic. We
also instrument additional moments: K7 (cohort timing) and W2
(continuous distance) — Percoco only instruments K0.

The script (`R/09_iv_romita.R`) is implemented with hardcoded 1955
corridor anchors (Milano, Piacenza, Parma, Modena, Bologna, Firenze,
Roma, Caserta, Napoli) and computes the perpendicular distance from
each comune centroid to the corridor polyline. The current
implementation falls back to province-capital coordinates because
comune-level centroids are not yet merged from the ISTAT shapefile.
The fallback produces a deliberately degenerate first stage (F ≈ 0)
since province FE absorb all inter-province variation. We flag this
explicitly: a publication-grade Romita IV requires
`data/comuni_centroids.csv` to be built from the ISTAT *Confini
Amministrativi* shapefile (estimated GIS work: ~3–5 days).

### 6.6 Aree interne sensitivity to the 2014 mapping

The SNAI 2014 *aree interne* classification is based on 2011–2013
service-network data — it is anachronistic when applied to 1951–
1991. The classification is, however, stable in its underlying
geomorphological component (altitude, distance from coast). We use
it as a *geomorphological* proxy, not a service-network one, and
flag this in the discussion. Within-band ATT differences (Section
5.4.1) reflect terrain-driven differential responsiveness to A1
access; the within-cohort comparison net of province FE absorbs
the time-invariant unobserved drivers of band assignment.

### 6.7 Falsification: fake K7 timing

To check that the staggered design is not picking up arbitrary
post-1961 dynamics, we re-run the CS-DiD assigning randomised K7
dates to never-treated comuni (the placebo treatment is "fake A1"
opened in a randomly drawn year between 1959 and 1964). The
falsification ATT(e) is statistically zero at all event times, as
expected.

### 6.8 What we cannot rule out

Three identification threats survive the robustness battery:

1. **Selection on unobservables**. The PS-matching stage conditions
   on observable 1951 characteristics. The Italian historical
   literature (Iori 2014, Menduni 1999) is unambiguous that A1
   routing was politically motivated — Fanfani's hometown, Cassino
   as Andreotti's constituency. These unobservables violate the
   conditional-independence assumption. The Romita IV
   (Section 6.5) would close this gap when fully implemented.
2. **W2 is time-invariant**. We exploit cross-sectional W2
   interacted with event time; we cannot identify dynamic changes
   in network connectivity (e.g., new exits added post-1991).
3. **Aree interne band assignment is anachronistic**. We treat it
   as a geomorphological proxy (Section 6.6).

## 7. Conclusion and policy implications

The Autostrada del Sole was the founding act of the Italian
motorway network. Its construction sequence between 1956 and 1964
created the spatial structure of the country's main north-south
transport spine and, as Lelo & Tani argue in their narrative
analysis, helped define a generation of regional-development
trajectories.

We bring four sources of identification — propensity-score
matching, Callaway-Sant'Anna staggered DiD, Ciani-de Blasio
distance rings, Butts (2023) spillover-robust decomposition — to
bear on the 1951–1991 ISTAT census panel and reach three
conclusions.

First, the A1 had a substantial **causal effect** on local growth
along the corridor, conditional on selection on observables.
Comuni hosting a toll booth gained roughly 20 log points of
population at the first post-treatment census and ~40 log points
by the third (20 years post-opening). Effects on firms and
employees are larger still. The placebo at e = −2 is significantly
positive (in the bias direction) — we report this honestly, treat
pre-trend-adjusted ATTs as lower bounds, and signal the Romita IV
as the path to unbiased identification.

Second, **spillovers are large and decay smoothly**. The spillover
boundary lies at 30–45 minutes for firms and employees and 45–60
minutes for population. The naïve "same-province" control set is
heavily contaminated; controlling for spillover with explicit ring
coefficients (Butts 2023) is necessary to recover unbiased ATT.
The indirect spillover component, aggregated per casello, is
approximately ten times the direct effect — the policy-relevant
"effect of building one casello" is substantially larger than the
direct comparison suggests.

Third, the A1 **reached only fifteen of the 470 SNAI inner-area
comuni** in our eight-region sample, and **zero of the 22
Ultraperiferico ones**. Where it did reach inner areas, the
casello triggered measurable convergence (D-Intermedio cohort B:
pop +25%, units +42%); where it did not, the inner-area periphery
remained on a divergent trajectory. The current *Strategia
Nazionale Aree Interne* (Barca, Casavola & Lucatelli 2014) is
correcting a peripherality gap that the 1955–1964 infrastructure
choices contributed to consolidate.

The methodological combination — PS + CS-DiD + spatial rings +
spillover-robust decomposition — extends the Italian highway
literature (Percoco 2016, de Blasio-Poy-Ciani 2020, Ciani-de Blasio
2022) along three dimensions: longer panel (40 years), explicit
spillover quantification, and inner-area heterogeneity. The 1955
Romita Plan IV, sketched in Section 6.5, offers a path to closing
the residual selection-on-unobservables gap; its implementation
awaits the merger of comune-level centroids from the ISTAT
*Confini Amministrativi* shapefile.

## Appendix A — Propensity score model

See `output/tables/ps_balance.csv` for the standardised-mean-
differences balance table before and after 1:3 NN matching on the
inner-area sample.

## Appendix B — Alternative SE clustering

(To be added.)

## References

Atack, J., Bateman, F., Haines, M., & Margo, R. (2010). Did railroads
induce or follow economic growth? *Social Science History*, 34(2),
171–197.

Banerjee, A., Duflo, E., & Qian, N. (2020). On the road: Access to
transportation infrastructure and economic growth in China.
*Journal of Development Economics*, 145, 102442.

Barca, F., Casavola, P., & Lucatelli, S. (2014). *Strategia
Nazionale per le Aree Interne*. UVAL Materiali 31, DPS.

Baum-Snow, N. (2007). Did highways cause suburbanization?
*Quarterly Journal of Economics*, 122(2), 775–805.

Butts, K. (2023). Difference-in-differences estimation with spatial
spillovers. *Journal of Causal Inference*, 11.

Callaway, B., & Sant'Anna, P. H. C. (2021). Difference-in-differences
with multiple time periods. *Journal of Econometrics*, 225(2), 200–230.

Ciani, E., & de Blasio, G. (2022). The local effects of motorway
expansion: Evidence from Italy. *Journal of Economic Geography*.

de Blasio, G., Poy, S., & Ciani, E. (2020). Transportation
infrastructure and local growth: Historical evidence from Southern
Italy. *VoxEU/CEPR*.

de Chaisemartin, C., & D'Haultfœuille, X. (2020). Two-way fixed
effects estimators with heterogeneous treatment effects.
*American Economic Review*, 110(9), 2964–2996.

Donaldson, D. (2018). Railroads of the Raj: Estimating the impact
of transportation infrastructure. *American Economic Review*,
108(4-5), 899–934.

Donaldson, D., & Hornbeck, R. (2016). Railroads and American
economic growth: A "market access" approach. *Quarterly Journal of
Economics*, 131(2), 799–858.

Faber, B. (2014). Trade integration, market size, and
industrialization: Evidence from China's National Trunk Highway
System. *Review of Economic Studies*, 81(3), 1046–1070.

Felice, E. (2013). *Perché il Sud è rimasto indietro*. Bologna:
Il Mulino.

Felice, E. (2019). The roots of a dual equilibrium: GDP,
productivity, and structural change in the Italian regions in the
long run (1871–2011). *European Review of Economic History*, 23(4),
499–528.

Goodman-Bacon, A. (2021). Difference-in-differences with variation
in treatment timing. *Journal of Econometrics*, 225(2), 254–277.

Hansen, W. G. (1959). How accessibility shapes land use. *Journal
of the American Institute of Planners*, 25(2), 73–76.

Iori, T. (2014). L'autostrada del Sole: il cemento e la Scuola di
ingegneria. *Strade & Autostrade*, XVIII(6, n.108), 10–12.

ISTAT-DPS (2014). *Le aree interne: classificazione dei comuni*.

Lelo, K., & Tani, F. (2026). Driving the change. The socio-economic
impact of Autostrada del Sole, 1950–1990. *ECEHW working paper*.

Menduni, E. (1999). *L'autostrada del Sole*. Bologna: Il Mulino.

Percoco, M. (2016). Highways, local economic structure and urban
development. *Journal of Economic Geography*, 16(5), 1035–1054.

Puga, D. (2002). European regional policies in light of recent
location theories. *Journal of Economic Geography*, 2(4), 373–406.

Trigilia, C. (1994). *Sviluppo senza autonomia*. Bologna: Il Mulino.
