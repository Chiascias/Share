# Building the spine. A spatial-staggered DiD of the Autostrada del Sole on Italian local development, 1951-1991

Research proposal — empirical companion to Lelo & Tani (2026, ECEHW).

---

## 1. Research questions

1. **RQ1 (causal effect)**. Did direct A1 access cause higher
   demographic and economic growth in the 53 toll-booth comuni, net
   of pre-treatment selection on observables and of spatial spillovers
   to nearby controls?
2. **RQ2 (distance decay)**. How does the effect decay with driving
   minutes to the nearest casello? Where does the spillover band end
   and the "true control" begin?
3. **RQ3 (inner-area heterogeneity)**. Did the A1 reach Italy's
   *aree interne* (SNAI 2014 bands D, E, F)? Where it did, did it
   trigger convergence with poli/cintura, or did the casello prove
   insufficient to overcome geographical peripherality?
4. **RQ4 (sectoral heterogeneity)**. Was the effect concentrated in
   manufacturing, services, or agriculture? Did the A1 accelerate
   structural transformation or reinforce existing specialisation?

## 2. Contribution to the literature

| stream | benchmark | what we add |
|---|---|---|
| Italian highway causal studies | de Blasio-Poy-Ciani 2020 (A3 Salerno-RC, single cohort, 10-yr horizon) | A1 = **founding** infrastructure, **2 cohorts**, **40-yr** panel, **PS-matched** controls |
| Italian highway cross-section | Percoco 2016 (national, IV with Roman roads, single census) | **CS-DiD** on cohort-time variation, decennial dynamics, distance rings with **explicit spillover bands** |
| Spatial DiD methodology | Ciani-de Blasio 2022 distance rings | Crossed with **Callaway-Sant'Anna 2021 staggered DiD** and **PS matching** at the design stage (CS-DiD × spatial rings × PS) |
| Long-run infrastructure effects | Donaldson 2018 (Indian rail), Atack et al. 2010 (US rail), Banerjee-Duflo-Qian 2020 (Chinese roads) | First **Italian post-war** application; first to use **W2 time-invariant minutes** as continuous treatment intensity (Donaldson-Hornbeck market access style) on a granular decennial panel |
| Italian inner-areas / SNAI literature | Trigilia 1994, Felice 2013/2019, Barca-Casavola-Lucatelli 2014 SNAI | First quantitative test of whether **post-war motorway routing CONTRIBUTED to today's aree-interne gap** |
| Italian motorway historical | Menduni 1999, Iori 2014, Maggi 2009 | We rely on these for institutional context but **bring causal-econometric evidence** they do not have |

The methodological novelty is a **combination**, not an invention:
PS + CS-DiD + spatial-rings each exist separately. The contribution
is bringing them together on a historically-important Italian case
that is otherwise treated narratively. The substantive contribution
— the **aree interne reading** — is original.

## 3. Identification strategy

Four-stage stacked design.

### Stage 1 — Propensity score

Logit `treat_A1 ~ X_1951` with X_1951 covering ONLY non-outcome
1951 features:

* **Family composition**: F1 (mean family size), A1 (alt fam ratio),
  F1_1, F3_1 (other family-type indicators)
* **Education**: I4 (illiterate share), SS4 (graduate share)
* **Housing / density**: DensU (dwelling density), Shape_Area
* (optional) Macro-area dummy, distance to nearest provincial capital

Variables *excluded* on principle: P1, UT, AT, lP1, lUT, lAT, L15,
L16, L17 — these would condition on outcome or industrial structure
correlated with future growth.

1:3 nearest-neighbour matching with replacement, caliper 0.25 of SD
of linear PS, common-support trim at [.01, .99]. Balance diagnostics
on standardised mean differences and KS test per variable.

### Stage 2 — Callaway-Sant'Anna staggered DiD

The K7 opening year defines two census cohorts:
* **Cohort A** (K7 ∈ 1959-60, n=19, first observed post = 1961)
* **Cohort B** (K7 ∈ 1962-64, n=34, first observed post = 1971)

For each (cohort, post-year) cell we compute
`ATT(g, t) = E[Δy | G=g] − E[Δy | G=Never]` on the **PS-matched
sample**, with frequency weights from MatchIt and cluster-robust SE
at province level. Aggregate to ATT(e) by cohort-size-weighted
averaging. Pre-trends test at e = −2 (cohort B in 1951).

### Stage 3 — Spatial component (Ciani-de Blasio distance rings)

Six concentric rings on W2 (drive-time to nearest casello in the
post-1964 final network):

| ring | W2 minutes | interpretation |
|---|---|---|
| R0 | hosts casello | direct treatment |
| R1 | 0-15 | spillover band 1 (close suburban) |
| R2 | 15-30 | spillover band 2 |
| R3 | 30-45 | near control |
| R4 | 45-60 | control |
| R5 | 60+ | far control (reference) |

R1 and R2 carry **explicit coefficients** rather than being donut-ed
out — their size *measures* the spillover. Continuous-W2 spec
(Donaldson-Hornbeck market access) reported in parallel.

### Stage 4 — Heterogeneity

Two stratifications interacted with treatment:

* **(H1) Aree interne**: Polo (A+B) vs Cintura (C) vs Aree interne
  (D+E+F). Within-stratum CS-DiD plus pooled spec with
  `treat × aree_int_band` interactions.
* **(H2) Sectoral**: outcomes broken into U2-U9 / A2_1-A9_1 sector
  buckets (manufacturing, construction, trade, services, agriculture,
  finance...). Identifies whether A1 access triggered manufacturing
  relocation, services expansion (Cintura tertiarisation), or
  agricultural decline.

## 4. Data

* **ISTAT Ottomila census** 1951, 1961, 1971, 1981, 1991 — 3,242 comuni
  in 8 regions of the A1 corridor (Piemonte, Lombardia, Veneto,
  Emilia-Romagna, Toscana, Umbria, Lazio, Campania).
* **K0 / K5 / K7 / W2** from the project's hand-built A1 casello
  database (cross-checked against Menduni 1999 Table 1 and Autostrade
  per l'Italia archive).
* **Aree interne classification** from ISTAT-DPS 2014 (mapped to
  2011 comune codes, harmonised back via PRO_COM).

## 5. Robustness battery

1. **Multiple PS specifications**: drop/add covariates; logit vs probit;
   re-estimate with alternative samples (tight, donut30, donut45).
2. **Multiple control groups**: never-treated, never+not-yet-treated,
   wide (cross-region).
3. **Multiple clustering**: province, region, two-way province × decade.
4. **Placebo**: 1951-1961 first differences (pre-A1) — coefficient
   should be ≈0; already pre-tested in current pipeline.
5. **Honest 2-cohort staggered**: verify Goodman-Bacon decomposition
   weights aren't negative (already small risk with 2 cohorts and
   never-treated reference; Callaway-Sant'Anna estimator avoids the
   issue by construction).
6. **Falsification**: assign synthetic K7 dates (e.g., a "fake A1"
   built in 1979) and verify no effect.
7. **IV layer (if time permits)**: Roman-road distance à la Percoco
   2016 or least-cost-path à la Banerjee-Duflo-Qian 2020 as
   instrument for K0=1. Would close the selection-on-unobservables
   gap. This is the canonical robustness in the literature; without
   it our claim remains "causal conditional on observables".

## 6. Tables and figures we expect

* Tab 1: descriptive statistics by treatment and aree-interne band
* Tab 2: PS balance before/after matching (full sample + aree interne)
* Tab 3: CS-DiD ATT(g,t) pooled + cohort-separated (donut30 baseline)
* Tab 4: Distance-ring DiD with spillover coefficients (R1, R2)
* Tab 5: Heterogeneity by aree-interne band and by sector
* Fig 1: Event study (TWFE staggered) — main result
* Fig 2: Cohort-separated ATT(g,t) trajectories — staggered visualisation
* Fig 3: Distance-ring dose-response
* Fig 4: ATT by aree-interne band — policy figure
* Fig 5: Map of treated vs control comuni colour-coded by ATT
  (requires shapefile)

## 7. Outlets

Tier 1 (ambitious): *Explorations in Economic History*, *Journal of
Regional Science*, *Regional Science and Urban Economics*.

Tier 2 (realistic): *Regional Studies*, *Journal of Economic
Geography*, *Italian Economic Journal*, *Rivista di Economia e
Statistica del Territorio*.

Workshop circuit: ECEHW 2026 (already targeted), AISRe, SIE, SIDE.

---

## 8. CRITICAL FEASIBILITY ASSESSMENT

This section is intentionally pessimistic. The proposal above is
defensible but each of its claims has a soft spot.

### 8.1 Methodological novelty — modest, not revolutionary

The "PS + CS-DiD + spatial rings" combination is a careful empirical
strategy but **none of the three components is new**:

* **PS-matching DiD** is textbook since Heckman-Ichimura-Todd 1997.
* **CS-DiD** is Callaway-Sant'Anna 2021, already 5 years old.
* **Ciani-de Blasio rings** is Ciani-de Blasio 2022.

A referee at *EEH* or *JRS* will ask: *what specifically is the
methodological innovation*? The honest answer is "the combination on
an Italian case", which is **applied novelty**, not methodological
novelty. We should not over-sell.

A genuinely methodological contribution would require, e.g., a new
estimator for spillover-robust ATT(g,t) (Butts 2023, Borusyak-Hull
2024 territory). We are not doing that.

### 8.2 Small treated sample in the inner-areas focus

* SNAI strict (D + E + F): **15 treated** in the 8-region panel.
* PS-trimmed: **13 treated** with 36 matched controls.
* F-Ultraperiferico: **0 treated** — the most policy-relevant
  category cannot be analysed AT ALL with this data.

The inner-area part of the story is THE most original substantive
angle, but the sample is at the edge of what is defensible. Any
referee will ask whether the ATT(e=+2) = +0.22 on log Pop in the
matched sample (CI: 0.11-0.31) generalises beyond these 13 specific
comuni. **It does not**. They are mostly the Roma-Napoli
Apennine-foothill leg (Anagni, Colleferro, Frosinone hinterland,
Cassino plain) and 2 BO-FI mountain passes.

Mitigations:
- Frame the inner-area finding as **case-based qualitative** within
  a broader quantitative analysis.
- Add hand-drawn case studies for the 2 E-Periferico mountain comuni
  (Castiglione dei Pepoli, San Benedetto Val di Sambro) — both lost
  population despite the casello, a striking counterpoint.
- Triangulate with sector breakdowns and continuous-W2 within-aree-
  interne specs.

### 8.3 PS does not fix selection on unobservables

The 1951 covariate set (family / educ / housing) is reasonable but
**conspicuously omits**:

* **Local political connections** — the literature (Iori 2014,
  Menduni 1999) is unambiguous that A1 routing was political
  (the "Fanfani curve" near Arezzo; Cassino on Andreotti's home
  patch; Bologna-Firenze concession arbitration). These are
  *exactly* the kind of unobservables that violate ignorability.
* **Pre-A1 industrial dynamism** — local Marshallian-district seeds
  that planners may have anticipated.
* **Geomorphology** — proxied weakly by aree-interne band but not
  measured precisely. Mountain passes vs flat plains have different
  baseline trajectories independent of A1.

A referee will rightly say: "your PS conditions on observables but
the A1 location decision was made on unobservables". Without an IV
the causal claim is **conditional on observables**, which is a much
weaker claim than the standard reads "A1 caused X% growth".

**Mitigation**: be honest. Use phrases like "conditional ATT",
"reduced-form association", "consistent with a causal effect under
the conditional-independence assumption". Don't write "A1 caused".

The IV strategy (Roman roads à la Percoco 2016) would close this gap
**substantially**. It is sketched in Section 5 as a robustness but
should arguably be **the main estimator** rather than a footnote.

### 8.4 W2 is post-1964 and time-invariant

Two issues:

* W2 measures the geometry of the **final network** (after 1964).
  Pre-1964 it is a counterfactual measure. This is fine for
  identification under the staggered design (event-time comes from
  K7, not W2) but **awkward narratively**: we cannot say "comuni
  *became* closer to the network", we can only say "comuni *would
  be* closer in the planned/final network".
* Time-invariance means no within-comune variation in W2 across
  censuses → no panel-FE identification of W2 effects.

A reviewer who likes Donaldson-Hornbeck 2016 will accept this. A
reviewer who likes Faber 2014 may push for distance-to-the-network-
*as-of-each-census* which we cannot easily build.

### 8.5 Aree interne classification is anachronistic

The ISTAT-DPS 2014 *aree interne* classification is based on:
* presence of a hospital with first-aid unit,
* secondary schools (all three branches: lyceum, technical, professional),
* a railway station of at least "Silver" class.

These criteria are **defined on 2011-2013 services**, not the 1950s
service network. Mapping the classification back to 1951-1991 is
**logically inconsistent**: a comune classified "Periferico" today
may have been "Cintura" in 1961 (or vice versa).

The classification correlates strongly with **time-invariant
geomorphology** (altitude, mountainousness, distance from coast),
which is what we are *implicitly* using. We should make this
explicit:

> "We use the 2014 aree-interne classification as a stable
> geomorphological proxy. The classification is anachronistic relative
> to our outcome window but stable in its underlying geography. Our
> identifying assumption is that the *terrain* — not the *service
> provision* — drives the classification."

### 8.6 Decennial census granularity

* Only 2 census cohorts ⇒ Goodman-Bacon decomposition has limited
  bite; we cannot weigh forbidden 2x2 comparisons because there are
  too few of them. Callaway-Sant'Anna with never-treated as
  comparison sidesteps this but at the cost of throwing away the
  late cohort × early calendar variation.
* Cohort-specific dynamics within the 1959-1964 window (which the
  data DO record via K7) cannot be exploited — the census collapses
  them to "treated by 1961" vs "treated by 1971".
* Effects within the first decade post-opening cannot be
  disentangled from long-run dynamics.

This is a **hard constraint of the data** — not fixable with this
panel. Annual labour-market data (e.g., INPS micro-data, which exist
from 1974) would be needed.

### 8.7 8-region sample is non-random

The panel covers only the regions crossed by A1 plus 2 adjacent
(Piemonte, Veneto). Far-northern and far-southern Italy (Friuli,
Sardegna, Sicilia, Calabria, Basilicata, Puglia, Liguria, Marche,
Abruzzo, Molise) are entirely absent. This is fine for the
within-A1-corridor identification but limits any "national" claim.

Lelo & Tani's draft does not push a national claim, so this is
fine. But a Tier 1 referee may ask why we don't extend to A2/A3/
A4/A14 to make the story national.

### 8.8 Sector heterogeneity is harder than it looks

U2-U9 and A2_1-A9_1 use the 1971-style 9-sector classification.
ISTAT changed sector codes across censuses (e.g., service-sector
splits in 1981, financial-sector reorganisation in 1991). To compare
**within-sector** growth across censuses we need a **harmonised
crosswalk**. This is non-trivial — maybe a month of careful
classification work.

If we skip the sector breakdown, RQ4 disappears. If we keep it, we
need to flag the harmonisation caveats explicitly in the paper.

### 8.9 Practical scope — what's actually doable in 6 months?

Conservative scope (achievable):
* Stages 1-3 (PS + CS-DiD + rings) on full sample and on aree-interne
* Heterogeneity H1 (aree interne) — already implemented
* Standard robustness battery (multi-PS, multi-control, placebo)
* Workshop submission + working-paper version

Stretch scope (likely needs +6 months):
* Sectoral heterogeneity H2 (sector crosswalk + interpretation)
* IV layer (Roman roads — need road shapefile from De Benedictis et al.)
* Conley spatial-HAC SE (need comune centroids)
* Map figure (need ISTAT comune shapefile)

Out of scope:
* Annual-data extensions (INPS, Camera di Commercio)
* Welfare-quantification (consumer surplus from accessibility)
* Spatial-equilibrium calibration (Allen-Arkolakis style)

### 8.10 Honest sales pitch

* **Strong**: well-identified within-province design; long horizon
  (40 yrs); novel inner-areas policy angle; clean integration with
  Lelo & Tani's historical narrative.
* **Medium**: methodological combination is sound but not novel;
  results consistent with the broader Italian literature (de Blasio
  et al. 2020, Percoco 2016).
* **Weak**: selection on unobservables (no IV); small inner-areas
  sample; anachronistic SNAI mapping.

The paper is **publishable in Tier 2 outlets as-is** (Regional
Studies, IEJ, REST) and **competitive at Tier 1 only with the IV
extension and a tighter aree-interne narrative**.

---

## 9. Suggested next steps (priority order)

1. **Already done in the current pipeline** (R/01..R/08): PS,
   CS-DiD, rings, inner-area focus. Ready for write-up.
2. **Add sector heterogeneity** using a careful 9-sector crosswalk
   (1-2 months work).
3. **Add IV layer** using De Benedictis-Licio-Pinna 2022 Roman-road
   shapefile (2 months data work + estimation).
4. **Add Conley SE** once comune centroids are merged from ISTAT
   shapefile (1 month).
5. **Write the paper** following the structure of de Blasio-Poy-
   Ciani 2020 + Lelo's existing draft sections (3-4 months).

Realistic timeline to working paper: **6-9 months**.
Realistic timeline to journal submission: **12-15 months**.
