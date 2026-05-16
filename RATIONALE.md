# Spatial-staggered DiD on the Autostrada del Sole — rationale

R replication code for the empirical part of Lelo & Tani (2026),
*Driving the change. The socio-economic impact of Autostrada del Sole,
1950-1990*.

## What this analysis does

We combine two econometric ideas:

1. **Callaway-Sant'Anna (2021) staggered DiD** — exploits the fact
   that the A1 toll booths opened in two distinct census cohorts:
   * Cohort A (n=19), K7 ∈ {1959, 1960}, first observed post = 1961
   * Cohort B (n=34), K7 ∈ {1962, 1963, 1964}, first observed post = 1971
2. **Ciani-de Blasio (2022) spatial-DiD with explicit spillover bands**
   — uses W2 (driving minutes from each comune to its nearest A1
   casello in the post-1964 final network) to assign comuni into
   concentric rings around treated units. Spillover-contaminated bands
   get explicit coefficients (R1, R2) instead of being donut-ed out.

The combination — **spatial-staggered DiD** — is the methodological
novelty of the paper. It is implemented in `R/05_spatial_staggered.R`
and gives one ATT per (event-time × ring) cell.

## Pipeline

```
R/
  00_setup.R              packages, helpers (regtab, with_cluster, tidy_cr)
  01_prepare.R            data + treatment + samples + cohorts + rings
  02_descriptive.R        cluster analysis + intercensal growth
  03_csdid.R              CS-DiD staggered (cohort A vs B vs Never)
  04_spatial_rings.R      Ciani rings with explicit spillover (R1, R2)
  05_spatial_staggered.R  CS-DiD × rings (the combined novelty)
  06_aree_interne.R       focus on ISTAT inner-areas classification
  main.R                  master runner
```

Outputs land in `output/tables/` and `output/figures/`.

## Treatment and samples

Treatment is K0=1 from the raw data (53 comuni with an A1 toll booth)
plus K7 for the opening year. Travel time W2 (in minutes) is computed
on the post-1964 final A1 network — it is a *cross-sectional* measure
that interacts with the time-series (pre/post) variation in census
years to identify the effect, exactly as in Donaldson-Hornbeck (2016).

Sample variants (all built in `R/01_prepare.R`):

| sample          | description                              | n_ctrl |
|-----------------|------------------------------------------|--------|
| `sample_tight`  | same-province non-treated                | 1,141 |
| `sample_donut30`| `tight` minus controls with W2 ≤ 30 min  | 561 ← **baseline** |
| `sample_donut45`| `tight` minus controls with W2 ≤ 45 min  | 241 |
| `sample_wide`   | all comuni in the 8-region panel         | 3,189 |

## Distance rings

R/01_prepare.R also assigns each comune to one of six concentric
rings based on W2:

| ring | description | n (1961, tight sample) |
|------|---|---:|
| R0 | inner — hosts the casello (treat_A1 = 1) | 53 |
| R1 | 0-15 min (donut 1)         | 159 |
| R2 | 15-30 min (donut 2)        | 421 |
| R3 | 30-45 min (near control)   | 320 |
| R4 | 45-60 min (control)        | 136 |
| **R5** | **60+ min (far control, reference)** | **98** |

R1 and R2 are the **spillover bands**. In `R/04_spatial_rings.R` they
get their own coefficients, so we can READ OFF the spillover size
rather than assuming it away with a donut.

## Headline results

(donut-30 baseline; full numbers in `output/tables/`.)

### CS-DiD staggered event study (`R/03_csdid.R`)

The pooled ATT(e) aggregating across the 2 cohorts, weighted by cohort
size:

| event time | log Pop | log Local units | log Employees |
|---|---|---|---|
| **−2** (B in 1951, placebo) | ≈ 0 ns | ≈ 0 ns | ≈ 0 ns |
| −1 (ref) | 0 | 0 | 0 |
| **0** | +0.18*** | +0.13*** | +0.29*** |
| +1 | +0.33*** | +0.19*** | +0.33*** |
| +2 | +0.35*** | +0.34*** | +0.41*** |
| +3 (A only) | +0.42*** | +0.30** | +0.45*** |

See `fig02_csdid_event_study.png` (pooled), `fig03_csdid_donut_robustness.png`
(across 3 samples), `fig04_csdid_cohort_separated.png` (cohort A vs B
separately — the cleanest visualisation of staggered identification).

### Ring dose-response (`R/04_spatial_rings.R`)

1961-1991 long DiD on rings, far ring (R5) as reference:

| ring | Δ log Pop | Δ log Units | Δ log Emp |
|---|---|---|---|
| R0 inner (casello) | **+0.44*** | **+0.64*** | **+0.92*** |
| R1 0-15 min (spillover) | **+0.43*** | **+0.36*** | **+0.64*** |
| R2 15-30 min (spillover) | **+0.36*** | **+0.35*** | **+0.53*** |
| R3 30-45 min | +0.28*** | +0.25*** | +0.39*** |
| R4 45-60 min | +0.14**  | +0.16*   | +0.25*   |
| R5 60+ min | 0 (ref) | 0 (ref) | 0 (ref) |

Spillover is **massive and decays smoothly**: comuni in R1 (0-15 min
from a casello, but without one of their own) gain almost as much as
inner-ring comuni on population (+0.43 vs +0.44). The "near control"
bands R3 and R4 still gain substantially. ONLY R5 (>60 min) looks
like a clean untreated baseline.

This is the textbook reason for the donut: in our data, calling R1-R4
"controls" mechanically underestimates the casello effect.

Continuous W2 per-minute coefficients (same wide regression):

| outcome | β per minute of W2 | reading |
|---|---|---|
| Δ log Pop      | **-0.0071*** | each extra minute = 0.7% less growth |
| Δ log Local units | **-0.0069*** | -0.7% per minute |
| Δ log Employees | **-0.0116*** | **-1.2% per minute** |

### Spatial × staggered (`R/05_spatial_staggered.R`)

The combined surface — ATT for each ring × calendar year, with the R5
ring used as control. See `fig06_spatial_staggered.png`.

### Aree interne focus (`R/06_aree_interne.R`)

Crosstab of cohort × aree-interne band at 1991 (n in each cell):

| | A Polo | B Polo intercom | C Cintura | D Intermedio | E Periferico | F Ultraperif |
|---|---|---|---|---|---|---|
| Never | 27 | 11 | 628 | 277 | 233 | 47 |
| Cohort A | 6 | 1 | 8 | 2 | 0 | 0 |
| Cohort B | 6 | 1 | 16 | 11 | 0 | 0 |

**No A1 casello was placed in a Periferico or Ultraperiferico
comune** — the network was built across A/B/C/D bands only. This is
itself a finding: the *Autostrada del Sole spatially bypassed the
inner-area periphery*. Whatever convergence the motorway delivered, it
went to comuni that were *already* close to a service hub.

Long-run ATT (1991 vs each cohort's pre-census) by band:

The figure `fig07_aree_interne_csdid.png` shows the band-by-band
effects. The pattern in our (very small) treated cells is consistent
with Puga (2002) polarisation: gains are concentrated in A/B/C bands,
near-zero in D, undefined in E/F because no comune was treated there.

## How much does this innovate vs Ciani-de Blasio?

|  | Ciani-de Blasio 2020-2022 | This paper |
|---|---|---|
| Treatment cohorts | 1 | **2 (CS-DiD)** |
| Identification | DiD pre/post | **Callaway-Sant'Anna ATT(g,t)** |
| Time horizon | 10-15 yrs | **40 yrs (1951-1991)** |
| Spatial design | distance rings | **rings + continuous + aree-interne crossing** |
| Spillover | donut out | **explicit coefficients on R1, R2** |
| Setting | A3 / broadband / single events | **A1 = founding of the Italian motorway spine** |

## Literature gaps we fill

1. **Methodological**: spatial-staggered DiD applications are still
   rare. The combination of CS-DiD with explicit ring spillovers (vs
   donut) follows Butts (2023) and Borusyak-Hull (2024) but few
   applications exist outside US contexts.

2. **Historical Italian empirical**: the A1 motorway has been studied
   narratively (Menduni 1999, Iori 2014, Maggi 2009) but never with a
   modern long-run causal design. de Blasio-Poy-Ciani (2020) study A3
   in the South; Percoco (2016) is cross-sectional. **We fill the gap
   of a long-run, granular, causal study of the founding event of the
   Italian motorway network.**

3. **Policy-aree interne**: the SNAI 2014 takes today's aree-interne
   hierarchy as given. **No one has asked whether and how post-war
   infrastructure CREATED the hierarchy.** We show that the A1 was
   never routed through Periferico/Ultraperiferico comuni, so it
   could only reinforce — not correct — the pre-existing peripherality
   of the inner areas. SNAI 2014 is partly cleaning up an effect that
   started 60 years earlier.

## Caveats

* W2 measures distance on the *post-1964* network; for 1951-1961
  outcomes it is a counterfactual measure (what would the distance
  have been if the network already existed). This is the
  Donaldson-Hornbeck (2016) market-access approach and the standard
  in the literature; it does NOT invalidate the staggered
  identification because the event-time variation comes from K7, not
  from W2.
* No GIS centroids → we cannot do Conley spatial-HAC standard errors
  yet. Provincial clustering at COD_PROV is the second-best.
* The IV strategy à la Percoco (2016) using Roman roads, or
  Banerjee-Duflo-Qian (2020) using least-cost-paths, would close the
  remaining endogeneity gap. Out of scope here.
