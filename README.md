# Autostrada del Sole — Spatial DiD analysis (Lelo & Tani, 2026)

R replication code for the empirical part of Lelo & Tani, *Driving the change.
The socio-economic impact of Autostrada del sole, 1950–1990*. The strategy
adapts the within-province difference-in-differences design used by
**Ciani & de Blasio** (de Blasio, Poy & Ciani 2020; Percoco 2016 for highway
toll booths in Italy) to the A1 Milano–Napoli corridor.

## Data

`archivio/` contains the ISTAT *Ottomila* census panels for the comuni
covered by the A1 corridor regions (Piemonte, Lombardia, Veneto,
Emilia-Romagna, Toscana, Umbria, Lazio, Campania), 1951, 1961, 1971,
1981, 1991. One xlsx per year, sheet `comuni_<YY>`, n = 3,242 comuni.

Variable codes (prefix `<YY>_`):

| code | meaning                                            |
|------|----------------------------------------------------|
| P1   | total resident population                          |
| F1   | total families                                     |
| A1   | average family size (P1/F1 × 100)                  |
| I4   | share of illiterates                               |
| SS4  | share of high-school/graduate population (6+)      |
| L15  | share of employment in agriculture                 |
| L16  | share of employment in industry                    |
| L17  | share of employment in tertiary (excl. trade)      |
| UT   | total local units (firms/establishments)           |
| U2…U9| local units by sector                              |
| AT   | total employees                                    |
| A2_1…A9_1 | employees by sector                           |

`data/treated_comuni.csv` is a template listing the 47 A1 toll-booth comuni
(treated). The list is built from Menduni (1999) plus the official
*Autostrade* gazette and must be cross-checked against the authors'
internal database before publication.

## Empirical strategy

We follow the within-province DiD design that the authors themselves
sketch in the draft (Section *Data and methods*) and that is standard in
the Italian highway literature:

1. **Treatment**: comune *i* hosts an A1 toll booth (`treat_A1 = 1`).
2. **Control – tight**: comune in the *same province* as a treated unit,
   no A1 toll booth, no other motorway access either.
3. **Control – wide**: comune in the same province, no A1 access (may
   have other motorways) — used for robustness.
4. **Pre/post**: A1 is built between 1956 and 1964. We use **1961 as the
   pre-treatment year** and **1971/1981/1991** as post. 1951 provides a
   placebo / pre-trends check.

Outcomes (in logs, except shares):

* `log(P1)`        — population
* `log(UT)`        — local units
* `log(AT)`        — employees
* `AT / P1`        — employment rate
* `L15`, `L16`, `L17`  — sector employment shares

Specifications:

* **(M1) Cross-section long DiD** — first differences 1961 → 1991, OLS
  with province FE and pre-treatment controls (1961 levels).
* **(M2) Event study** — comune & year FE on the panel 1951–1991, log
  outcomes interacted with `treat × year`. Coefficient on
  `treat × 1951` is the **pre-trends placebo**.
* **(M3) Heterogeneous effects** by macro-area (North = Piemonte,
  Lombardia, Emilia-Romagna; Centre = Toscana, Umbria, Lazio;
  South = Campania) and by initial development quartile
  (cluster from `R/03_descriptive.R`).
* **(M4) Conley spatial-HAC standard errors** as robustness, to allow
  for spatial correlation across nearby comuni (requires comune
  centroids — provide via `data/comuni_centroids.csv`; not shipped).

## How to run

```r
# inside R
source("R/main.R")
```

`main.R` calls, in order:

```
R/01_prepare_data.R   # build long panel
R/02_treatment.R      # merge in treated_comuni.csv
R/03_descriptive.R    # cluster analysis + tables of growth rates
R/04_spatial_did.R    # M1 long DiD + M2 event study
R/05_heterogeneous.R  # M3 heterogeneity
R/06_robustness.R     # M4 Conley SE + placebos
```

Outputs land in `output/tables/` (LaTeX + CSV) and `output/figures/`
(PNG + PDF).

## Required R packages

```
tidyverse, readxl, fixest, modelsummary, broom, sandwich, lmtest,
sf (optional, for centroids), ggplot2, scales, kableExtra, conleyreg
```

Install once:

```r
install.packages(c("tidyverse","readxl","fixest","modelsummary",
                   "broom","sandwich","lmtest","ggplot2","scales",
                   "kableExtra"))
# conleyreg only if you have centroids
# install.packages("conleyreg")
```

## Identification caveats (already noted in the draft)

The location of A1 exits is not random — it follows pre-existing
demographic and economic potential. Within-province DiD absorbs the
common time-invariant provincial component; pre-trends on 1951–1961
check the **parallel-trends assumption**. For full causal
identification an IV strategy along Percoco (2016) (Roman roads) or
Banerjee–Duflo–Qian (2020) (least-cost paths between historical hubs)
would be needed and is left for future work.
