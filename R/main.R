# =====================================================================
# main.R  --  master pipeline
# =====================================================================
#
# Run from project root:
#     setwd("/path/to/Share")
#     source("R/main.R")
#
# What it does:
#   01  load + harmonise the five census xlsx files into a long panel
#   02  flag treated comuni (A1 toll booth) and define control samples
#   03  cluster analysis + intercensal growth (descriptive)
#   04  long DiD (M1) + event study (M2) -- the core spatial DiD
#   05  heterogeneous effects (M3) by macro-area, 1961 cluster, size
#   06  robustness checks (placebo, drop capoluoghi, wide control, SE)
#
# All outputs land in output/tables (CSV + LaTeX) and output/figures
# (PNG + PDF).

dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

source("R/01_prepare_data.R")
source("R/02_treatment.R")
source("R/03_descriptive.R")
source("R/04_spatial_did.R")
source("R/05_heterogeneous.R")
source("R/06_robustness.R")
source("R/07_accessibility_did.R")  # continuous-treatment spatial DiD
source("R/08_staggered_did.R")      # staggered DiD exploiting K7 opening year

message("\n*** Pipeline completed. Outputs in output/tables and output/figures ***")
