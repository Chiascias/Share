# =====================================================================
# main.R  --  master pipeline (rewritten)
# =====================================================================
#
# Spatial-staggered DiD on Autostrada del Sole (Lelo & Tani, 2026).
#
# 01  data + treatment + samples + cohorts + rings
# 02  descriptive (cluster analysis, growth rates)
# 03  Callaway-Sant'Anna staggered DiD (2 cohorts)
# 04  Ciani-de Blasio distance rings with EXPLICIT spillover bands
# 05  Spatial × staggered combined spec  (novelty)
# 06  Focus on ISTAT aree interne classification (policy angle)
#
# Run from project root:
#   setwd("/path/to/Share")
#   source("R/main.R")

dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

source("R/01_prepare.R")
source("R/02_descriptive.R")
source("R/03_csdid.R")
source("R/04_spatial_rings.R")
source("R/05_spatial_staggered.R")
source("R/06_aree_interne.R")

message("\n*** Pipeline complete. Outputs in output/tables and output/figures ***")
