# =====================================================================
# 02_treatment.R  --  define treatment and control groups
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# In the Ciani-de Blasio (2022) / de Blasio-Poy-Ciani (2020) design,
# treatment is *spatial*: a unit is treated if it lies within a small
# distance of a motorway access point. Their identification relies on a
# DiD that compares municipalities just inside the treatment ring to
# those just outside, conditional on province fixed effects.
#
# In Lelo & Tani's draft the spatial dimension is collapsed into a
# binary indicator: a comune is treated iff it directly hosts an A1
# toll booth. We follow the same definition here for two reasons:
#
#   (a) it matches the descriptive numbers reported in the draft
#       (n = 47 treated comuni; +27% / +51.9% / +56.4% headline growth
#       differentials), so the empirical results can be cross-checked
#       against what is already written;
#
#   (b) without comune-level centroids we cannot build distance rings.
#       The user can later upgrade `data/treated_comuni.csv` to a
#       full geo file (Shape_Area is in the panel but centroids are
#       not).
#
# We define two control groups:
#
#   * `sample_tight`  -- comuni in the SAME PROVINCE as a treated unit
#                        but not on the A1 themselves. This is the
#                        analogue of Ciani-de Blasio's "donut" control
#                        ring: a comune that shares the local labour
#                        market and local policy environment with a
#                        treated unit but did not get direct access.
#                        ==> THIS IS THE BASELINE SAMPLE.
#
#   * `sample_wide`   -- all comuni in the panel (treated + every other
#                        comune in the 8 regions of the dataset).
#                        Reported only as robustness (R4): the further
#                        the control group from the treated areas the
#                        more confounding factors (regional policies,
#                        morphology) become problematic.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# --- Build treatment indicator from K0/K7 in the raw data --------------
# K0 (binary, 1 if comune hosts an A1 toll booth) and K7 (year the
# booth opened, 1959-1964) are recorded in each year's xlsx, but only
# the 1991 cross-section has them populated (other years are NA).
# Cross-check against the cleaner aree_interne.csv (one row per comune,
# K0/K7 already deduplicated).
ai <- read_csv("data/aree_interne.csv", show_col_types = FALSE)

treat_xs <- ai %>%
  transmute(PRO_COM,
            treat_A1   = ifelse(is.na(K0), 0L, as.integer(K0 > 0)),
            year_open  = K7,           # main opening year (matches Lelo Tab 1)
            year_open2 = K5,           # alternative opening date
            TMP_60     = TMP_60,       # travel-time band to nearest polo
            Aree_Int   = Aree_Int)

panel <- panel %>%
  select(-K0, -K5, -K7) %>%   # drop the per-year copies, keep only treat_xs
  left_join(treat_xs, by = "PRO_COM") %>%
  mutate(treat_A1 = coalesce(treat_A1, 0L))

# Optional override from data/treated_comuni.csv (kept for documentation
# and so a future user can hand-edit the list). Only applied if the
# file exists and is non-empty.
if (file.exists("data/treated_comuni.csv")) {
  override <- read_csv("data/treated_comuni.csv", show_col_types = FALSE)
  message(glue("  (note: data/treated_comuni.csv lists {nrow(override)} ",
               "comuni; K0 in the raw data marks ",
               "{sum(treat_xs$treat_A1)} comuni; using the K0 list as primary.)"))
}

# Provinces that host at least one A1 toll booth (the within-province
# control pool is built from these)
treated_provinces <- panel %>% filter(treat_A1 == 1L) %>%
  distinct(COD_PROV) %>% pull(COD_PROV)

panel <- panel %>%
  mutate(in_A1_prov  = COD_PROV %in% treated_provinces,
         sample_tight = treat_A1 == 1 | (in_A1_prov & treat_A1 == 0),
         sample_wide  = TRUE,
         # Need a comune-level W2 broadcast (W2 in panel is by-year copy;
         # take first non-NA per comune).
         W2 = ifelse(is.na(W2), NA_real_, W2))

# Broadcast W2 = single time-invariant value per PRO_COM
w2_xs <- panel %>% filter(!is.na(W2)) %>%
  distinct(PRO_COM, W2) %>% group_by(PRO_COM) %>%
  summarise(W2 = first(W2), .groups = "drop")
panel <- panel %>% select(-W2) %>% left_join(w2_xs, by = "PRO_COM")

# ---- DONUT samples (Ciani-de Blasio SUTVA fix) -------------------------
# Drop control comuni that are "too close" to a treated casello to be
# clean controls -- they receive spillovers (suburbanisation, indotto).
# We build two donut variants with increasing aggressiveness:
#   * sample_donut30: drop controls with W2 <= 30 minutes
#   * sample_donut45: drop controls with W2 <= 45 minutes (conservative)
#
# Treated comuni (treat_A1 == 1, all with W2 ~ 0-15 min) are kept in
# both variants regardless of their own W2.
panel <- panel %>%
  mutate(
    near_casello30 = !is.na(W2) & W2 <= 30 & treat_A1 == 0,
    near_casello45 = !is.na(W2) & W2 <= 45 & treat_A1 == 0,
    sample_donut30 = sample_tight & !near_casello30,
    sample_donut45 = sample_tight & !near_casello45
  )

# Report sample sizes
n_treat       <- panel %>% filter(year == 1961, treat_A1 == 1) %>% nrow()
n_tight_ctrl  <- panel %>% filter(year == 1961, treat_A1 == 0, in_A1_prov) %>% nrow()
n_wide_ctrl   <- panel %>% filter(year == 1961, treat_A1 == 0) %>% nrow()
n_provinces   <- length(treated_provinces)

n_donut30_ctrl <- panel %>% filter(year == 1961, treat_A1 == 0,
                                    sample_donut30) %>% nrow()
n_donut45_ctrl <- panel %>% filter(year == 1961, treat_A1 == 0,
                                    sample_donut45) %>% nrow()

message(glue(
  "Treated comuni: {n_treat} across {n_provinces} provinces.\n",
  "Tight    control (same prov, no A1, W2 any):   {n_tight_ctrl}.\n",
  "Donut-30 control (same prov, no A1, W2 > 30m): {n_donut30_ctrl}.\n",
  "Donut-45 control (same prov, no A1, W2 > 45m): {n_donut45_ctrl}.\n",
  "Wide     control (all non-treated comuni):     {n_wide_ctrl}."))

saveRDS(panel, "data/panel_long.rds")
