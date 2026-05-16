# =====================================================================
# 01_prepare.R  --  build long panel + treatment + samples
# =====================================================================
#
# Consolidated data preparation. Outputs `data/panel_long.rds` with:
#
#   * decennial census panel 1951-1991 for 3,242 comuni (ISTAT
#     Ottomila), 8 regions of the A1 corridor.
#   * outcomes:   P1 (pop), UT (firms), AT (employees), L15/L16/L17
#                 (sector emp. shares), I4 (illiterates), SS4 (edu)
#   * treatment:  K0=1 hosts an A1 toll booth (53 comuni); K7 = the
#                 year that booth opened (1959/60/62/64)
#   * spatial:    W2 = driving time in minutes from each comune to
#                 its nearest A1 toll booth in the post-1964 final
#                 network (time-invariant cross-section)
#   * aree int.:  TMP_60 (30-/60-/60+), Aree_Int (A/B/C/D/E/F)
#
# Samples produced:
#   sample_tight    : same-province controls (no donut)
#   sample_donut30  : drops controls with W2 <= 30 min  (preferred)
#   sample_donut45  : drops controls with W2 <= 45 min  (conservative)
#
# Cohort definition (the 2 census cohorts we use everywhere):
#   cohort_A : K7 in 1959-1960, n=19  --  first observed post = 1961
#   cohort_B : K7 in 1962-1964, n=34  --  first observed post = 1971
#   never    : K0 = 0

source("R/00_setup.R")

# ---------- 1. Read 5 census xlsx ------------------------------------
read_census <- function(year) {
  yy   <- substr(as.character(year), 3, 4)
  raw  <- read_excel(file.path("archivio", paste0(year, ".xlsx")),
                     sheet = paste0("comuni_", yy))
  keep <- c(P1 = paste0(yy,"_P1"),  F1 = paste0(yy,"_F1"),
            I4 = paste0(yy,"_I4"),  SS4= paste0(yy,"_SS4"),
            L15= paste0(yy,"_L15"), L16= paste0(yy,"_L16"),
            L17= paste0(yy,"_L17"), UT = paste0(yy,"_UT"),
            AT = paste0(yy,"_AT"),
            K0 = paste0(yy,"_K0"),  K5 = paste0(yy,"_K5"),
            K7 = paste0(yy,"_K7"),  W2 = paste0(yy,"_W2"))
  out <- raw %>%
    select(COD_RIP, COD_REG, COD_PROV, PRO_COM, COMUNE, Shape_Area,
           any_of(unname(keep))) %>%
    rename(any_of(keep))
  num_cols <- intersect(c("P1","F1","I4","SS4","L15","L16","L17","UT","AT",
                          "K0","K5","K7","W2"), names(out))
  out[num_cols] <- lapply(out[num_cols], function(x) suppressWarnings(as.numeric(x)))
  out$year <- year
  out
}
panel <- bind_rows(lapply(CENSUS_YEARS, read_census))

# Anchor identifiers on 1961 (Lodi province didn't exist before 1992)
ids_1961 <- panel %>% filter(year == 1961) %>%
  select(PRO_COM, COD_RIP_61 = COD_RIP, COD_REG_61 = COD_REG,
         COD_PROV_61 = COD_PROV, COMUNE_61 = COMUNE)
panel <- panel %>%
  left_join(ids_1961, by = "PRO_COM") %>%
  mutate(COD_REG  = coalesce(COD_REG_61,  COD_REG),
         COD_PROV = coalesce(COD_PROV_61, COD_PROV),
         COMUNE   = coalesce(COMUNE_61,   COMUNE),
         macro    = macro_area(COD_REG),
         lP1 = log1p(P1), lUT = log1p(UT), lAT = log1p(AT),
         emp_rate = ifelse(P1 > 0, AT / P1, NA_real_)) %>%
  select(-ends_with("_61"))

# ---------- 2. Bring K0/K7/W2 from aree-interne file -----------------
# The clean cross-section is in data/aree_interne.csv (one row per
# comune). The per-year copies in the xlsx are partly NA; we replace
# them with the broadcast cross-section so they're consistent.
ai <- read_csv("data/aree_interne.csv", show_col_types = FALSE) %>%
  transmute(PRO_COM,
            treat_A1  = ifelse(is.na(K0), 0L, as.integer(K0 > 0)),
            K7_open   = K7,         # main opening year
            K5_open   = K5,         # alt opening date
            W2,                     # min drive to nearest A1 casello
            TMP_60, Aree_Int)
panel <- panel %>%
  select(-K0, -K5, -K7, -W2) %>%
  left_join(ai, by = "PRO_COM") %>%
  mutate(treat_A1 = coalesce(treat_A1, 0L))

# ---------- 3. Cohort assignment (2 census cohorts) ------------------
panel <- panel %>%
  mutate(
    cohort = case_when(
      K7_open %in% c(1959, 1960)           ~ "A",      # treated by 1961
      K7_open %in% c(1962, 1963, 1964)     ~ "B",      # treated by 1971
      TRUE                                  ~ "Never"),
    cohort = factor(cohort, levels = c("Never","A","B")),
    first_post = case_when(cohort == "A" ~ 1961,
                           cohort == "B" ~ 1971,
                           TRUE          ~ NA_real_),
    # event time in 10-year units, relative to each cohort's first post
    e = ifelse(!is.na(first_post), (year - first_post) / 10, NA_real_)
  )

# ---------- 4. Sample definitions ------------------------------------
treated_provinces <- panel %>% filter(treat_A1 == 1) %>%
  distinct(COD_PROV) %>% pull(COD_PROV)
panel <- panel %>%
  mutate(in_A1_prov     = COD_PROV %in% treated_provinces,
         sample_tight   = treat_A1 == 1 | (in_A1_prov & treat_A1 == 0),
         sample_donut30 = sample_tight & !(treat_A1 == 0 & W2 <= 30 & !is.na(W2)),
         sample_donut45 = sample_tight & !(treat_A1 == 0 & W2 <= 45 & !is.na(W2)),
         sample_wide    = TRUE)

# ---------- 5. Distance rings (à la Ciani-de Blasio) -----------------
# Continuous W2 in 5 rings: inner / donut / control / far / very-far.
# We use rings based on absolute minutes to make them interpretable.
panel <- panel %>%
  mutate(
    ring = case_when(
      treat_A1 == 1                          ~ "R0 inner (casello)",
      W2 > 0  & W2 <= 15                     ~ "R1 0-15 min (donut)",
      W2 > 15 & W2 <= 30                     ~ "R2 15-30 min (donut)",
      W2 > 30 & W2 <= 45                     ~ "R3 30-45 min (near control)",
      W2 > 45 & W2 <= 60                     ~ "R4 45-60 min (control)",
      W2 > 60                                ~ "R5 60+ min (far control)",
      TRUE                                    ~ NA_character_),
    ring = factor(ring,
                  levels = c("R5 60+ min (far control)",     # ref = far
                             "R4 45-60 min (control)",
                             "R3 30-45 min (near control)",
                             "R2 15-30 min (donut)",
                             "R1 0-15 min (donut)",
                             "R0 inner (casello)")))

# ---------- 6. Summary ----------------------------------------------
n_treat       <- panel %>% filter(year == 1961, treat_A1 == 1) %>% nrow()
n_provinces   <- length(treated_provinces)
n_tight_ctrl  <- panel %>% filter(year == 1961, treat_A1 == 0, in_A1_prov) %>% nrow()
n_d30_ctrl    <- panel %>% filter(year == 1961, treat_A1 == 0, sample_donut30) %>% nrow()
n_d45_ctrl    <- panel %>% filter(year == 1961, treat_A1 == 0, sample_donut45) %>% nrow()
n_A <- panel %>% filter(year == 1961, cohort == "A") %>% nrow()
n_B <- panel %>% filter(year == 1961, cohort == "B") %>% nrow()

message(glue(
  "Panel: {nrow(panel)} obs, ",
  "{length(unique(panel$PRO_COM))} comuni, ",
  "{length(CENSUS_YEARS)} years.\n",
  "Treated (K0=1):                 {n_treat}  (cohort A={n_A}, B={n_B})\n",
  "Tight    control (same prov):   {n_tight_ctrl}\n",
  "Donut-30 control (W2>30 min):   {n_d30_ctrl}\n",
  "Donut-45 control (W2>45 min):   {n_d45_ctrl}\n",
  "Ring distribution at 1961:"))
print(panel %>% filter(year == 1961, sample_tight) %>% count(ring))

saveRDS(panel, "data/panel_long.rds")
