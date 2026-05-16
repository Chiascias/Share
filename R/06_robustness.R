# =====================================================================
# 06_robustness.R  --  robustness checks
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Five robustness exercises:
#
#   (R1) Placebo on 1951 -> 1961 differences (A1 not yet open). If the
#        parallel-trends assumption holds the coefficient on `treat_A1`
#        should be statistically zero.
#
#   (R2) Excluding provincial capitals. Lelo & Tani note that
#        capoluoghi (Roma, Milano, Firenze, Bologna, Napoli...) are
#        always treated and dominate the raw averages. Dropping them
#        gives the effect on "ordinary" exit municipalities.
#
#   (R3) Wide control sample (every comune in the 8-region panel, not
#        only same-province ones).
#
#   (R4) Alternative clustering of standard errors: province only,
#        region only, two-way (province + region).
#
#   (R5) Outcome-by-outcome event-study placebo: regress each outcome
#        on the placebo "treatment * 1951" dummy with year FE; should be
#        zero.

source("R/00_setup.R")
panel            <- readRDS("data/panel_long.rds")

# Helper: build first-difference data set between two years
fd <- function(y0, y1, sample_var = "sample_tight") {
  panel %>%
    filter(year %in% c(y0, y1), !!sym(sample_var)) %>%
    select(PRO_COM, COD_PROV, COD_REG, macro, treat_A1, year,
           P1, UT, AT, lP1, lUT, lAT, emp_rate) %>%
    pivot_wider(names_from = year,
                values_from = c(P1, UT, AT, lP1, lUT, lAT, emp_rate),
                names_sep = "_") %>%
    mutate(d_lP = !!sym(paste0("lP1_", y1)) - !!sym(paste0("lP1_", y0)),
           d_lU = !!sym(paste0("lUT_", y1)) - !!sym(paste0("lUT_", y0)),
           d_lA = !!sym(paste0("lAT_", y1)) - !!sym(paste0("lAT_", y0)))
}

# ---- (R1) Placebo 1951 -> 1961 ------------------------------------------
plac <- fd(1951, 1961)
plac_models <- list(
  Population = with_cluster(lm(d_lP ~ treat_A1 + factor(COD_PROV), data = plac), plac$COD_PROV, plac),
  Units      = with_cluster(lm(d_lU ~ treat_A1 + factor(COD_PROV), data = plac), plac$COD_PROV, plac),
  Employees  = with_cluster(lm(d_lA ~ treat_A1 + factor(COD_PROV), data = plac), plac$COD_PROV, plac)
)
regtab(plac_models, keep = "^treat_A1$",
       out_csv = "output/tables/r1_placebo_1951_1961.csv",
       out_tex = "output/tables/r1_placebo_1951_1961.tex",
       title   = "Placebo: pre-treatment differences 1951-1961")
message("[06] (R1) placebo done.")

# ---- (R2) Excluding provincial capitals ---------------------------------
# 1961 capoluoghi crossed by the A1 panel:
capoluoghi <- c(15146,   # Milano
                33032,   # Piacenza
                34027,   # Parma
                35033,   # Reggio nell'Emilia
                36023,   # Modena
                37006,   # Bologna
                48017,   # Firenze
                51002,   # Arezzo
                52032,   # Siena (not on A1 but listed in draft)
                54001,   # Perugia
                55054,   # Terni
                56059,   # Viterbo
                57059,   # Rieti
                58091,   # Roma
                60038,   # Frosinone
                61022,   # Caserta
                63049)   # Napoli
no_cap <- fd(1961, 1991) %>% filter(!PRO_COM %in% capoluoghi)
m_nocap <- list(
  Population = with_cluster(lm(d_lP ~ treat_A1 + factor(COD_PROV), data = no_cap), no_cap$COD_PROV, no_cap),
  Units      = with_cluster(lm(d_lU ~ treat_A1 + factor(COD_PROV), data = no_cap), no_cap$COD_PROV, no_cap),
  Employees  = with_cluster(lm(d_lA ~ treat_A1 + factor(COD_PROV), data = no_cap), no_cap$COD_PROV, no_cap)
)
regtab(m_nocap, keep = "^treat_A1$",
       out_csv = "output/tables/r2_no_capoluoghi.csv",
       out_tex = "output/tables/r2_no_capoluoghi.tex",
       title   = "Excluding provincial capitals (1961-1991)")
message("[06] (R2) no-capoluoghi done.")

# ---- (R3) Wide control sample -------------------------------------------
wide_full <- fd(1961, 1991, sample_var = "sample_wide")
m_wide <- list(
  Population = with_cluster(lm(d_lP ~ treat_A1 + factor(COD_PROV), data = wide_full), wide_full$COD_PROV, wide_full),
  Units      = with_cluster(lm(d_lU ~ treat_A1 + factor(COD_PROV), data = wide_full), wide_full$COD_PROV, wide_full),
  Employees  = with_cluster(lm(d_lA ~ treat_A1 + factor(COD_PROV), data = wide_full), wide_full$COD_PROV, wide_full)
)
regtab(m_wide, keep = "^treat_A1$",
       out_csv = "output/tables/r3_wide_control.csv",
       out_tex = "output/tables/r3_wide_control.tex",
       title   = "Wide control: all non-A1 comuni (1961-1991)")
message("[06] (R3) wide control done.")

# ---- (R4) Alternative SE clustering -------------------------------------
base <- fd(1961, 1991)
m   <- lm(d_lP ~ treat_A1 + factor(COD_PROV), data = base)
used <- as.integer(rownames(model.frame(m)))
robust_se <- function(cluster_vec_name) {
  cl <- base[[cluster_vec_name]][used]
  V <- clubSandwich::vcovCR(m, cluster = cl, type = "CR1")
  lmtest::coeftest(m, vcov. = V)["treat_A1", ]
}
se_table <- tibble(
  cluster_var = c("COD_PROV", "COD_REG"),
  result      = lapply(cluster_var, robust_se)
) %>%
  mutate(estimate = sapply(result, function(r) r[["Estimate"]]),
         se       = sapply(result, function(r) r[["Std. Error"]]),
         t        = sapply(result, function(r) r[["t value"]]),
         p        = sapply(result, function(r) r[["Pr(>|t|)"]])) %>%
  select(-result)
write_csv(se_table, "output/tables/r4_alternative_se.csv")
print(se_table)
message("[06] (R4) alternative SE done.")

message("[06] Done.")
