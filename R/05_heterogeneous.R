# =====================================================================
# 05_heterogeneous.R  --  effect heterogeneity
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Lelo & Tani report (qualitatively) very different impact patterns
# across the country: rapid early gains in the North (Via Emilia),
# steady growth in the Centre (Florence/Rome suburbanisation), delayed
# but persistent gains in the South. We test this formally by
# interacting `treat_A1` with two stratifying variables:
#
#   (a) macro-area  -- North / Centre / South
#       The omitted (reference) category is "Centre", since Lelo & Tani
#       point at Florence/Rome as the central case.
#
#   (b) 1961 development cluster (from R/03_descriptive.R)
#       Tests whether ex-ante backward comuni gained more or less from
#       A1 access than ex-ante advanced ones (Puga 2002 "polarisation"
#       hypothesis vs. Hansen 1965 "unbalanced growth").
#
#   (c) 1961 population quartile -- proxy for initial size

source("R/00_setup.R")
panel            <- readRDS("data/panel_long.rds")
baseline_cluster <- readRDS("data/cluster_1961.rds")

wide <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro, treat_A1, year,
         P1, UT, AT, lP1, lUT, lAT, emp_rate) %>%
  pivot_wider(names_from = year,
              values_from = c(P1, UT, AT, lP1, lUT, lAT, emp_rate),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961) %>%
  left_join(baseline_cluster, by = "PRO_COM") %>%
  mutate(p_quartile = ntile(P1_1961, 4),
         macro      = factor(macro, levels = c("Centre", "North", "South")))

# ---- (a) macro-area split ----------------------------------------------
het_macro <- list(
  Population = with_cluster(lm(d_lP ~ treat_A1 * macro + factor(COD_PROV), data = wide),
                            wide$COD_PROV, wide),
  Units      = with_cluster(lm(d_lU ~ treat_A1 * macro + factor(COD_PROV), data = wide),
                            wide$COD_PROV, wide),
  Employees  = with_cluster(lm(d_lA ~ treat_A1 * macro + factor(COD_PROV), data = wide),
                            wide$COD_PROV, wide)
)

regtab(het_macro, keep = "(treat_A1|macro)",
       out_csv = "output/tables/m3_heter_macro.csv",
       out_tex = "output/tables/m3_heter_macro.tex",
       title   = "Heterogeneity by macro-area (Centre = ref)")

# Predicted treatment effect per macro-area (delta-method CI):
#   TE(Centre) = b_treat
#   TE(North)  = b_treat + b_treat:macroNorth
#   TE(South)  = b_treat + b_treat:macroSouth
make_te <- function(m, label) {
  V <- clubSandwich::vcovCR(m, cluster = attr(m, "cluster_vec"), type = "CR1")
  b <- coef(m)
  pull_term <- function(t) if (t %in% names(b)) b[[t]] else 0
  pull_se   <- function(terms) {
    # Match on V's own row names: vcovCR drops coefficients that are
    # NA in the fit (singularities). names(b) may be longer than V.
    in_V <- intersect(terms, rownames(V))
    if (length(in_V) == 0) return(NA_real_)
    sub <- V[in_V, in_V, drop = FALSE]
    sqrt(sum(sub))
  }
  rows <- list()
  rows[[1]] <- tibble(macro = "Centre",
                      estimate = pull_term("treat_A1"),
                      se = pull_se("treat_A1"))
  for (m_lab in c("North", "South")) {
    term_int <- paste0("treat_A1:macro", m_lab)
    rows[[length(rows) + 1]] <- tibble(
      macro    = m_lab,
      estimate = pull_term("treat_A1") + pull_term(term_int),
      se       = pull_se(c("treat_A1", term_int)))
  }
  bind_rows(rows) %>%
    mutate(lo = estimate - 1.96 * se,
           hi = estimate + 1.96 * se,
           outcome = label)
}

te_macro <- bind_rows(
  make_te(het_macro$Population, "log Population"),
  make_te(het_macro$Units,      "log Local units"),
  make_te(het_macro$Employees,  "log Employees")
)
write_csv(te_macro, "output/tables/m3_te_by_macro.csv")
print(te_macro)

p_macro <- te_macro %>%
  ggplot(aes(x = macro, y = estimate, ymin = lo, ymax = hi, colour = macro)) +
  geom_pointrange(size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ outcome) +
  scale_colour_manual(values = c("Centre" = "#16a085",
                                 "North"  = "#2c3e50",
                                 "South"  = "#c0392b"),
                      guide = "none") +
  labs(x = NULL, y = "1961-1991 treatment effect (log points)",
       title = "A1 treatment effect by macro-area",
       subtitle = "Long DiD with province FE; 95% cluster-robust CI") +
  theme_paper()

ggsave("output/figures/fig03_het_macro.png", p_macro,
       width = 9, height = 4, dpi = 200)
ggsave("output/figures/fig03_het_macro.pdf", p_macro, width = 9, height = 4)

# ---- (b) by 1961 development cluster ------------------------------------
wide_b <- wide %>% filter(!is.na(profile_1961))
het_clust <- with_cluster(
  lm(d_lP ~ treat_A1 * factor(profile_1961) + factor(COD_PROV), data = wide_b),
  wide_b$COD_PROV, wide_b)
regtab(list(`Delta log Population` = het_clust),
       keep = "(treat_A1|profile)",
       out_csv = "output/tables/m3_heter_cluster.csv",
       out_tex = "output/tables/m3_heter_cluster.tex",
       title   = "Heterogeneity by 1961 development profile")

# ---- (c) by 1961 population quartile ------------------------------------
het_q <- with_cluster(
  lm(d_lP ~ treat_A1 * factor(p_quartile) + factor(COD_PROV), data = wide),
  wide$COD_PROV, wide)
regtab(list(`Delta log Population` = het_q),
       keep = "(treat_A1|quartile)",
       out_csv = "output/tables/m3_heter_size.csv",
       out_tex = "output/tables/m3_heter_size.tex",
       title   = "Heterogeneity by 1961 population quartile")

message("[05] Done.")
