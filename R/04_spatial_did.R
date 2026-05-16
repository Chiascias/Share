# =====================================================================
# 04_spatial_did.R  --  spatial DiD estimation
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Two complementary specifications, both clustered at the province level
# (the level at which treatment varies and at which Ciani-de Blasio also
# cluster).
#
# ---- (M1) Long DiD on first differences 1961 -> 1991 ----
#
#   Delta y_i = a + b * treat_i + gamma' X_i^{1961} + d_{prov(i)} + e_i
#
# This is a first-difference (within-comune) estimator that absorbs any
# time-invariant comune-level confounder. Province fixed effects then
# absorb province-specific shocks (e.g. autostrada A2 in Lazio, Casse
# del Mezzogiorno in Campania). The set X_i^{1961} contains 1961
# levels of population, illiteracy, education, and sectoral employment
# shares -- these capture the channel through which planners selected
# more developed areas (the "endogeneity" concern Lelo & Tani flag at
# the end of Section 4).
#
# Outcomes: log(P1), log(UT), log(AT), employment rate AT/P1.
#
# Three nested columns per outcome:
#   (1) Plain OLS    -> raw correlation
#   (2) + Province FE -> within-province comparison
#   (3) + 1961 controls -> conditional on baseline development
#
# ---- (M2) Event study 1951..1991 ----
#
#   y_{it} = alpha_i + lambda_t + sum_{k != 1961} delta_k * (treat_i * 1{t = k})
#            + e_{it}
#
# Comune FE + year FE. The coefficient on (treat × 1951) is the
# pre-trends placebo: if the parallel-trends assumption holds, it should
# be statistically zero. Post-1961 coefficients trace out the dynamics
# of the treatment effect, the central piece of evidence in modern DiD
# papers.

source("R/00_setup.R")
panel            <- readRDS("data/panel_long.rds")
baseline_cluster <- readRDS("data/cluster_1961.rds")

# ---- (M1) Long DiD ------------------------------------------------------
wide61_91 <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro, treat_A1, year,
         P1, UT, AT, lP1, lUT, lAT, emp_rate,
         I4, SS4, L15, L16, L17) %>%
  pivot_wider(names_from = year,
              values_from = c(P1, UT, AT, lP1, lUT, lAT, emp_rate,
                              I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961,
         d_er = emp_rate_1991 - emp_rate_1961) %>%
  left_join(baseline_cluster, by = "PRO_COM")

# Drop rows with missing outcomes / covariates
wide61_91 <- wide61_91 %>%
  filter(!is.na(d_lP), !is.na(lP1_1961))

# Control vector at 1961 baseline
ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")

fit_three <- function(y, dat) {
  list(
    `(1) OLS`            = with_cluster(lm(reformulate("treat_A1", y), data = dat),
                                        dat$COD_PROV, dat),
    `(2) +Prov FE`       = with_cluster(lm(reformulate(c("treat_A1", "factor(COD_PROV)"), y), data = dat),
                                        dat$COD_PROV, dat),
    `(3) +1961 controls` = with_cluster(lm(reformulate(c("treat_A1", "factor(COD_PROV)", ctrls), y), data = dat),
                                        dat$COD_PROV, dat)
  )
}

outcomes <- c(d_lP = "Delta_logPop",
              d_lU = "Delta_logUnits",
              d_lA = "Delta_logEmp",
              d_er = "Delta_EmpRate")

m1_all <- list()
for (y in names(outcomes)) {
  message(glue("[04] M1: {outcomes[y]}"))
  m1_all[[outcomes[y]]] <- fit_three(y, wide61_91)
  regtab(m1_all[[outcomes[y]]], keep = "^treat_A1$",
         out_csv = glue("output/tables/m1_longdid_{outcomes[y]}.csv"),
         out_tex = glue("output/tables/m1_longdid_{outcomes[y]}.tex"),
         title   = glue("Long DiD 1961-1991 -- {outcomes[y]}"))
}
saveRDS(m1_all, "output/tables/m1_models.rds")

# Headline preferred-spec table (column 3 for each outcome)
preferred <- lapply(m1_all, function(x) x[[3]])
names(preferred) <- names(outcomes)
regtab(preferred, keep = "^treat_A1$",
       out_csv = "output/tables/m1_longdid_summary.csv",
       out_tex = "output/tables/m1_longdid_summary.tex",
       title   = "Long DiD 1961-1991 -- preferred specification")

# ---- (M2) Event study ---------------------------------------------------
# Construct interaction terms manually: D_k = treat * 1{year = k}.
panel_es <- panel %>%
  filter(sample_tight) %>%
  mutate(year_f = factor(year, levels = CENSUS_YEARS))

# Build dummies treat_x_1951, ..., treat_x_1991 (omit 1961)
for (k in CENSUS_YEARS) {
  if (k == PRE_YEAR) next
  panel_es[[paste0("D_", k)]] <- as.integer(panel_es$year == k) * panel_es$treat_A1
}
event_terms <- paste0("D_", setdiff(CENSUS_YEARS, PRE_YEAR))

# Event study, by outcome. PRO_COM and year FE are intercepts; we
# cluster at the province level.
fit_event <- function(y) {
  rhs <- c(event_terms, "factor(PRO_COM)", "factor(year)")
  m <- lm(reformulate(rhs, y), data = panel_es)
  with_cluster(m, panel_es$COD_PROV, panel_es)
}

es_outcomes <- list(`log Population` = "lP1",
                    `log Local units` = "lUT",
                    `log Employees`   = "lAT",
                    `Employment rate` = "emp_rate")

es_models <- lapply(es_outcomes, fit_event)

# Save a SLIM version (drop residuals/QR which make the file huge): we
# only need the cluster vec and coef table later.
slim <- function(m) {
  list(coefficients = coef(m),
       vcov         = clubSandwich::vcovCR(m, cluster = attr(m, "cluster_vec"),
                                           type = "CR1"),
       nobs         = nobs(m),
       r2           = summary(m)$r.squared)
}
saveRDS(lapply(es_models, slim), "output/tables/m2_event_study_slim.rds")

# Tidy with cluster-robust SE for plotting
es_long <- bind_rows(lapply(seq_along(es_models), function(i) {
  m    <- es_models[[i]]
  name <- names(es_models)[i]
  td   <- tidy_cr(m) %>%
    filter(grepl("^D_", term)) %>%
    mutate(year = as.integer(sub("D_", "", term)), outcome = name)
  bind_rows(td,
            tibble(term = "ref", year = PRE_YEAR, outcome = name,
                   estimate = 0, std.error = NA_real_,
                   statistic = NA_real_, p.value = NA_real_,
                   conf.low = 0, conf.high = 0))
}))

# Plot
p_es <- es_long %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = A1_OPEN_YEAR, linetype = "dotted",
             colour = "#c0392b") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = CENSUS_YEARS) +
  labs(x = NULL, y = "Coefficient on treat x year (95% CI)",
       title = "Event study: A1 toll-booth comuni vs same-province controls",
       subtitle = paste0("Reference year: ", PRE_YEAR,
                         "  |  Red dotted line: A1 inaugurated Oct 1964")) +
  theme_paper()

ggsave("output/figures/fig02_event_study.png", p_es,
       width = 9, height = 6, dpi = 200)
ggsave("output/figures/fig02_event_study.pdf", p_es, width = 9, height = 6)

# Event-study coefficient table (for the LaTeX appendix)
es_tab <- es_long %>%
  filter(term != "ref") %>%
  transmute(outcome, year, estimate = round(estimate, 4),
            std.error = round(std.error, 4),
            t = round(statistic, 2),
            p = round(p.value, 4))
write_csv(es_tab, "output/tables/m2_event_study_coefs.csv")

message("[04] Done.")
