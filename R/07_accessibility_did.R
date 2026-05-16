# =====================================================================
# 07_accessibility_did.R  --  continuous distance-from-casello DiD
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# W2 = driving time (minutes) from each comune to the nearest A1 toll
# booth, computed on the post-1964 final A1 network. It is the
# continuous distance variable Lelo & Tani referred to.
#
# Because W2 is *time-invariant* (one value per comune, same across all
# 5 censuses), the identification follows the Faber (2014) / Donaldson
# (2018) / Banerjee-Duflo-Qian (2020) strategy:
#
#   * the level of W2 is cross-sectional, but
#   * its INTERACTION with a post-treatment time dummy creates a
#     DiD-style variation, because comuni near a casello got treated
#     by A1 access while comuni far from one did not.
#
# Three specifications:
#
# (W1) Long DiD on first differences, levels of W2 as treatment intensity:
#         Delta log y_i = a + b * W2_i + g' X_i^{1961}
#                         + d_{prov(i)} + e_i
#      Expected sign of b: NEGATIVE (more distance -> less growth).
#
# (W2) Distance-band DiD (Ciani-de Blasio rings): comuni partitioned
#      into quintiles of W2. Coefficients on the upper four quintiles
#      vs Q1 trace the dose-response. Should be MONOTONICALLY NEGATIVE
#      (further from casello -> less growth).
#
# (W3) Continuous event study: W2_i interacted with year dummies in a
#      two-way FE model (comune + year FE). Pre-treatment interactions
#      (1951 x W2) should be statistically zero. Post-treatment
#      interactions (1971/1981/1991 x W2) should be negative.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")
baseline_cluster <- readRDS("data/cluster_1961.rds")

# Sanity
if (all(is.na(panel$W2))) {
  stop("W2 not found in the panel. Did you re-run R/01_prepare_data.R?")
}

# Broadcast a single, time-invariant W2 per comune (drop the per-year NA)
w2_xs <- panel %>% filter(!is.na(W2)) %>%
  distinct(PRO_COM, W2) %>% group_by(PRO_COM) %>%
  summarise(W2 = first(W2), .groups = "drop")
panel <- panel %>% select(-W2) %>% left_join(w2_xs, by = "PRO_COM")

message(glue("[07] W2 summary  N={sum(!is.na(panel$W2)) / 5}  ",
             "min={min(panel$W2, na.rm=TRUE)}  ",
             "median={median(panel$W2, na.rm=TRUE)}  ",
             "max={max(panel$W2, na.rm=TRUE)}"))

# ---- Long-DiD dataset 1961 -> 1991 -------------------------------------
wide <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro, treat_A1, year, W2,
         P1, lP1, lUT, lAT, emp_rate,
         I4, SS4, L15, L16, L17) %>%
  pivot_wider(names_from = year,
              values_from = c(P1, lP1, lUT, lAT, emp_rate,
                              I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961,
         d_er = emp_rate_1991 - emp_rate_1961) %>%
  filter(!is.na(W2), !is.na(d_lP))

# (W1) ------------------------------------------------------------------
ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")

fit_W1 <- function(y, dat) {
  list(
    `(1) OLS`        = with_cluster(lm(reformulate("W2", y), data = dat),
                                    dat$COD_PROV, dat),
    `(2) +Prov FE`   = with_cluster(lm(reformulate(c("W2","factor(COD_PROV)"), y), data = dat),
                                    dat$COD_PROV, dat),
    `(3) +1961 ctrl` = with_cluster(lm(reformulate(c("W2","factor(COD_PROV)", ctrls), y), data = dat),
                                    dat$COD_PROV, dat)
  )
}

outcomes <- c(d_lP = "Delta_logPop",
              d_lU = "Delta_logUnits",
              d_lA = "Delta_logEmp",
              d_er = "Delta_EmpRate")
w1_all <- list()
for (y in names(outcomes)) {
  message(glue("[07] (W1) {outcomes[y]}"))
  w1_all[[outcomes[y]]] <- fit_W1(y, wide)
  regtab(w1_all[[outcomes[y]]], keep = "^W2$",
         out_csv = glue("output/tables/w1_continuous_{outcomes[y]}.csv"),
         out_tex = glue("output/tables/w1_continuous_{outcomes[y]}.tex"),
         title   = glue("Continuous W2-DiD 1961-1991 -- {outcomes[y]}"))
}

preferred_w1 <- lapply(w1_all, function(x) x[[3]])
names(preferred_w1) <- names(outcomes)
regtab(preferred_w1, keep = "^W2$",
       out_csv = "output/tables/w1_continuous_summary.csv",
       out_tex = "output/tables/w1_continuous_summary.tex",
       title   = "Continuous distance-from-casello DiD (preferred spec)")

# (W2) ------------------------------------------------------------------
wide <- wide %>%
  mutate(w2_q = ntile(W2, 5),
         w2_band = factor(w2_q, levels = 1:5,
                          labels = c("Q1 (closest)","Q2","Q3","Q4",
                                     "Q5 (farthest)")))
fit_W2 <- function(y) {
  with_cluster(
    lm(reformulate(c("w2_band","factor(COD_PROV)", ctrls), y), data = wide),
    wide$COD_PROV, wide)
}
w2_models <- list(
  Pop   = fit_W2("d_lP"),
  Units = fit_W2("d_lU"),
  Emp   = fit_W2("d_lA"))
regtab(w2_models, keep = "^w2_band",
       out_csv = "output/tables/w2_distance_bands.csv",
       out_tex = "output/tables/w2_distance_bands.tex",
       title   = "Distance-band DiD: outcome growth by W2 quintile (ref Q1)")

# Plot dose-response
dose <- bind_rows(lapply(seq_along(w2_models), function(i) {
  m <- w2_models[[i]]; nm <- names(w2_models)[i]
  td <- tidy_cr(m) %>% filter(grepl("^w2_band", term)) %>%
    mutate(band = sub("w2_band", "", term), outcome = nm)
  bind_rows(td,
            tibble(term = "ref", band = "Q1 (closest)", outcome = nm,
                   estimate = 0, std.error = NA, statistic = NA,
                   p.value = NA, conf.low = 0, conf.high = 0))
})) %>%
  mutate(band = factor(band, levels = c("Q1 (closest)","Q2","Q3","Q4",
                                        "Q5 (farthest)")))

p_dose <- ggplot(dose, aes(x = band, y = estimate,
                           ymin = conf.low, ymax = conf.high,
                           group = outcome, colour = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(0.3), size = 0.4) +
  scale_colour_manual(values = c("Pop" = "#2c3e50",
                                 "Units" = "#16a085",
                                 "Emp" = "#c0392b"), name = NULL) +
  labs(x = "Quintile of W2 (driving min to nearest A1 casello)",
       y = "1961-1991 effect vs Q1 (closest)",
       title = "Dose-response: distance from A1 casello and 1961-1991 growth",
       subtitle = "Long DiD with prov FE + 1961 controls; 95% cluster-robust CI") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("output/figures/fig04_W2_dose_response.png", p_dose,
       width = 9, height = 4.5, dpi = 200)
ggsave("output/figures/fig04_W2_dose_response.pdf", p_dose,
       width = 9, height = 4.5)

# (W3) ------------------------------------------------------------------
# Event study with W2 (time-invariant) interacted with each census year
panel_es <- panel %>% filter(sample_tight, !is.na(W2))
for (k in CENSUS_YEARS) {
  if (k == PRE_YEAR) next
  panel_es[[paste0("W2x_", k)]] <- as.integer(panel_es$year == k) * panel_es$W2
}
event_terms <- paste0("W2x_", setdiff(CENSUS_YEARS, PRE_YEAR))

fit_W3 <- function(y) {
  rhs <- c(event_terms, "factor(PRO_COM)", "factor(year)")
  m <- lm(reformulate(rhs, y), data = panel_es)
  with_cluster(m, panel_es$COD_PROV, panel_es)
}
w3_models <- list(
  `log Population`  = fit_W3("lP1"),
  `log Local units` = fit_W3("lUT"),
  `log Employees`   = fit_W3("lAT"))

w3_long <- bind_rows(lapply(seq_along(w3_models), function(i) {
  m <- w3_models[[i]]; nm <- names(w3_models)[i]
  td <- tidy_cr(m) %>% filter(grepl("^W2x_", term)) %>%
    mutate(year = as.integer(sub("W2x_", "", term)), outcome = nm)
  bind_rows(td,
            tibble(term = "ref", year = PRE_YEAR, outcome = nm,
                   estimate = 0, std.error = NA, statistic = NA,
                   p.value = NA, conf.low = 0, conf.high = 0))
}))

p_w3 <- w3_long %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = A1_OPEN_YEAR, linetype = "dotted",
             colour = "#c0392b") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = CENSUS_YEARS) +
  labs(x = NULL, y = "Coefficient on W2 x year (95% CI)",
       title = "Event study: outcome growth and distance from A1 casello",
       subtitle = paste0("Reference year: ", PRE_YEAR,
                         "  |  Coef < 0 means: more distance -> less growth")) +
  theme_paper()

ggsave("output/figures/fig05_W2_event_study.png", p_w3,
       width = 9, height = 4.5, dpi = 200)
ggsave("output/figures/fig05_W2_event_study.pdf", p_w3,
       width = 9, height = 4.5)

write_csv(w3_long %>% filter(term != "ref"),
          "output/tables/w3_event_study_coefs.csv")

message("[07] Done.")
