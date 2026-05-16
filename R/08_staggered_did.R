# =====================================================================
# 08_staggered_did.R  --  staggered DiD à la Callaway-Sant'Anna
# =====================================================================
#
# Honesty note --------------------------------------------------------
#
# The A1 toll booths opened in four cohorts (K7 = 1959, 1960, 1962,
# 1964). The census panel is decennial: 1951, 1961, 1971, 1981, 1991.
# All four cohorts fall in the SAME inter-census window (1961-1971),
# so at the granularity of the census the four cohorts share an
# identical event-time mapping (pre = 1961, first post = 1971).
#
# This rules out a full Callaway-Sant'Anna staggered design with
# the post-1961 outcomes alone -- there is no within-cohort variation
# in event time to identify cohort-specific dynamics.
#
# HOWEVER: at the **1961 census** the 1959 cohort has been treated
# for 2 years and the 1960 cohort for 1 year, while the 1962 and
# 1964 cohorts are still untreated. This gives us ONE genuinely
# staggered comparison:
#
#   (T1) Short-run staggered DiD:
#        early cohorts (1959+1960)  treated in 1961
#        late  cohorts (1962+1964)  not yet treated in 1961
#        never-treated controls     same as ever
#        outcome:  Delta y between 1951 and 1961
#
#   Identification:
#     ATT_short = E[y_61 - y_51 | early] - E[y_61 - y_51 | late]
#               + E[y_61 - y_51 | late ] - E[y_61 - y_51 | never]
#
#   The first difference compares treated-by-1961 to not-yet-treated,
#   netting out anything that the cohort dummies absorb. The second
#   difference uses the never-treated as the "clean" control. If the
#   parallel-trends assumption holds, ATT_short identifies the 1959/
#   1960 cohort's first 1-2 years of A1 exposure.
#
# Three blocks below:
#
#   (T1) Short-run staggered DiD as defined above (real staggered).
#   (S1) Event study (non-staggered: shared event-time mapping).
#        Re-labelled to remove the "staggered" claim.
#   (S2) Cohort-by-cohort long DiD  --  heterogeneity, NOT staggered.
#   (S4) Aree_Int 6-band heterogeneity.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- cohort assignment ------------------------------------------------
panel <- panel %>%
  mutate(
    cohort = case_when(
      year_open == 1959                 ~ "1959",
      year_open == 1960                 ~ "1960",
      year_open >= 1961 & year_open<=1963 ~ "1962",
      year_open >= 1964                 ~ "1964",
      TRUE                              ~ NA_character_),
    cohort = factor(cohort, levels = c("1959","1960","1962","1964")),
    # "early" cohorts are treated by the 1961 census; "late" are not
    early = ifelse(!is.na(cohort) & cohort %in% c("1959","1960"), 1L, 0L),
    late  = ifelse(!is.na(cohort) & cohort %in% c("1962","1964"), 1L, 0L)
  )

dat <- panel %>% filter(sample_tight)

# ---- (T1) Short-run staggered DiD 1951 -> 1961 -----------------------
# Build the 1951-1961 first-difference dataset
fd <- dat %>% filter(year %in% c(1951, 1961)) %>%
  pivot_wider(id_cols = c(PRO_COM, COD_PROV, COD_REG, macro, treat_A1,
                          cohort, early, late, year_open),
              names_from = year,
              values_from = c(P1, lP1, lUT, lAT),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1961 - lP1_1951,
         d_lU = lUT_1961 - lUT_1951,
         d_lA = lAT_1961 - lAT_1951)

# Define the comparison: early (n=19) vs late (n=34) vs never (n~1140)
# - treat = "early": 1959/60 cohort, treated by 1961
# - control 1 ("late"): 1962/64 cohort, NOT yet treated by 1961
# - control 2 ("never"): all other comuni in the tight sample
fd <- fd %>%
  mutate(grp = case_when(early == 1 ~ "Early (treated by 1961)",
                         late  == 1 ~ "Late (not yet treated)",
                         TRUE       ~ "Never treated"),
         grp = factor(grp, levels = c("Never treated",
                                      "Late (not yet treated)",
                                      "Early (treated by 1961)")))
table(fd$grp)

fit_T1 <- function(y) {
  with_cluster(
    lm(reformulate(c("grp", "factor(COD_PROV)"), y), data = fd),
    fd$COD_PROV, fd)
}
t1_models <- list(
  Pop   = fit_T1("d_lP"),
  Units = fit_T1("d_lU"),
  Emp   = fit_T1("d_lA"))
regtab(t1_models, keep = "^grp",
       out_csv = "output/tables/t1_staggered_shortrun.csv",
       out_tex = "output/tables/t1_staggered_shortrun.tex",
       title   = "Short-run staggered DiD 1951-1961 (TRUE staggered)")

# The interpretation:
#   * coef on "Late (not yet treated)" should be ~0 if late and never
#     groups followed the same pre-trend (parallel trends test).
#   * coef on "Early (treated by 1961)" minus coef on "Late" is the
#     **ATT_short** (1-2 years of A1 exposure).
# Compute the contrast with delta-method SE.
contrast_T1 <- function(m) {
  V <- clubSandwich::vcovCR(m, cluster = attr(m, "cluster_vec"), type = "CR1")
  b <- coef(m)
  est <- b[["grpEarly (treated by 1961)"]] - b[["grpLate (not yet treated)"]]
  v   <- V[c("grpEarly (treated by 1961)","grpLate (not yet treated)"),
           c("grpEarly (treated by 1961)","grpLate (not yet treated)")]
  se <- sqrt(v[1,1] + v[2,2] - 2*v[1,2])
  tibble(estimate = est, se = se,
         lo = est - 1.96*se, hi = est + 1.96*se,
         t  = est/se, p = 2*(1 - pnorm(abs(est/se))))
}
att_short <- bind_rows(lapply(seq_along(t1_models), function(i) {
  contrast_T1(t1_models[[i]]) %>% mutate(outcome = names(t1_models)[i])
}))
print(att_short)
write_csv(att_short, "output/tables/t1_att_short_contrast.csv")
message("[08] (T1) short-run staggered DiD done.")

# ---- (S1) Event study (NOT staggered: shared event-time) -------------
# We keep this as a robustness check: a standard event study on the
# treat_A1 binary indicator, identical to M2 in R/04 but restricted to
# the K0=1 list (so we can compare across the binary and continuous
# specs on the same sample).

dat_es <- dat
for (k in CENSUS_YEARS) {
  if (k == PRE_YEAR) next
  dat_es[[paste0("D_", k)]] <- as.integer(dat_es$year == k) * dat_es$treat_A1
}
event_terms <- paste0("D_", setdiff(CENSUS_YEARS, PRE_YEAR))
fit_S1 <- function(y) {
  rhs <- c(event_terms, "factor(PRO_COM)", "factor(year)")
  m <- lm(reformulate(rhs, y), data = dat_es)
  with_cluster(m, dat_es$COD_PROV, dat_es)
}
s1_models <- list(
  `log Population`  = fit_S1("lP1"),
  `log Local units` = fit_S1("lUT"),
  `log Employees`   = fit_S1("lAT"))
s1_long <- bind_rows(lapply(seq_along(s1_models), function(i) {
  m <- s1_models[[i]]; nm <- names(s1_models)[i]
  td <- tidy_cr(m) %>% filter(grepl("^D_", term)) %>%
    mutate(year = as.integer(sub("D_", "", term)), outcome = nm)
  bind_rows(td,
            tibble(term = "ref", year = PRE_YEAR, outcome = nm,
                   estimate = 0, std.error = NA, statistic = NA,
                   p.value = NA, conf.low = 0, conf.high = 0))
}))

p_s1 <- s1_long %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = A1_OPEN_YEAR, linetype = "dotted",
             colour = "#c0392b") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = CENSUS_YEARS) +
  labs(x = NULL, y = "Coefficient on treat × year (95% CI)",
       title = "Event study on K0=1 treatment indicator",
       subtitle = paste0("Reference: ", PRE_YEAR,
                         "  |  Not staggered: all cohorts share the same event-time mapping")) +
  theme_paper()
ggsave("output/figures/fig06_event_study_K0.png", p_s1,
       width = 9, height = 4.5, dpi = 200)
ggsave("output/figures/fig06_event_study_K0.pdf", p_s1,
       width = 9, height = 4.5)
write_csv(s1_long, "output/tables/s1_event_study_K0.csv")

# ---- (S2) Cohort heterogeneity in long DiD (NOT staggered) -----------
ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")
wide <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  pivot_wider(id_cols = c(PRO_COM, COD_PROV, COD_REG, macro, treat_A1,
                          cohort, year_open, Aree_Int),
              names_from = year,
              values_from = c(P1, lP1, lUT, lAT, emp_rate,
                              I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961,
         cohort_g = factor(ifelse(treat_A1 == 1, as.character(cohort), "Control"),
                           levels = c("Control","1959","1960","1962","1964")))

fit_S2 <- function(y) {
  with_cluster(
    lm(reformulate(c("cohort_g", "factor(COD_PROV)", ctrls), y), data = wide),
    wide$COD_PROV, wide)
}
s2_models <- list(
  Pop   = fit_S2("d_lP"),
  Units = fit_S2("d_lU"),
  Emp   = fit_S2("d_lA"))
regtab(s2_models, keep = "^cohort_g",
       out_csv = "output/tables/s2_cohort_long_did.csv",
       out_tex = "output/tables/s2_cohort_long_did.tex",
       title   = "Cohort heterogeneity in long DiD 1961-1991 (NOT staggered)")

cohort_te <- bind_rows(lapply(seq_along(s2_models), function(i) {
  m <- s2_models[[i]]; nm <- names(s2_models)[i]
  tidy_cr(m) %>% filter(grepl("^cohort_g", term)) %>%
    mutate(cohort = sub("cohort_g", "", term), outcome = nm)
}))
p_s2 <- cohort_te %>%
  mutate(cohort = factor(cohort, levels = c("1959","1960","1962","1964"))) %>%
  ggplot(aes(x = cohort, y = estimate, ymin = conf.low, ymax = conf.high,
             colour = outcome, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(0.4)) +
  scale_colour_manual(values = c("Pop" = "#2c3e50",
                                 "Units" = "#16a085",
                                 "Emp" = "#c0392b"),
                      name = NULL) +
  labs(x = "Opening cohort (K7)", y = "1961-1991 effect vs Control",
       title = "Long-DiD heterogeneity by opening cohort",
       subtitle = "NOT a staggered DiD: census granularity collapses all cohorts into one event-time. Heterogeneity only.") +
  theme_paper()
ggsave("output/figures/fig07_cohort_effects.png", p_s2,
       width = 8, height = 4, dpi = 200)
ggsave("output/figures/fig07_cohort_effects.pdf", p_s2,
       width = 8, height = 4)

# ---- (S4) Aree_Int six-band heterogeneity ----------------------------
wide_ai2 <- wide %>%
  mutate(Aree_Int = factor(Aree_Int,
                           levels = c("F - Ultraperiferico", "E - Periferico",
                                      "D - Intermedio",      "C - Cintura",
                                      "B - Polo intercomunale", "A - Polo"))) %>%
  filter(!is.na(Aree_Int))
fit_S4 <- function(y) {
  with_cluster(
    lm(reformulate(c("Aree_Int", "factor(COD_PROV)", ctrls), y), data = wide_ai2),
    wide_ai2$COD_PROV, wide_ai2)
}
s4_models <- list(
  Pop   = fit_S4("d_lP"),
  Units = fit_S4("d_lU"),
  Emp   = fit_S4("d_lA"))
regtab(s4_models, keep = "^Aree_Int",
       out_csv = "output/tables/s4_aree_int_did.csv",
       out_tex = "output/tables/s4_aree_int_did.tex",
       title   = "Aree-interne six-band heterogeneity (ref = F - Ultraperiferico)")

message("[08] Done.")
