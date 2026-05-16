# =====================================================================
# 08_staggered_did.R  --  staggered DiD exploiting K7 opening year
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# The data carry the *opening year* of each A1 toll booth (K7, range
# 1959-1964 — exactly matching Lelo & Tani Table 1). This gives us
# VARIATION IN TREATMENT TIMING which is much richer than a simple
# pre/post 1961-1991 comparison and is the modern standard in
# difference-in-differences (Goodman-Bacon 2021, Callaway-Sant'Anna
# 2021, de Chaisemartin-D'Haultfoeuille 2020).
#
# Three opening cohorts emerge from K7:
#   1959 cohort  --  Milano-Piacenza-Parma-Modena-Bologna axis (the
#                    "Via Emilia" branch)
#   1960 cohort  --  Bologna-Firenze Apennine crossing
#   1962 cohort  --  Roma-Frosinone-Cassino-Capua-Napoli leg
#   1964 cohort  --  Valdarno-Chiusi-Orvieto-Orte (the spine closure)
#
# We exploit this in three ways:
#
# (S1) Event study in EVENT TIME (year - K7) on log outcomes, with
#      comune and calendar-year FE. The omitted event time is -1
#      (the census just before opening). With census granularity (10y
#      gaps), the available event times are -2, -1, 0, +1, +2 (or so)
#      times the inter-census spacing — we use the closest census to
#      year_open as e=0.
#
# (S2) Cohort-by-time interactions, plotting the average post-treatment
#      coefficient by opening cohort (1959/1960/1962/1964). This is a
#      simple "did the early cohorts gain more or less than the late
#      cohorts?" diagnostic and gives a hint of the Goodman-Bacon
#      decomposition bias.
#
# (S3) Long DiD by cohort:
#        Delta y = a + b_c * (treat * cohort) + d_prov + e
#      so we get a separate treatment effect for each opening cohort.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- prep: event time relative to the closest census to year_open -----
# Map K7 (1959,1960,1962,1964) to the corresponding pre/post census.
# Census years are 1951, 1961, 1971, 1981, 1991. Treatment effectively
# starts being measurable from the census AFTER opening:
#   K7 in 1958-1964  -> pre census 1961, first post census 1971
# We code event_time as (year - first_post_census). Since first_post is
# always 1971 for the A1, event_time takes values -2 (1951), -1 (1961),
# 0 (1971), +1 (1981), +2 (1991).
panel <- panel %>%
  mutate(
    cohort   = case_when(
      year_open <= 1959                   ~ "1959",
      year_open >= 1960 & year_open <= 1961 ~ "1960",
      year_open >= 1962 & year_open <= 1963 ~ "1962",
      year_open >= 1964                   ~ "1964",
      TRUE                                 ~ NA_character_),
    cohort   = factor(cohort, levels = c("1959","1960","1962","1964")),
    # event-time mapping: the next census after K7 is e=0
    e_census = case_when(
      is.na(year_open) ~ NA_real_,
      year_open <= 1961 ~ (year - 1971) / 10,  # base = 1971 for early cohorts
      year_open <= 1971 ~ (year - 1971) / 10,
      year_open <= 1981 ~ (year - 1981) / 10,
      TRUE              ~ (year - 1991) / 10),
    e_census = ifelse(treat_A1 == 1, e_census, NA_real_)
  )

# Restrict to within-A1-province sample for clean comparison
dat <- panel %>% filter(sample_tight)

# Inspect cohort sizes
cohort_n <- dat %>% filter(year == 1991, treat_A1 == 1) %>%
  count(cohort)
print(cohort_n)
write_csv(cohort_n, "output/tables/s0_cohort_sizes.csv")

# ---- (S1) Event study in event-time -----------------------------------
# Build event-time dummies: e_{-2}, e_{-1} (omitted), e_{0}, e_{1}, e_{2}.
# For controls (treat_A1==0), e_census is NA -> dummies are 0 (i.e.
# the control group is the reference at every census).
dat_es <- dat %>%
  mutate(e_int = round(e_census))
for (e in c(-2, 0, 1, 2)) {
  dat_es[[paste0("E", ifelse(e<0,"m",""), abs(e))]] <-
    as.integer(!is.na(dat_es$e_int) & dat_es$e_int == e & dat_es$treat_A1 == 1)
}
# omitted: e = -1 (pre-opening census, 1961)
event_terms <- c("Em2", "E0", "E1", "E2")

fit_event_S1 <- function(y) {
  rhs <- c(event_terms, "factor(PRO_COM)", "factor(year)")
  m <- lm(reformulate(rhs, y), data = dat_es)
  with_cluster(m, dat_es$COD_PROV, dat_es)
}
s1_models <- list(
  `log Population`  = fit_event_S1("lP1"),
  `log Local units` = fit_event_S1("lUT"),
  `log Employees`   = fit_event_S1("lAT"))

# Tidy + plot
s1_long <- bind_rows(lapply(seq_along(s1_models), function(i) {
  m <- s1_models[[i]]; nm <- names(s1_models)[i]
  td <- tidy_cr(m) %>% filter(grepl("^E", term) & !grepl("year|PRO_COM|Intercept", term)) %>%
    mutate(event_time = case_when(term == "Em2" ~ -2, term == "E0" ~ 0,
                                  term == "E1"  ~  1, term == "E2" ~ 2),
           outcome = nm)
  bind_rows(td,
            tibble(term = "ref", event_time = -1, outcome = nm,
                   estimate = 0, std.error = NA, statistic = NA,
                   p.value = NA, conf.low = 0, conf.high = 0))
}))

p_s1 <- s1_long %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "#c0392b") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = -2:2,
                     labels = c("-2 (1951)", "-1 (1961, ref)",
                                "0 (1971)", "+1 (1981)", "+2 (1991)")) +
  labs(x = NULL, y = "Event-time coefficient (95% CI)",
       title = "Staggered event study (K7 opening year)",
       subtitle = "Comune + year FE | reference: census just before opening") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("output/figures/fig06_staggered_event_study.png", p_s1,
       width = 9, height = 4.5, dpi = 200)
ggsave("output/figures/fig06_staggered_event_study.pdf", p_s1,
       width = 9, height = 4.5)
write_csv(s1_long, "output/tables/s1_event_study_eventtime.csv")
message("[08] (S1) staggered event study done.")

# ---- (S2) Cohort-by-time interaction ----------------------------------
# For each cohort, estimate the 1961 -> 1991 long DiD effect, separately.
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
         d_lA = lAT_1991 - lAT_1961)

# Reset cohort to "Control" for non-treated
wide <- wide %>%
  mutate(cohort_g = factor(ifelse(treat_A1 == 1, as.character(cohort), "Control"),
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
       title   = "Long DiD by opening cohort (1961-1991)")

# Plot
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
       title = "Treatment effect by opening cohort",
       subtitle = "Long DiD with prov FE + 1961 controls, 95% cluster-robust CI") +
  theme_paper()

ggsave("output/figures/fig07_cohort_effects.png", p_s2,
       width = 8, height = 4, dpi = 200)
ggsave("output/figures/fig07_cohort_effects.pdf", p_s2,
       width = 8, height = 4)

# (NOTE: TMP_60 (ISTAT aree-interne polo travel-time) is intentionally
#  NOT used as treatment intensity here -- the continuous distance
#  variable W2 from R/07_accessibility_did.R is the proper distance
#  measure. TMP_60 is kept only as a heterogeneity stratifier below.)

# ---- (S4) Aree_Int six-band classification --------------------------
# Same idea using the full ISTAT classification:
# A=Polo, B=Polo intercom., C=Cintura, D=Intermedio, E=Periferico, F=Ultraperif.
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
       title   = "Aree-interne six-band DiD (ref = F - Ultraperiferico)")

message("[08] Done.")
