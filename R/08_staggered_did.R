# =====================================================================
# 08_staggered_did.R  --  staggered DiD with 2 census cohorts
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# The A1 toll booths opened in four years (K7 ∈ {1959, 1960, 1962,
# 1964}) but at the granularity of the decennial census these collapse
# into TWO cohorts:
#
#   Cohort A  -- K7 in {1959, 1960}, n = 19
#                first observed post-census = 1961
#                event-time map: 1951 -> -1, 1961 -> 0, 1971 -> +1,
#                                1981 -> +2, 1991 -> +3
#
#   Cohort B  -- K7 in {1962, 1963, 1964}, n = 34
#                first observed post-census = 1971
#                event-time map: 1951 -> -2, 1961 -> -1, 1971 -> 0,
#                                1981 -> +1, 1991 -> +2
#
#   Never     -- K0 = 0, comuni in A1-province control set, n ~ 1140
#
# A and B contribute to different event-times in the SAME calendar
# year, which IS the staggered identification the user wants:
#
#   1961 census  ATT(e=0) | A     and  pre-treatment ref | B
#   1971 census  ATT(e=1) | A     and  ATT(e=0) | B
#   1981 census  ATT(e=2) | A     and  ATT(e=1) | B
#   1991 census  ATT(e=3) | A     and  ATT(e=2) | B
#
# Three specifications:
#
# (T1) TWFE staggered event study with event-time dummies. The
#      coefficient on event-time = -2 is the *pre-trends placebo*
#      (1951 for cohort B). Coefficients on e in {0, +1, +2, +3}
#      trace the dynamic treatment effect, pooled across cohorts at
#      each event-time.
#
# (T2) Callaway-Sant'Anna 2x2 ATT(g, t) for each cohort-time pair,
#      computed manually using the "never-treated" comparison
#      group. Then aggregate to ATT(e) by averaging ATT(g, t) over
#      (g, t) pairs at each event-time.
#
# (S2) Long-DiD cohort heterogeneity (1961-1991), kept as before.
# (S4) Aree_Int 6-band heterogeneity.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- cohort assignment (2 census cohorts) ----------------------------
panel <- panel %>%
  mutate(
    cohort_yr = case_when(
      year_open == 1959 ~ "1959",
      year_open == 1960 ~ "1960",
      year_open >= 1961 & year_open <= 1963 ~ "1962",
      year_open >= 1964 ~ "1964",
      TRUE ~ NA_character_),
    cohort_yr = factor(cohort_yr, levels = c("1959","1960","1962","1964")),
    # Map K7 to the FIRST POST-TREATMENT census year. This is what
    # creates staggered identification across calendar years.
    first_post = case_when(
      year_open %in% c(1959, 1960)        ~ 1961,
      year_open %in% c(1962, 1963, 1964)  ~ 1971,
      TRUE                                 ~ NA_real_),
    cohort_census = case_when(
      first_post == 1961 ~ "A (post-1961)",
      first_post == 1971 ~ "B (post-1971)",
      TRUE                ~ "Never"),
    cohort_census = factor(cohort_census,
                           levels = c("Never","A (post-1961)","B (post-1971)")),
    # Event time relative to own first_post (in 10-year units)
    e = ifelse(!is.na(first_post), (year - first_post) / 10, NA_real_)
  )

dat <- panel %>% filter(sample_tight)

# Inspect grid
grid <- dat %>%
  group_by(cohort_census, year) %>%
  summarise(e_unique = paste(sort(unique(e)), collapse = ","),
            n = n(), .groups = "drop")
print(grid)

# ---- (T1) TWFE staggered event study ---------------------------------
# Build event-time dummies E_e * treat. The omitted reference is e=-1
# (the census just before treatment in each cohort's calendar). For
# control comuni e is NA -> all dummies are 0, so they act as the
# always-untreated baseline that the calendar-year FE shifts.

dat_es <- dat %>%
  mutate(e_int = ifelse(treat_A1 == 1, round(e), NA_real_))

# Available event-times across both cohorts: -2, -1, 0, +1, +2, +3
# (e=-2 from B in 1951; e=+3 from A in 1991)
event_e <- c(-2, 0, 1, 2, 3)   # omit -1 = reference
for (ee in event_e) {
  nm <- if (ee < 0) paste0("Em", abs(ee)) else paste0("E", ee)
  dat_es[[nm]] <- as.integer(!is.na(dat_es$e_int) & dat_es$e_int == ee)
}
event_terms <- c("Em2", "E0", "E1", "E2", "E3")

fit_T1 <- function(y) {
  rhs <- c(event_terms, "factor(PRO_COM)", "factor(year)")
  m <- lm(reformulate(rhs, y), data = dat_es)
  with_cluster(m, dat_es$COD_PROV, dat_es)
}
t1_models <- list(
  `log Population`  = fit_T1("lP1"),
  `log Local units` = fit_T1("lUT"),
  `log Employees`   = fit_T1("lAT"))

# Tidy + plot
t1_long <- bind_rows(lapply(seq_along(t1_models), function(i) {
  m <- t1_models[[i]]; nm <- names(t1_models)[i]
  td <- tidy_cr(m) %>%
    filter(term %in% event_terms) %>%
    mutate(event_time = case_when(term == "Em2" ~ -2, term == "E0" ~ 0,
                                  term == "E1"  ~  1, term == "E2" ~ 2,
                                  term == "E3"  ~  3),
           outcome = nm)
  bind_rows(td,
            tibble(term = "ref", event_time = -1, outcome = nm,
                   estimate = 0, std.error = NA, statistic = NA,
                   p.value = NA, conf.low = 0, conf.high = 0))
}))

p_t1 <- t1_long %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "#c0392b") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(
    breaks = -2:3,
    labels = c("-2 (B 1951)", "-1 (ref)", "0 (A 1961, B 1971)",
               "+1 (A 1971, B 1981)", "+2 (A 1981, B 1991)", "+3 (A 1991)")) +
  labs(x = NULL, y = "Event-time coefficient (95% CI)",
       title = "TRUE staggered event study (2 census cohorts)",
       subtitle = "Cohort A: K7 in 1959-60, treated by 1961  |  Cohort B: K7 in 1962-64, treated by 1971") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("output/figures/fig08_staggered_event_study.png", p_t1,
       width = 10, height = 4.5, dpi = 200)
ggsave("output/figures/fig08_staggered_event_study.pdf", p_t1,
       width = 10, height = 4.5)
write_csv(t1_long %>% filter(term != "ref"),
          "output/tables/t1_staggered_event_study.csv")

# Print the headline coefficients
regtab(t1_models, keep = "^(Em|E)[0-9]",
       out_csv = "output/tables/t1_staggered_event_coefs.csv",
       out_tex = "output/tables/t1_staggered_event_coefs.tex",
       title   = "Staggered event study (2 census cohorts)")
message("[08] (T1) TWFE staggered event study done.")

# ---- (T2) Callaway-Sant'Anna 2x2 ATT(g, t), manual implementation ----
# For each (g, t) with t >= first_post(g): compute
#    ATT(g, t) = E[Y_t - Y_{pre} | G=g] - E[Y_t - Y_{pre} | never-treated]
# where pre is the LAST CENSUS BEFORE g's treatment:
#   * cohort A (first_post=1961): pre = 1951
#   * cohort B (first_post=1971): pre = 1961

# Long form, restricted to never-treated + each cohort separately
make_att <- function(y, cohort_lab, pre_year, post_year) {
  # Treated rows for this cohort
  tr <- dat %>% filter(cohort_census == cohort_lab,
                       year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, year, val = !!sym(y))
  # Never-treated rows
  ct <- dat %>% filter(cohort_census == "Never",
                       year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, year, val = !!sym(y))
  # First differences
  tr_d <- tr %>% pivot_wider(names_from = year, values_from = val,
                              names_prefix = "y") %>%
    mutate(dy = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)))
  ct_d <- ct %>% pivot_wider(names_from = year, values_from = val,
                              names_prefix = "y") %>%
    mutate(dy = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)))
  att <- mean(tr_d$dy, na.rm = TRUE) - mean(ct_d$dy, na.rm = TRUE)
  # cluster-robust SE via simple 2x2 DiD regression with prov FE
  panel_gt <- bind_rows(
    tr_d %>% transmute(PRO_COM, group = 1L, dy),
    ct_d %>% transmute(PRO_COM, group = 0L, dy)) %>%
    left_join(dat %>% distinct(PRO_COM, COD_PROV), by = "PRO_COM")
  m <- with_cluster(lm(dy ~ group + factor(COD_PROV), data = panel_gt),
                    panel_gt$COD_PROV, panel_gt)
  td <- tidy_cr(m) %>% filter(term == "group")
  tibble(cohort = cohort_lab, pre = pre_year, post = post_year,
         outcome = y, estimate = td$estimate, se = td$std.error,
         lo = td$conf.low, hi = td$conf.high, p = td$p.value,
         n_tr = nrow(tr_d), n_ct = nrow(ct_d))
}

cs_grid <- list(
  list(coh = "A (post-1961)", pre = 1951, post = 1961),  # e=0 for A
  list(coh = "A (post-1961)", pre = 1951, post = 1971),  # e=+1 for A
  list(coh = "A (post-1961)", pre = 1951, post = 1981),  # e=+2 for A
  list(coh = "A (post-1961)", pre = 1951, post = 1991),  # e=+3 for A
  list(coh = "B (post-1971)", pre = 1961, post = 1971),  # e=0 for B
  list(coh = "B (post-1971)", pre = 1961, post = 1981),  # e=+1 for B
  list(coh = "B (post-1971)", pre = 1961, post = 1991)   # e=+2 for B
)

cs_results <- bind_rows(lapply(c("lP1","lUT","lAT"), function(y) {
  bind_rows(lapply(cs_grid, function(x)
    make_att(y, x$coh, x$pre, x$post)))
}))

# Aggregate to event-time effects: ATT(e) = mean over (g, t) at this e,
# weighted by cohort size.
weights <- dat %>% filter(year == 1991, cohort_census != "Never") %>%
  count(cohort_census, name = "wt")
cs_results <- cs_results %>%
  mutate(event_time = case_when(
    cohort == "A (post-1961)" & post == 1961 ~ 0,
    cohort == "A (post-1961)" & post == 1971 ~ 1,
    cohort == "A (post-1961)" & post == 1981 ~ 2,
    cohort == "A (post-1961)" & post == 1991 ~ 3,
    cohort == "B (post-1971)" & post == 1971 ~ 0,
    cohort == "B (post-1971)" & post == 1981 ~ 1,
    cohort == "B (post-1971)" & post == 1991 ~ 2))

cs_agg <- cs_results %>%
  left_join(weights, by = c("cohort" = "cohort_census")) %>%
  group_by(outcome, event_time) %>%
  summarise(att   = weighted.mean(estimate, wt, na.rm = TRUE),
            se    = sqrt(weighted.mean(se^2, wt^2, na.rm = TRUE)) /
                    sqrt(n()),  # rough, conservative
            n_pairs = n(),
            .groups = "drop") %>%
  mutate(lo = att - 1.96*se, hi = att + 1.96*se)

write_csv(cs_results, "output/tables/t2_cs_2x2_atts.csv")
write_csv(cs_agg,     "output/tables/t2_cs_event_time_aggregated.csv")

# Plot the aggregated CS event-time effects
p_t2 <- cs_agg %>%
  mutate(outcome = recode(outcome, "lP1" = "log Population",
                          "lUT" = "log Local units",
                          "lAT" = "log Employees")) %>%
  ggplot(aes(x = event_time, y = att, ymin = lo, ymax = hi)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange() +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = 0:3) +
  labs(x = "Event-time (10-year periods)", y = "ATT (Callaway-Sant'Anna, 2x2)",
       title = "Callaway-Sant'Anna ATT(e) -- aggregated across cohorts A and B",
       subtitle = "Each e uses the never-treated as comparison group; pre = census just before each cohort's treatment") +
  theme_paper()
ggsave("output/figures/fig09_cs_event_time.png", p_t2,
       width = 10, height = 4, dpi = 200)
ggsave("output/figures/fig09_cs_event_time.pdf", p_t2,
       width = 10, height = 4)
message("[08] (T2) Callaway-Sant'Anna ATT(g,t) done.")

# (Old S1 block removed: the calendar-time event study lives in
#  R/04_spatial_did.R as M2.  The TRUE staggered version is T1 above.)

# ---- (S2) Cohort heterogeneity in long DiD (NOT staggered) -----------
ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")
wide <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  pivot_wider(id_cols = c(PRO_COM, COD_PROV, COD_REG, macro, treat_A1,
                          cohort_yr, year_open, Aree_Int),
              names_from = year,
              values_from = c(P1, lP1, lUT, lAT, emp_rate,
                              I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961,
         cohort_g = factor(ifelse(treat_A1 == 1, as.character(cohort_yr), "Control"),
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
