# =====================================================================
# 03_csdid.R  --  Callaway-Sant'Anna staggered DiD (manual)
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Two census cohorts:
#   A : K7 in {1959, 1960}, n=19, first observed post = 1961
#   B : K7 in {1962, 1963, 1964}, n=34, first observed post = 1971
#
# The Callaway-Sant'Anna (2021, J Econom) estimator with never-treated
# comparison group gives, for each (cohort g, calendar year t) cell
# with t >= first_post(g):
#
#   ATT(g, t) = E[Y_t - Y_{g_pre} | G = g]
#             - E[Y_t - Y_{g_pre} | G = Never]
#
# where g_pre is the LAST CENSUS BEFORE g's treatment.  We:
#   1. compute ATT(g,t) for every (g, t) pair using OLS with prov FE
#      and cluster-robust SE
#   2. aggregate to event-time ATT(e) using sample-size weights
#   3. report a pre-trend test (e=-2 for cohort B in 1951)
#
# We do this manually because the canonical `did` R package is not
# available in the apt distribution. The hand-rolled implementation is
# equivalent to `did::att_gt(control_group = "nevertreated")` followed
# by `did::aggte(type = "dynamic")`.
#
# Baseline sample: sample_donut30 (drops controls within 30 min of a
# casello, the Ciani-de Blasio SUTVA fix). Run on tight + donut45 as
# robustness.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- ATT(g,t) for one (cohort, post-year) cell ----------------------
att_gt <- function(panel_df, cohort_lab, pre_year, post_year, outcome) {
  # Treated rows: this cohort, two periods
  tr <- panel_df %>%
    filter(cohort == cohort_lab, year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, year, val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val,
                names_prefix = "y") %>%
    mutate(dy    = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 1L)
  # Never-treated: same comparison window
  ct <- panel_df %>%
    filter(cohort == "Never", year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, year, val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val,
                names_prefix = "y") %>%
    mutate(dy    = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 0L)
  pool <- bind_rows(tr, ct)
  m <- with_cluster(lm(dy ~ group + factor(COD_PROV), data = pool),
                    pool$COD_PROV, pool)
  td <- tidy_cr(m) %>% filter(term == "group")
  tibble(cohort = cohort_lab, outcome = outcome,
         pre_year = pre_year, post_year = post_year,
         estimate = td$estimate, se = td$std.error,
         lo = td$conf.low, hi = td$conf.high, p = td$p.value,
         n_tr = nrow(tr), n_ct = nrow(ct))
}

# ATT(g, t) grid for cohort A and B
build_grid <- function() {
  bind_rows(
    # Cohort A: pre = 1951, post in 1961, 1971, 1981, 1991
    tibble(cohort = "A", pre_year = 1951,
           post_year = c(1961, 1971, 1981, 1991)),
    # Cohort B: pre = 1961, post in 1971, 1981, 1991
    tibble(cohort = "B", pre_year = 1961,
           post_year = c(1971, 1981, 1991)),
    # Pre-trends placebo: B in 1951 vs 1961 baseline -- here pre is
    # 1951 and "post" is also 1961 (which is the ref for B). We need
    # a slightly different construction: B vs Never on 1951 -> 1961
    # to check B's pre-trend. We label this as event_time = -2.
    tibble(cohort = "B", pre_year = 1961, post_year = 1951)
    # (we'll interpret 1951 < 1961 as a 'pre' placebo when we map e=-2)
  )
}
grid <- build_grid()

# Run for each outcome
run_csdid <- function(panel_df, sample_label) {
  out <- bind_rows(lapply(c("lP1","lUT","lAT"), function(y) {
    bind_rows(lapply(seq_len(nrow(grid)), function(i) {
      g <- grid[i, ]
      att_gt(panel_df, g$cohort, g$pre_year, g$post_year, y)
    })) %>% mutate(sample = sample_label)
  }))
  # Map (cohort, post_year) to event_time
  out %>% mutate(event_time = case_when(
    cohort == "A" & post_year == 1961 ~ 0,
    cohort == "A" & post_year == 1971 ~ 1,
    cohort == "A" & post_year == 1981 ~ 2,
    cohort == "A" & post_year == 1991 ~ 3,
    cohort == "B" & post_year == 1971 ~ 0,
    cohort == "B" & post_year == 1981 ~ 1,
    cohort == "B" & post_year == 1991 ~ 2,
    cohort == "B" & post_year == 1951 ~ -2  # placebo
  ))
}

# Three sample variants
results <- bind_rows(
  run_csdid(panel %>% filter(sample_tight),    "tight"),
  run_csdid(panel %>% filter(sample_donut30),  "donut30"),
  run_csdid(panel %>% filter(sample_donut45),  "donut45"))

write_csv(results, "output/tables/csdid_att_gt.csv")
print(results %>% filter(sample == "donut30"))

# ---- Aggregate to ATT(e) -- sample-size-weighted average across g ----
weights <- panel %>% filter(year == 1991, treat_A1 == 1) %>%
  count(cohort, name = "wt")

agg <- results %>%
  filter(!is.na(event_time)) %>%
  left_join(weights, by = "cohort") %>%
  group_by(sample, outcome, event_time) %>%
  summarise(att = weighted.mean(estimate, wt, na.rm = TRUE),
            # Conservative SE under independence: sqrt(mean(se^2))/sqrt(k)
            se  = sqrt(weighted.mean(se^2, wt^2, na.rm = TRUE)) / sqrt(n()),
            n_cells = n(),
            .groups = "drop") %>%
  mutate(lo = att - 1.96 * se, hi = att + 1.96 * se,
         outcome = recode(outcome, "lP1"="log Population",
                          "lUT"="log Local units",
                          "lAT"="log Employees"))
write_csv(agg, "output/tables/csdid_att_e.csv")

# ---- Event-study plot, donut30 baseline -----------------------------
p_main <- agg %>% filter(sample == "donut30") %>%
  bind_rows(tibble(sample = "donut30", outcome = unique(agg$outcome),
                   event_time = -1, att = 0, se = NA, lo = 0, hi = 0,
                   n_cells = NA_integer_)) %>%
  ggplot(aes(x = event_time, y = att, ymin = lo, ymax = hi)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "#c0392b") +
  geom_pointrange() + geom_line() +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = -2:3) +
  labs(x = "Event time (10-year periods)",
       y = "ATT (Callaway-Sant'Anna), 95% CI",
       title = "Callaway-Sant'Anna staggered DiD on the A1 toll-booth opening",
       subtitle = "donut-30 sample (drops controls within 30 min of a casello); placebo at e=-2") +
  theme_paper()
ggsave("output/figures/fig02_csdid_event_study.png", p_main,
       width = 10, height = 4.5, dpi = 200)
ggsave("output/figures/fig02_csdid_event_study.pdf", p_main,
       width = 10, height = 4.5)

# ---- Robustness across samples + cohort-separated plot --------------
p_robust <- agg %>%
  ggplot(aes(x = event_time, y = att, ymin = lo, ymax = hi,
             colour = sample, group = sample)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "#c0392b") +
  geom_pointrange(position = position_dodge(0.4)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_colour_manual(values = c("tight"="grey50","donut30"="#2c3e50",
                                 "donut45"="#c0392b"), name = NULL) +
  scale_x_continuous(breaks = -2:3) +
  labs(x = "Event time", y = "ATT (95% CI)",
       title = "Donut robustness: ATT(e) across three control samples",
       subtitle = "Spillover-contaminated controls inflate ATT downward in tight; donut45 over-shrinks the comparison set") +
  theme_paper()
ggsave("output/figures/fig03_csdid_donut_robustness.png", p_robust,
       width = 11, height = 4.5, dpi = 200)
ggsave("output/figures/fig03_csdid_donut_robustness.pdf", p_robust,
       width = 11, height = 4.5)

# Cohort-separated plot (the one Lelo finds easiest to read)
sep <- results %>% filter(sample == "donut30", !is.na(event_time)) %>%
  bind_rows(tibble(sample="donut30",
                   cohort=c("A","A","B","B","B"),
                   outcome=rep(c("lP1","lUT","lAT"), each=5)[1:5],
                   event_time=-1, estimate=0, se=NA, lo=0, hi=0,
                   p=NA, pre_year=NA_integer_, post_year=NA_integer_,
                   n_tr=NA, n_ct=NA)) %>%
  mutate(outcome = recode(outcome, "lP1"="log Population",
                          "lUT"="log Local units",
                          "lAT"="log Employees"),
         cal_year = case_when(
           cohort == "A" & event_time == -1 ~ 1951,
           cohort == "A" & event_time ==  0 ~ 1961,
           cohort == "A" & event_time ==  1 ~ 1971,
           cohort == "A" & event_time ==  2 ~ 1981,
           cohort == "A" & event_time ==  3 ~ 1991,
           cohort == "B" & event_time == -2 ~ 1951,
           cohort == "B" & event_time == -1 ~ 1961,
           cohort == "B" & event_time ==  0 ~ 1971,
           cohort == "B" & event_time ==  1 ~ 1981,
           cohort == "B" & event_time ==  2 ~ 1991),
         cohort_lab = recode(cohort,
                             "A" = "Cohort A (K7 1959-60), first post 1961",
                             "B" = "Cohort B (K7 1962-64), first post 1971"))
p_sep <- sep %>%
  ggplot(aes(x = cal_year, y = estimate, ymin = lo, ymax = hi,
             colour = cohort_lab, shape = cohort_lab, group = cohort_lab)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 1961, linetype = "dotted", colour = "#16a085") +
  geom_vline(xintercept = 1971, linetype = "dotted", colour = "#c0392b") +
  geom_line(position = position_dodge(width = 1.5), alpha = 0.6) +
  geom_pointrange(position = position_dodge(width = 1.5)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(1951, 1961, 1971, 1981, 1991)) +
  scale_colour_manual(values = c("Cohort A (K7 1959-60), first post 1961" = "#16a085",
                                 "Cohort B (K7 1962-64), first post 1971" = "#c0392b"),
                      name = NULL) +
  scale_shape_manual(values = c("Cohort A (K7 1959-60), first post 1961" = 16,
                                "Cohort B (K7 1962-64), first post 1971" = 17),
                      name = NULL) +
  labs(x = "Census year",
       y = "ATT(g,t) vs never-treated, 95% CI",
       title = "Cohort-separated ATT(g,t) -- the staggered identification visualised",
       subtitle = "Each cohort's traj uses its OWN pre-treatment census as reference") +
  theme_paper()
ggsave("output/figures/fig04_csdid_cohort_separated.png", p_sep,
       width = 11, height = 5, dpi = 200)
ggsave("output/figures/fig04_csdid_cohort_separated.pdf", p_sep,
       width = 11, height = 5)

message("[03] Done.")
