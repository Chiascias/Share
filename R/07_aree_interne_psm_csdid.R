# =====================================================================
# 07_aree_interne_psm_csdid.R  --  PS-matched CS-DiD on aree interne only
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Restrict to SNAI 2014 "aree interne" = bands D + E + F. Of the 470
# comuni in this stratum (8-region tight sample), 15 are treated by
# A1 and 455 are never-treated. A naive comparison can still be biased
# because the 15 treated are not a random draw of the 470 -- the A1
# bypasses the bulk of the inner-area periphery and only touches
# Apennine-pass / Roma-Napoli leg comuni. We need to match on
# pre-treatment characteristics.
#
# Propensity score, 1951 covariates, NON-OUTCOME ONLY:
#   * Family composition : F1 (avg fam size), A1 (alt fam ratio),
#                          F1_1, F3_1 (other family-type indicators)
#   * Education          : I4 (illiterate share), SS4 (graduate share)
#   * Housing / density  : DensU (dwelling density), Shape_Area
#
# We DELIBERATELY EXCLUDE:
#   * P1, UT, AT, lP1, lUT, lAT  (outcomes)
#   * L15, L16, L17              (sector employment shares -- could be
#                                 considered outcome / industrial
#                                 structure proxies)
#
# Matching: 1:3 nearest-neighbour on the linear PS, with replacement,
# common-support filter at the [0.01, 0.99] quantiles. Then CS-DiD on
# the matched sample, with frequency weights from MatchIt.
#
# Run only after R/01_prepare.R has built the panel.

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(MatchIt)
})

panel <- readRDS("data/panel_long.rds")

# ---- 1.  Pull 1951 covariates ---------------------------------------
# F1 and A1 are time-varying (defined per year). For PS we want the
# 1951 cross-section.
xs <- panel %>% filter(year == 1951) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro,
         F1_1951 = F1, I4_1951 = I4, SS4_1951 = SS4)

# DensU and Shape_Area: read from any year's xlsx since they are
# time-invariant.
suppressMessages({
  raw51 <- read_excel("archivio/1951.xlsx", sheet = "comuni_51")
})
xs_extra <- raw51 %>%
  transmute(PRO_COM,
            A1_1951    = as.numeric(`51_A1`),
            F1_1_1951  = as.numeric(`51_F1_1`),
            F3_1_1951  = as.numeric(`51_F3_1`),
            DensU_1951 = as.numeric(`51_DensU`),
            ShapeArea  = as.numeric(Shape_Area))
xs <- xs %>% left_join(xs_extra, by = "PRO_COM")

# ---- 2.  Restrict to AREE INTERNE (D + E + F) ------------------------
aree_interne_bands <- c("D - Intermedio", "E - Periferico", "F - Ultraperiferico")
ai <- panel %>% filter(year == 1991, sample_tight,
                       Aree_Int %in% aree_interne_bands) %>%
  select(PRO_COM, Aree_Int, cohort, treat_A1) %>%
  left_join(xs, by = "PRO_COM")

cat("Aree interne (D+E+F) in tight A1-province sample:\n")
print(ai %>% count(cohort, Aree_Int))

# ---- 3.  Drop NAs in PS covariates ----------------------------------
ps_vars <- c("F1_1951","A1_1951","F1_1_1951","F3_1_1951",
             "I4_1951","SS4_1951","DensU_1951","ShapeArea")
ai_ps <- ai %>% drop_na(all_of(ps_vars))
cat("After dropping rows with NA in PS covars: n =", nrow(ai_ps),
    "(treated =", sum(ai_ps$treat_A1), ")\n\n")

# ---- 4.  Estimate the propensity score ------------------------------
ps_formula <- as.formula(paste("treat_A1 ~",
                                paste(ps_vars, collapse = " + ")))
ps_fit <- glm(ps_formula, data = ai_ps, family = binomial(link = "logit"))
cat("Propensity score model:\n")
print(summary(ps_fit)$coefficients)

ai_ps$pscore <- predict(ps_fit, type = "response")

# Plot overlap before matching
p_overlap <- ai_ps %>%
  mutate(grp = ifelse(treat_A1 == 1, "Treated (A1 casello)", "Control (never-treated)")) %>%
  ggplot(aes(x = pscore, fill = grp)) +
  geom_density(alpha = 0.45) +
  scale_fill_manual(values = c("Treated (A1 casello)" = "#c0392b",
                               "Control (never-treated)" = "grey60"),
                    name = NULL) +
  labs(x = "Propensity score (predicted P(treat=1))",
       y = "Density",
       title = "Propensity-score overlap on aree interne (D+E+F)",
       subtitle = "Pre-matching density of pscore by treatment status") +
  theme_paper()
ggsave("output/figures/fig08_ps_overlap.png", p_overlap,
       width = 7, height = 4.5, dpi = 200)
ggsave("output/figures/fig08_ps_overlap.pdf", p_overlap,
       width = 7, height = 4.5)

# ---- 5.  Common-support trimming + 1:3 nearest-neighbour matching ---
ps_lo <- quantile(ai_ps$pscore[ai_ps$treat_A1 == 1], 0.01)
ps_hi <- quantile(ai_ps$pscore[ai_ps$treat_A1 == 1], 0.99)
ai_trim <- ai_ps %>% filter(pscore >= ps_lo & pscore <= ps_hi)

m_out <- matchit(ps_formula, data = ai_trim,
                 method = "nearest", ratio = 3, replace = TRUE,
                 distance = "glm", caliper = 0.25)
print(summary(m_out))

matched <- match.data(m_out)
cat("\nMatched sample size:\n")
print(matched %>% count(treat_A1))

# Balance table -- standardised mean differences before/after
bal_tab <- summary(m_out, standardize = TRUE)$sum.matched %>%
  as.data.frame() %>% rownames_to_column("variable")
write_csv(bal_tab, "output/tables/ps_balance.csv")

# ---- 6.  CS-DiD on matched sample (D+E+F aree interne) --------------
# We need per-comune panel data with the matching weights.
matched_keys <- matched %>% select(PRO_COM, weights, any_of("subclass"))
panel_m <- panel %>%
  inner_join(matched_keys, by = "PRO_COM")

# Manual 2x2 CS ATT(g, t) helper, using IPW from `weights` for the
# treated-vs-control contrast.
att_gt_w <- function(panel_df, cohort_lab, pre_year, post_year, outcome) {
  tr <- panel_df %>%
    filter(cohort == cohort_lab, year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, weights, year, val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val, names_prefix = "y") %>%
    mutate(dy    = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 1L)
  ct <- panel_df %>%
    filter(cohort == "Never", year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, weights, year, val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val, names_prefix = "y") %>%
    mutate(dy    = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 0L)
  if (nrow(tr) < 2) return(NULL)
  pool <- bind_rows(tr, ct)
  m <- with_cluster(lm(dy ~ group + factor(COD_PROV),
                       data = pool, weights = weights),
                    pool$COD_PROV, pool)
  td <- tidy_cr(m) %>% filter(term == "group")
  tibble(cohort = cohort_lab, outcome = outcome,
         pre_year = pre_year, post_year = post_year,
         estimate = td$estimate, se = td$std.error,
         lo = td$conf.low, hi = td$conf.high, p = td$p.value,
         n_tr = nrow(tr), n_ct = nrow(ct))
}

grid <- bind_rows(
  expand_grid(cohort = "A", pre_year = 1951,
              post_year = c(1961, 1971, 1981, 1991)),
  expand_grid(cohort = "B", pre_year = 1961,
              post_year = c(1971, 1981, 1991))
)
out_match <- bind_rows(lapply(c("lP1","lUT","lAT"), function(y) {
  bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    r <- grid[i, ]
    att_gt_w(panel_m, r$cohort, r$pre_year, r$post_year, y)
  }))
})) %>% mutate(event_time = case_when(
  cohort == "A" & post_year == 1961 ~ 0,
  cohort == "A" & post_year == 1971 ~ 1,
  cohort == "A" & post_year == 1981 ~ 2,
  cohort == "A" & post_year == 1991 ~ 3,
  cohort == "B" & post_year == 1971 ~ 0,
  cohort == "B" & post_year == 1981 ~ 1,
  cohort == "B" & post_year == 1991 ~ 2
))
write_csv(out_match, "output/tables/ai_psm_csdid_att_gt.csv")

# Aggregate to event-time ATT(e)
weights_coh <- panel_m %>% filter(year == 1991, treat_A1 == 1) %>%
  count(cohort, name = "wt")
agg <- out_match %>%
  filter(!is.na(event_time), !is.na(estimate)) %>%
  left_join(weights_coh, by = "cohort") %>%
  group_by(outcome, event_time) %>%
  summarise(att = weighted.mean(estimate, wt, na.rm = TRUE),
            se  = sqrt(weighted.mean(se^2, wt^2, na.rm = TRUE)) / sqrt(n()),
            .groups = "drop") %>%
  mutate(lo = att - 1.96 * se, hi = att + 1.96 * se,
         outcome = recode(outcome, "lP1"="log Population",
                          "lUT"="log Local units",
                          "lAT"="log Employees"))
write_csv(agg, "output/tables/ai_psm_csdid_att_e.csv")

# Plot
p_att <- agg %>%
  bind_rows(tibble(outcome = unique(agg$outcome),
                   event_time = -1, att = 0, se = NA,
                   lo = 0, hi = 0)) %>%
  ggplot(aes(x = event_time, y = att, ymin = lo, ymax = hi)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "#c0392b") +
  geom_line() + geom_pointrange() +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = -1:3) +
  labs(x = "Event time (10y)", y = "ATT (PS-matched), 95% CI",
       title = "PS-matched CS-DiD on AREE INTERNE only (D + E + F)",
       subtitle = "1:3 NN on 1951 family/educ/housing covariates; never-treated controls") +
  theme_paper()
ggsave("output/figures/fig09_aree_interne_psm_csdid.png", p_att,
       width = 10, height = 4.5, dpi = 200)
ggsave("output/figures/fig09_aree_interne_psm_csdid.pdf", p_att,
       width = 10, height = 4.5)

# Save matched key (so other scripts can re-use the same set)
saveRDS(matched_keys, "data/matched_aree_interne.rds")

message("[07] Done.")
