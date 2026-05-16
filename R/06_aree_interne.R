# =====================================================================
# 06_aree_interne.R  --  focus on ISTAT inner-areas classification
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# ISTAT's "Aree Interne" classification (since SNAI 2014) ranks every
# Italian comune by travel-time to the nearest service POLO (a hub
# providing first-aid hospital + secondary school + Silver-class
# railway station):
#
#   A  Polo                       (host of services)
#   B  Polo intercomunale         (shared services)
#   C  Cintura                    (< 27.7 min from polo)
#   D  Intermedio                 (27.7 - 40.9 min)
#   E  Periferico                 (40.9 - 66.9 min)
#   F  Ultraperiferico            (> 66.9 min)
#
# This is the policy categorisation behind the National Strategy for
# Inner Areas (SNAI 2014) and the PNRR M5C3 territorial cohesion line.
#
# The question this script asks: did the A1 motorway opening
# CONTRIBUTE TO creating today's Aree Interne hierarchy, or did it
# simply REINFORCE existing patterns?
#
# Three pieces:
#
# (a) Cross-tab: how does cohort A/B treatment status correlate with
#     today's aree-interne class?
# (b) CS-DiD ATT(g, t) within each aree-interne band -- which bands
#     gained more from A1 access?
# (c) Heterogeneous ATT(e) by band: did peripheral comuni gain less
#     than central ones?  (Puga 2002 polarisation hypothesis)

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- (a) Cross-tab cohort x aree-interne ----------------------------
xt <- panel %>% filter(year == 1991, sample_tight, !is.na(Aree_Int)) %>%
  count(cohort, Aree_Int) %>%
  pivot_wider(names_from = Aree_Int, values_from = n, values_fill = 0)
write_csv(xt, "output/tables/aree_int_crosstab.csv")
print(xt)

# ---- (b) CS-DiD restricted to each aree-interne band ----------------
att_gt_in_band <- function(panel_df, cohort_lab, pre_year, post_year,
                            band, outcome) {
  tr <- panel_df %>%
    filter(cohort == cohort_lab, Aree_Int == band,
           year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, year, val = !!sym(outcome))
  # Require at least 2 comuni × 2 periods on the treated side
  n_tr_comuni <- length(unique(tr$PRO_COM))
  if (n_tr_comuni < 2) {
    return(tibble(cohort=cohort_lab, band=band, outcome=outcome,
                  pre_year=pre_year, post_year=post_year,
                  estimate=NA_real_, se=NA_real_, lo=NA_real_, hi=NA_real_,
                  p=NA_real_, n_tr=n_tr_comuni, n_ct=NA_integer_))
  }
  tr <- tr %>%
    pivot_wider(names_from = year, values_from = val, names_prefix = "y") %>%
    mutate(dy = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 1L)
  ct <- panel_df %>%
    filter(cohort == "Never",
           year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, year, val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val, names_prefix = "y") %>%
    mutate(dy = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 0L)
  pool <- bind_rows(tr, ct)
  m <- with_cluster(lm(dy ~ group + factor(COD_PROV), data = pool),
                    pool$COD_PROV, pool)
  td <- tidy_cr(m) %>% filter(term == "group")
  tibble(cohort=cohort_lab, band=band, outcome=outcome,
         pre_year=pre_year, post_year=post_year,
         estimate=td$estimate, se=td$std.error,
         lo=td$conf.low, hi=td$conf.high, p=td$p.value,
         n_tr=nrow(tr), n_ct=nrow(ct))
}

bands <- panel %>% filter(!is.na(Aree_Int)) %>% pull(Aree_Int) %>% unique() %>% sort()
grid <- expand_grid(
  cohort = c("A","B"),
  band   = bands) %>%
  mutate(pre_year = ifelse(cohort == "A", 1951, 1961))
out_band <- bind_rows(lapply(c("lP1","lUT","lAT"), function(y) {
  bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    r <- grid[i, ]
    # Use 1991 as the long-run post census for both cohorts
    att_gt_in_band(panel %>% filter(sample_donut30),
                   r$cohort, r$pre_year, 1991, r$band, y)
  }))
}))
write_csv(out_band, "output/tables/aree_int_csdid.csv")

# ---- (c) Plot: ATT by aree-interne band -----------------------------
p_band <- out_band %>%
  filter(!is.na(estimate)) %>%
  mutate(outcome = recode(outcome, "lP1"="log Pop",
                          "lUT"="log Units",
                          "lAT"="log Employees"),
         band = factor(band,
                       levels = c("F - Ultraperiferico",
                                  "E - Periferico",
                                  "D - Intermedio",
                                  "C - Cintura",
                                  "B - Polo intercomunale",
                                  "A - Polo"))) %>%
  ggplot(aes(x = band, y = estimate, ymin = lo, ymax = hi,
             colour = cohort, shape = cohort, group = cohort)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(0.4)) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_colour_manual(values = c("A"="#16a085", "B"="#c0392b"), name = NULL) +
  scale_shape_manual(values = c("A"=16, "B"=17), name = NULL) +
  labs(x = NULL, y = "ATT (1991 vs cohort's pre-census)",
       title = "Long-run ATT by aree-interne band",
       subtitle = "Within-band CS-DiD; donut-30 sample") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))
ggsave("output/figures/fig07_aree_interne_csdid.png", p_band,
       width = 11, height = 5, dpi = 200)
ggsave("output/figures/fig07_aree_interne_csdid.pdf", p_band,
       width = 11, height = 5)

message("[06] Done.")
