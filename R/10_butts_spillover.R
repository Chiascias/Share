# =====================================================================
# 10_butts_spillover.R  --  spillover-robust CS-DiD (Butts 2023)
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Butts (2023) "Difference-in-Differences Estimation with Spatial
# Spillovers" (J Causal Inference, arXiv:2105.03737) shows that
# standard DiD is biased when SUTVA is violated by spatial spillovers.
# The unbiased estimator includes EXPLICIT SPILLOVER TERMS (ring
# dummies) and identifies:
#
#   * the direct effect on treated units
#   * the indirect/spillover effect on nearby controls
#   * the "true counterfactual" from units beyond the spillover boundary
#
# The Butts framework also provides a TEST for the spillover boundary:
# the smallest distance ring at which the spillover coefficient is
# statistically zero. Units beyond this distance form the valid
# untreated comparison set.
#
# Identification assumption: there exists a distance R such that
# spillover is zero beyond R (the "spillover boundary"). Conditional
# on unit and time FE, ring r ≥ R units evolve in parallel with what
# unit i would have evolved without ANY treatment in its province.
#
# What this script adds vs R/04 + R/05 ---------------------------------
#
# R/04 already estimates ring dummies. The contribution of this script
# is to MAKE EXPLICIT the Butts decomposition:
#
# (B1) Spillover boundary test: starting from R5 (far) and walking
#      inward, test H0: β_r = 0. The smallest r failing the test
#      defines the boundary. Units in rings beyond the boundary are
#      the "clean control".
#
# (B2) Direct + spillover decomposition:
#        Direct effect       = β_{R0}
#        Total spillover     = Σ_{r ≤ boundary} (n_r / n_treated) · β_r
#        Average treatment   = Direct + Total spillover
#                              (aggregate policy effect)
#
# (B3) Spillover-robust CS-DiD: apply the Butts decomposition within
#      each (cohort, post-year) cell of the CS-DiD, then aggregate to
#      ATT(e) spillover-decomposed.
#
# (B4) Spatially clustered SE (placeholder: requires comune centroids
#      for Conley HAC). For now use province-clustered as best
#      feasible alternative.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")
bc    <- readRDS("data/cluster_1961.rds")

ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")

# =====================================================================
# (B1) Spillover boundary identification
# =====================================================================
# Estimate the full ring model on 1961-1991 long DiD, then walk inward
# from R5 testing whether the coefficient is significantly different
# from zero.

wide <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  pivot_wider(id_cols = c(PRO_COM, COD_PROV, COD_REG, macro,
                          treat_A1, ring, W2),
              names_from = year,
              values_from = c(lP1, lUT, lAT,
                              I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961) %>%
  filter(!is.na(d_lP), !is.na(ring))

# Refit the ring spec with R5 as reference
fit_full <- function(y) {
  with_cluster(
    lm(reformulate(c("ring", "factor(COD_PROV)", ctrls), y), data = wide),
    wide$COD_PROV, wide)
}
full_models <- list(`d log Pop`   = fit_full("d_lP"),
                    `d log Units` = fit_full("d_lU"),
                    `d log Emp`   = fit_full("d_lA"))

# Walk-inward boundary test: for each outcome, find the smallest ring
# (innermost in spillover space) where β_r is significantly different
# from 0 at alpha = 0.05.
walk_inward <- function(m, outcome_label) {
  td <- tidy_cr(m) %>% filter(grepl("^ring", term)) %>%
    mutate(ring = sub("ring", "", term),
           order = case_when(
             grepl("R4", ring) ~ 4L,   # 45-60 min, closest to R5 boundary
             grepl("R3", ring) ~ 3L,
             grepl("R2", ring) ~ 2L,
             grepl("R1", ring) ~ 1L,
             grepl("R0", ring) ~ 0L)) %>%
    arrange(desc(order))   # start from R4 (45-60), walk inward
  # Walk
  boundary <- "R5 60+ min (far control)"
  for (i in seq_len(nrow(td))) {
    if (td$p.value[i] < 0.05) {
      boundary <- td$ring[i]
      break
    }
  }
  tibble(outcome = outcome_label, spillover_boundary = boundary,
         td_table = list(td))
}
boundaries <- bind_rows(lapply(seq_along(full_models), function(i) {
  walk_inward(full_models[[i]], names(full_models)[i])
}))
print(boundaries %>% select(outcome, spillover_boundary))
write_csv(boundaries %>% select(outcome, spillover_boundary),
          "output/tables/butts_boundary.csv")

message(glue("\n[10] Spillover-boundary test result:"))
for (i in seq_len(nrow(boundaries))) {
  message(glue("    {boundaries$outcome[i]}: boundary at {boundaries$spillover_boundary[i]}"))
}

# =====================================================================
# (B2) Direct + Spillover decomposition
# =====================================================================
# Given the boundary, decompose:
#   ATT_direct   = β_{R0}
#   ATT_indirect = Σ_{r=1..R_boundary-1} weight_r · β_r
#   ATT_total    = direct + indirect
#
# weights = n_r / n_treated (share of comuni in each spillover ring
# relative to the directly treated group, à la Butts)

ring_sizes <- wide %>% count(ring, name = "n_ring")
n_treated <- sum(wide$treat_A1)

decomp <- function(m, outcome_label, boundary) {
  td <- tidy_cr(m) %>% filter(grepl("^ring", term)) %>%
    mutate(ring = sub("ring", "", term))
  # Always extract R0 (direct)
  direct <- td %>% filter(ring == "R0 inner (casello)")
  # Spillover rings: innermost up to (but not including) the boundary
  spillover_rings <- c("R1 0-15 min (donut)",
                       "R2 15-30 min (donut)",
                       "R3 30-45 min (near control)",
                       "R4 45-60 min (control)")
  # If the boundary is "R5 60+ min..." then all 4 spillover rings count
  # If the boundary is, say, "R3 30-45 min", then we only count R1 + R2
  # (R3 is the boundary, ie. statistically indistinguishable from R5)
  boundary_idx <- match(boundary, c("R1 0-15 min (donut)",
                                     "R2 15-30 min (donut)",
                                     "R3 30-45 min (near control)",
                                     "R4 45-60 min (control)",
                                     "R5 60+ min (far control)"))
  if (is.na(boundary_idx)) boundary_idx <- 5
  active_rings <- spillover_rings[seq_len(boundary_idx - 1)]

  # Sizes
  spill_tab <- td %>% filter(ring %in% active_rings) %>%
    left_join(ring_sizes %>% mutate(ring = as.character(ring)),
              by = "ring") %>%
    mutate(weight = n_ring / n_treated,
           contribution = estimate * weight)

  ATT_direct <- direct$estimate
  SE_direct  <- direct$std.error
  ATT_indirect <- sum(spill_tab$contribution)
  # Variance of weighted sum, assuming independence (approx)
  Var_indirect <- sum((spill_tab$std.error * spill_tab$weight)^2)
  SE_indirect  <- sqrt(Var_indirect)
  ATT_total    <- ATT_direct + ATT_indirect
  # Approx SE of total (treating direct and indirect as independent --
  # conservative, since they share province FE)
  SE_total <- sqrt(SE_direct^2 + Var_indirect)

  tibble(outcome = outcome_label,
         ATT_direct = ATT_direct, SE_direct = SE_direct,
         ATT_indirect = ATT_indirect, SE_indirect = SE_indirect,
         ATT_total = ATT_total, SE_total = SE_total,
         n_spillover_rings = nrow(spill_tab),
         boundary = boundary)
}

decomp_results <- bind_rows(lapply(seq_along(full_models), function(i) {
  decomp(full_models[[i]],
         names(full_models)[i],
         boundaries$spillover_boundary[i])
}))
print(decomp_results)
write_csv(decomp_results, "output/tables/butts_decomposition.csv")

# =====================================================================
# (B3) Spillover-decomposed CS-DiD on the cohort × time grid
# =====================================================================
# For each (cohort g, post-year t), refit the full ring spec on the
# (pre, post) first difference and report the decomposition. Then
# aggregate to ATT(e) direct + spillover.
#
# Identification: same as B1+B2 but evaluated cell by cell.

ai_grid <- bind_rows(
  expand_grid(cohort = "A", pre_year = 1951,
              post_year = c(1961, 1971, 1981, 1991)),
  expand_grid(cohort = "B", pre_year = 1961,
              post_year = c(1971, 1981, 1991))
)

cell_decomp <- function(cohort_lab, pre_year, post_year, outcome) {
  d <- panel %>%
    filter(year %in% c(pre_year, post_year), sample_tight) %>%
    select(PRO_COM, COD_PROV, year, ring, treat_A1, cohort,
           val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val,
                names_prefix = "y") %>%
    mutate(dy = !!sym(paste0("y", post_year)) -
                 !!sym(paste0("y", pre_year)))
  # For this cell, keep only:
  #   * comuni from THIS cohort (in R0)
  #   * never-treated comuni (in R1..R5 for spillover detection)
  d <- d %>% filter(cohort %in% c(cohort_lab, "Never"))
  if (sum(d$treat_A1) < 3) return(NULL)
  m <- with_cluster(lm(dy ~ ring + factor(COD_PROV), data = d),
                    d$COD_PROV, d)
  td <- tidy_cr(m) %>% filter(grepl("^ring", term)) %>%
    mutate(ring = sub("ring", "", term))
  ATT_direct <- td %>% filter(ring == "R0 inner (casello)") %>% pull(estimate)
  SE_direct  <- td %>% filter(ring == "R0 inner (casello)") %>% pull(std.error)
  # Spillover sum (all bands within R5)
  spill <- td %>% filter(ring %in% c("R1 0-15 min (donut)",
                                     "R2 15-30 min (donut)",
                                     "R3 30-45 min (near control)",
                                     "R4 45-60 min (control)")) %>%
    left_join(ring_sizes %>% mutate(ring = as.character(ring)), by = "ring") %>%
    mutate(weight = n_ring / max(1, sum(d$treat_A1)),
           contribution = estimate * weight)
  ATT_ind <- sum(spill$contribution, na.rm = TRUE)
  SE_ind  <- sqrt(sum((spill$std.error * spill$weight)^2, na.rm = TRUE))
  tibble(cohort = cohort_lab, pre_year = pre_year, post_year = post_year,
         outcome = outcome,
         ATT_direct = ATT_direct, SE_direct = SE_direct,
         ATT_indirect = ATT_ind, SE_indirect = SE_ind,
         ATT_total = ATT_direct + ATT_ind)
}

cells <- bind_rows(lapply(c("lP1","lUT","lAT"), function(y) {
  bind_rows(lapply(seq_len(nrow(ai_grid)), function(i) {
    r <- ai_grid[i, ]
    cell_decomp(r$cohort, r$pre_year, r$post_year, y)
  }))
}))
cells <- cells %>% mutate(event_time = case_when(
  cohort == "A" & post_year == 1961 ~ 0,
  cohort == "A" & post_year == 1971 ~ 1,
  cohort == "A" & post_year == 1981 ~ 2,
  cohort == "A" & post_year == 1991 ~ 3,
  cohort == "B" & post_year == 1971 ~ 0,
  cohort == "B" & post_year == 1981 ~ 1,
  cohort == "B" & post_year == 1991 ~ 2))
write_csv(cells, "output/tables/butts_csdid_cells.csv")

# Aggregate over cohorts at each event time
weights_coh <- panel %>% filter(year == 1991, treat_A1 == 1) %>%
  count(cohort, name = "wt")
agg <- cells %>%
  left_join(weights_coh, by = "cohort") %>%
  group_by(outcome, event_time) %>%
  summarise(att_direct   = weighted.mean(ATT_direct,   wt, na.rm = TRUE),
            att_indirect = weighted.mean(ATT_indirect, wt, na.rm = TRUE),
            att_total    = weighted.mean(ATT_total,    wt, na.rm = TRUE),
            se_direct    = sqrt(weighted.mean(SE_direct^2,   wt^2)) / sqrt(n()),
            se_indirect  = sqrt(weighted.mean(SE_indirect^2, wt^2)) / sqrt(n()),
            .groups = "drop") %>%
  mutate(outcome = recode(outcome, "lP1"="log Pop",
                          "lUT"="log Units",
                          "lAT"="log Emp"))
write_csv(agg, "output/tables/butts_att_e_decomposed.csv")
print(agg)

# Plot the direct + spillover decomposition over event-time
p_decomp <- agg %>%
  pivot_longer(c(att_direct, att_indirect, att_total),
               names_to = "component", values_to = "att") %>%
  mutate(se = case_when(component == "att_direct" ~ se_direct,
                        component == "att_indirect" ~ se_indirect,
                        TRUE ~ sqrt(se_direct^2 + se_indirect^2)),
         lo = att - 1.96 * se, hi = att + 1.96 * se,
         component = factor(component,
                            levels = c("att_direct","att_indirect","att_total"),
                            labels = c("Direct effect (R0 inner)",
                                       "Indirect / spillover (R1..R4)",
                                       "Total = direct + spillover"))) %>%
  ggplot(aes(x = event_time, y = att, ymin = lo, ymax = hi,
             colour = component, group = component)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), alpha = 0.5) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_colour_manual(values = c("Direct effect (R0 inner)" = "#c0392b",
                                 "Indirect / spillover (R1..R4)" = "#16a085",
                                 "Total = direct + spillover" = "#2c3e50"),
                      name = NULL) +
  scale_x_continuous(breaks = 0:3) +
  labs(x = "Event time (10y periods)",
       y = "ATT (log points), 95% CI",
       title = "Butts (2023) spillover-decomposed CS-DiD",
       subtitle = "Direct: A1 casello hosts.  Indirect: nearby comuni up to R4.  Total: aggregate policy effect.") +
  theme_paper()
ggsave("output/figures/fig11_butts_decomposition.png", p_decomp,
       width = 11, height = 4.5, dpi = 200)
ggsave("output/figures/fig11_butts_decomposition.pdf", p_decomp,
       width = 11, height = 4.5)

message("[10] Done.")
