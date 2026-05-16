# =====================================================================
# 05_spatial_staggered.R  --  the combined CS-DiD x rings spec
# =====================================================================
#
# This is the novelty of the paper: a SPATIAL STAGGERED DiD that
# crosses (i) the Callaway-Sant'Anna staggered identification with
# (ii) the Ciani-de Blasio distance-ring design.
#
# Spec --------------------------------------------------------------
#
# For each ring r ∈ {R0..R5} and each cohort g ∈ {A, B}, compute
# ATT(g, t, r) = E[Y_t - Y_{g_pre} | G=g, ring=r]
#               - E[Y_t - Y_{g_pre} | G=Never, ring=R5]
#
# Aggregate to event-time effects ATT(e, r) by averaging across (g, t)
# pairs at the same event time, separately for each ring. This gives a
# 2-dimensional surface: how does the effect of casello access decay
# with distance AND evolve with event time.
#
# Diagnostic value:
#   * R0 path  -- the "treated" path, should rise post-treatment
#   * R1, R2   -- spillover bands, smaller bumps vs R5 reference
#   * R3, R4   -- near-control, should stay flat (true comparison)
#   * R5       -- reference, by definition 0 everywhere
#
# References ---------------------------------------------------------
#   Callaway & Sant'Anna 2021 -- staggered DiD with cohort-time ATT
#   Ciani & de Blasio 2022    -- spatial DiD with rings
#   Butts 2023                -- DiD with spatial spillovers
#   Borusyak & Hull 2024      -- non-random exposure designs

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- helper: ATT(g, t, r) for one cell --------------------------------
att_gtr <- function(panel_df, cohort_lab, pre_year, post_year, ring_lab, outcome) {
  # treated: this cohort × ring × the two periods
  tr <- panel_df %>%
    filter(cohort == cohort_lab, ring == ring_lab,
           year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, year, val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val,
                names_prefix = "y") %>%
    mutate(dy = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 1L)
  # never-treated comparison in the FAR-CONTROL ring (R5)
  ct <- panel_df %>%
    filter(cohort == "Never",
           ring == "R5 60+ min (far control)",
           year %in% c(pre_year, post_year)) %>%
    select(PRO_COM, COD_PROV, year, val = !!sym(outcome)) %>%
    pivot_wider(names_from = year, values_from = val,
                names_prefix = "y") %>%
    mutate(dy = !!sym(paste0("y", post_year)) - !!sym(paste0("y", pre_year)),
           group = 0L)
  pool <- bind_rows(tr, ct)
  if (sum(pool$group) < 3 || sum(!pool$group) < 5) {
    return(tibble(cohort = cohort_lab, ring = ring_lab, outcome = outcome,
                  pre_year = pre_year, post_year = post_year,
                  estimate = NA_real_, se = NA_real_, lo = NA, hi = NA,
                  p = NA_real_, n_tr = sum(pool$group),
                  n_ct = sum(!pool$group)))
  }
  m <- with_cluster(lm(dy ~ group + factor(COD_PROV), data = pool),
                    pool$COD_PROV, pool)
  td <- tidy_cr(m) %>% filter(term == "group")
  tibble(cohort = cohort_lab, ring = ring_lab, outcome = outcome,
         pre_year = pre_year, post_year = post_year,
         estimate = td$estimate, se = td$std.error,
         lo = td$conf.low, hi = td$conf.high, p = td$p.value,
         n_tr = sum(pool$group), n_ct = sum(!pool$group))
}

# Build the full grid (g, t, r)
ring_levels <- levels(panel$ring)
ring_levels <- ring_levels[ring_levels != "R5 60+ min (far control)"]  # ref

# We treat the cohort label as the COHORT OF EXPOSURE for each comune.
# For controls in non-R5 rings (R1..R4), we DO NOT have a "cohort"
# (they were never directly treated). We model them as never-treated
# in non-far rings -- this is exactly the spillover band Ciani-de
# Blasio quantify. Their pre-year defaults to 1951, post-year to
# 1971/81/91 (we anchor the spillover on the same calendar timing as
# the network rolling out).
#
# For ring R0 (the casello-hosting comuni), there ARE cohorts (A/B).
# For rings R1..R4 (proximity to casello), all are technically Never,
# so we estimate ATT(t, r) = E[Δy_t | ring=r] - E[Δy_t | ring=R5].

panel_ng <- panel %>% mutate(cohort_eff = case_when(
  ring == "R0 inner (casello)" & cohort == "A" ~ "A",
  ring == "R0 inner (casello)" & cohort == "B" ~ "B",
  ring %in% c("R1 0-15 min (donut)","R2 15-30 min (donut)",
              "R3 30-45 min (near control)","R4 45-60 min (control)") ~ "Spillover",
  ring == "R5 60+ min (far control)" ~ "Never",
  TRUE ~ NA_character_))

# For R0: ATT(g, t) as in 03_csdid; for R1..R4: pool across pseudo-cohorts
# Use 1951 -> {1961, 1971, 1981, 1991} as the timeline (we anchor on the
# network roll-out, which mostly opened by 1964 = between 1961 and 1971
# censuses)

run_one <- function(panel_df) {
  # R0 by cohort
  cells <- bind_rows(
    expand_grid(cohort = c("A"),    pre_year = 1951,
                post_year = c(1961, 1971, 1981, 1991),
                ring = "R0 inner (casello)"),
    expand_grid(cohort = c("B"),    pre_year = c(1951, 1961),
                post_year = c(1971, 1981, 1991),
                ring = "R0 inner (casello)") %>% filter(pre_year == 1961),
    # placebo for B (pre-trend test) -- 1951 vs 1961 in ring R0
    tibble(cohort="B", pre_year=1961, post_year=1951, ring="R0 inner (casello)"),
    # R1..R4: treat "Spillover" as a single cohort with pre=1951
    expand_grid(cohort = "Spillover",
                pre_year = 1951,
                post_year = c(1961, 1971, 1981, 1991),
                ring = c("R1 0-15 min (donut)",
                         "R2 15-30 min (donut)",
                         "R3 30-45 min (near control)",
                         "R4 45-60 min (control)"))
  )
  bind_rows(lapply(c("lP1","lUT","lAT"), function(y) {
    bind_rows(lapply(seq_len(nrow(cells)), function(i) {
      r <- cells[i, ]
      att_gtr(panel_df %>% mutate(cohort = ifelse(ring != "R5 60+ min (far control)" &
                                                  cohort == "Never",
                                                  "Spillover", as.character(cohort))),
              r$cohort, r$pre_year, r$post_year, r$ring, y)
    }))
  }))
}

results <- run_one(panel %>% filter(sample_tight))
write_csv(results, "output/tables/spatial_csdid_full.csv")

# ---- Plot: ATT by ring x calendar year ------------------------------
plot_df <- results %>%
  mutate(outcome = recode(outcome, "lP1"="log Pop",
                          "lUT"="log Units",
                          "lAT"="log Employees"),
         ring_short = case_when(
           ring == "R0 inner (casello)"        ~ "R0 inner (casello)",
           ring == "R1 0-15 min (donut)"        ~ "R1 0-15 min",
           ring == "R2 15-30 min (donut)"       ~ "R2 15-30 min",
           ring == "R3 30-45 min (near control)" ~ "R3 30-45 min",
           ring == "R4 45-60 min (control)"     ~ "R4 45-60 min"),
         ring_short = factor(ring_short,
                             levels = c("R0 inner (casello)",
                                        "R1 0-15 min", "R2 15-30 min",
                                        "R3 30-45 min", "R4 45-60 min")))

# Event time mapping (we plot vs calendar year for clarity)
p_st <- plot_df %>%
  filter(!is.na(estimate)) %>%
  ggplot(aes(x = post_year, y = estimate, ymin = lo, ymax = hi,
             colour = ring_short, group = interaction(ring_short, cohort))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 1961, linetype = "dotted", colour = "#16a085") +
  geom_vline(xintercept = 1971, linetype = "dotted", colour = "#c0392b") +
  geom_line(alpha = 0.6) + geom_pointrange(size = 0.4) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_x_continuous(breaks = c(1951, 1961, 1971, 1981, 1991)) +
  scale_colour_brewer(palette = "RdYlBu", direction = -1, name = "Distance ring") +
  labs(x = "Census year",
       y = "ATT vs far-control ring (R5), 95% CI",
       title = "Spatial-staggered DiD: ATT by ring × calendar year",
       subtitle = "R0 = casello hosts (A green / B red lines via shape); R1-R4 = spillover bands; R5 omitted as reference") +
  theme_paper()
ggsave("output/figures/fig06_spatial_staggered.png", p_st,
       width = 12, height = 5, dpi = 200)
ggsave("output/figures/fig06_spatial_staggered.pdf", p_st,
       width = 12, height = 5)

message("[05] Done.")
