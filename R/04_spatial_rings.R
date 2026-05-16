# =====================================================================
# 04_spatial_rings.R  --  Ciani-de Blasio distance rings
# =====================================================================
#
# The spatial-DiD component, with EXPLICIT spillover bands instead of
# a donut (more informative because it MEASURES spillover instead of
# excluding it):
#
#   R0  inner ring  : hosts the casello (W2 ≈ 0 - 15 min, n=53)
#   R1  donut 1     : 0-15 min from casello  (n_ctrl = 159)
#   R2  donut 2     : 15-30 min              (n_ctrl = 421)
#   R3  near ctrl   : 30-45 min              (n_ctrl = 320)
#   R4  control     : 45-60 min              (n_ctrl = 136)
#   R5  far control : 60+ min  (reference)   (n_ctrl =  98)
#
# All five non-reference rings appear as dummies in the regression. The
# coefficients trace the dose-response of distance from casello.
# Spillover is *quantified* by the size of the donut coefficients (R1
# and R2): if they are positive and significant, controls within 30 min
# are partly treated.
#
# Two specs:
#
# (R1) Long DiD on first differences 1961 -> 1991:
#        Δlog y_i = a + Σ_r β_r * 1{ring = r} + γ'X^{1961}_i
#                   + δ_{prov(i)} + ε_i
#      The far ring (R5) is the omitted reference. β_0 (inner) is the
#      pure A1 effect; β_1 + β_2 are the spillover; β_3 + β_4 trace the
#      distance decay among controls.
#
# (R2) Continuous-treatment DiD: log y_{it} on W2 × year_FE
#      (already in old R/07 -- kept here as W2-continuous spec).

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")
bc    <- readRDS("data/cluster_1961.rds")

# ---- (R1) Long DiD with ring dummies --------------------------------
wide <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro, treat_A1, ring, W2, year,
         P1, lP1, lUT, lAT, emp_rate,
         I4, SS4, L15, L16, L17) %>%
  pivot_wider(names_from = year,
              values_from = c(P1, lP1, lUT, lAT, emp_rate,
                              I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961) %>%
  left_join(bc, by = "PRO_COM") %>%
  filter(!is.na(d_lP), !is.na(ring))

ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")

fit_rings <- function(y) {
  with_cluster(
    lm(reformulate(c("ring", "factor(COD_PROV)", ctrls), y), data = wide),
    wide$COD_PROV, wide)
}
r1_models <- list(
  `Δ log Pop`   = fit_rings("d_lP"),
  `Δ log Units` = fit_rings("d_lU"),
  `Δ log Emp`   = fit_rings("d_lA"))
regtab(r1_models, keep = "^ring",
       out_csv = "output/tables/rings_long_did.csv",
       out_tex = "output/tables/rings_long_did.tex",
       title   = "Distance-ring DiD 1961-1991 (ref = R5 60+ min far control)")

# Dose-response plot (each ring vs the R5 reference)
dose <- bind_rows(lapply(seq_along(r1_models), function(i) {
  m <- r1_models[[i]]; nm <- names(r1_models)[i]
  td <- tidy_cr(m) %>% filter(grepl("^ring", term)) %>%
    mutate(ring = sub("ring", "", term), outcome = nm)
  bind_rows(td,
            tibble(term = "ref",
                   ring = "R5 60+ min (far control)",
                   outcome = nm,
                   estimate = 0, std.error = NA, statistic = NA,
                   p.value = NA, conf.low = 0, conf.high = 0))
}))
dose <- dose %>%
  mutate(ring = factor(ring,
                       levels = c("R5 60+ min (far control)",
                                  "R4 45-60 min (control)",
                                  "R3 30-45 min (near control)",
                                  "R2 15-30 min (donut)",
                                  "R1 0-15 min (donut)",
                                  "R0 inner (casello)")))
p_dose <- ggplot(dose, aes(x = ring, y = estimate,
                           ymin = conf.low, ymax = conf.high,
                           colour = outcome, group = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(0.3)) +
  scale_colour_manual(values = c("Δ log Pop"="#2c3e50",
                                 "Δ log Units"="#16a085",
                                 "Δ log Emp"="#c0392b"), name = NULL) +
  labs(x = "Distance ring (W2 in min from nearest A1 casello)",
       y = "1961-1991 effect vs far ref ring",
       title = "Distance-ring dose-response, Ciani-de Blasio design",
       subtitle = "R1 and R2 ARE the spillover bands -- their size measures SUTVA contamination") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))
ggsave("output/figures/fig05_rings_dose_response.png", p_dose,
       width = 10, height = 4.5, dpi = 200)
ggsave("output/figures/fig05_rings_dose_response.pdf", p_dose,
       width = 10, height = 4.5)

# ---- (R2) Continuous W2 (for completeness) --------------------------
fit_cont <- function(y) {
  with_cluster(
    lm(reformulate(c("W2", "factor(COD_PROV)", ctrls), y), data = wide),
    wide$COD_PROV, wide)
}
r2_models <- list(
  `Δ log Pop`   = fit_cont("d_lP"),
  `Δ log Units` = fit_cont("d_lU"),
  `Δ log Emp`   = fit_cont("d_lA"))
regtab(r2_models, keep = "^W2$",
       out_csv = "output/tables/rings_continuous_W2.csv",
       out_tex = "output/tables/rings_continuous_W2.tex",
       title   = "Continuous W2 DiD 1961-1991 (per-minute coefficients)")

message("[04] Done.")
