# =====================================================================
# 08_aree_interne_spatial.R  --  spatial-staggered DiD, aree interne only
# =====================================================================
#
# Companion to R/07_aree_interne_psm_csdid.R. On the PS-matched aree
# interne sample, run:
#
# (i)  the distance-ring long DiD (à la Ciani-de Blasio) with explicit
#      spillover bands, so we can read off the dose-response of A1
#      access *within the aree-interne stratum*.
# (ii) the W2 continuous-treatment DiD (Donaldson-Hornbeck market-
#      access) on the same matched sample, for a complementary
#      identification.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")
matched_keys <- readRDS("data/matched_aree_interne.rds")

# Restrict the panel to the PS-matched aree-interne sample
panel_m <- panel %>% inner_join(matched_keys, by = "PRO_COM")

# 1961 baseline development covariates we still want to absorb after
# matching (these are NOT in the PS, so they add residual control)
bc <- readRDS("data/cluster_1961.rds")
ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")

# Wide 1961-1991 first-difference frame
wide <- panel_m %>%
  filter(year %in% c(1961, 1991)) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro, treat_A1, ring, W2, year,
         weights, P1, lP1, lUT, lAT,
         I4, SS4, L15, L16, L17) %>%
  pivot_wider(id_cols = c(PRO_COM, COD_PROV, COD_REG, macro,
                          treat_A1, ring, W2, weights),
              names_from = year,
              values_from = c(P1, lP1, lUT, lAT,
                              I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961) %>%
  filter(!is.na(d_lP), !is.na(ring))

cat("Matched aree-interne wide sample at 1961-1991:\n")
print(wide %>% count(treat_A1, ring))

# ---- (i) Ring long DiD ---------------------------------------------
fit_rings <- function(y) {
  with_cluster(
    lm(reformulate(c("ring", "factor(COD_PROV)", ctrls), y),
       data = wide, weights = weights),
    wide$COD_PROV, wide)
}
ring_models <- list(
  `d log Pop`   = fit_rings("d_lP"),
  `d log Units` = fit_rings("d_lU"),
  `d log Emp`   = fit_rings("d_lA"))
regtab(ring_models, keep = "^ring",
       out_csv = "output/tables/ai_psm_rings.csv",
       out_tex = "output/tables/ai_psm_rings.tex",
       title   = "Distance rings on PS-matched aree-interne (1961-1991)")

# ---- (ii) Continuous W2 DiD ----------------------------------------
fit_W2 <- function(y) {
  with_cluster(
    lm(reformulate(c("W2", "factor(COD_PROV)", ctrls), y),
       data = wide, weights = weights),
    wide$COD_PROV, wide)
}
w2_models <- list(
  `d log Pop`   = fit_W2("d_lP"),
  `d log Units` = fit_W2("d_lU"),
  `d log Emp`   = fit_W2("d_lA"))
regtab(w2_models, keep = "^W2$",
       out_csv = "output/tables/ai_psm_W2_continuous.csv",
       out_tex = "output/tables/ai_psm_W2_continuous.tex",
       title   = "Continuous W2 DiD on PS-matched aree-interne (1961-1991)")

# ---- Dose-response plot --------------------------------------------
dose <- bind_rows(lapply(seq_along(ring_models), function(i) {
  m <- ring_models[[i]]; nm <- names(ring_models)[i]
  td <- tidy_cr(m) %>% filter(grepl("^ring", term)) %>%
    mutate(ring = sub("ring", "", term), outcome = nm)
  bind_rows(td,
            tibble(term = "ref",
                   ring = "R5 60+ min (far control)",
                   outcome = nm,
                   estimate = 0, std.error = NA, statistic = NA,
                   p.value = NA, conf.low = 0, conf.high = 0))
})) %>%
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
  scale_colour_manual(values = c("d log Pop"="#2c3e50",
                                 "d log Units"="#16a085",
                                 "d log Emp"="#c0392b"), name = NULL) +
  labs(x = "Distance ring (W2 min from nearest A1 casello)",
       y = "1961-1991 effect vs far ref ring",
       title = "Distance-ring DiD on PS-matched AREE INTERNE",
       subtitle = "Matched sample (D+E+F bands); weighted by 1:3 NN PS weights") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 18, hjust = 1))
ggsave("output/figures/fig10_aree_interne_rings.png", p_dose,
       width = 10, height = 4.5, dpi = 200)
ggsave("output/figures/fig10_aree_interne_rings.pdf", p_dose,
       width = 10, height = 4.5)

message("[08] Done.")
