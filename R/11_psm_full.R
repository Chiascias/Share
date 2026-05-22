# =====================================================================
# 11_psm_full.R  --  Propensity score matching FULL SAMPLE, 1:1
# =====================================================================
#
# User's request: rerun the PS matching from scratch on the FULL
# eight-region sample (not just aree interne), with 1:1 matching as
# the baseline and a comparison across alternative matching options.
#
# Covariates 1951 (non-outcome):
#   * Family composition : F1, A1, F1_1, F3_1
#   * Education          : I4, SS4
#   * Housing / density  : DensU, Shape_Area
#
# Matching alternatives compared:
#   (M1) 1:1 nearest-neighbour with replacement
#   (M2) 1:1 nearest-neighbour WITHOUT replacement
#   (M3) 1:3 NN with replacement (current baseline)
#   (M4) 1:5 NN with replacement
#   (M5) 1:1 NN with caliper 0.25 SD of PS
#   (M6) Exact matching on macro-area + 1:1 NN within strata

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(MatchIt)
})

panel <- readRDS("data/panel_long.rds")

# ---- 1. Build 1951 PS covariate set ---------------------------------
xs <- panel %>% filter(year == 1951) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro,
         F1_1951 = F1, I4_1951 = I4, SS4_1951 = SS4)

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

# ---- 2. Pull treatment indicator (K0=1 from cross-section) ----------
treat_xs <- panel %>% filter(year == 1991) %>%
  distinct(PRO_COM, treat_A1, cohort, Aree_Int)

full <- xs %>% left_join(treat_xs, by = "PRO_COM") %>%
  mutate(treat_A1 = coalesce(treat_A1, 0L))

cat("Step 1 — full 1951 sample loaded:\n")
cat(sprintf("  n total          : %d\n", nrow(full)))
cat(sprintf("  n treated (K0=1) : %d\n", sum(full$treat_A1)))
cat(sprintf("  n control        : %d\n", sum(full$treat_A1 == 0)))

# ---- 3. Restrict to same-province controls (sample_tight analogue) ---
treated_provs <- full %>% filter(treat_A1 == 1) %>% pull(COD_PROV) %>% unique()
tight <- full %>% filter(COD_PROV %in% treated_provs)
cat("\nStep 2 — restrict to provinces hosting at least one casello:\n")
cat(sprintf("  treated provinces        : %d\n", length(treated_provs)))
cat(sprintf("  n total in those prov    : %d\n", nrow(tight)))
cat(sprintf("  n treated                : %d\n", sum(tight$treat_A1)))
cat(sprintf("  n control (same prov)    : %d\n", sum(tight$treat_A1 == 0)))

# ---- 4. Drop missings in PS covariates -------------------------------
ps_vars <- c("F1_1951","A1_1951","F1_1_1951","F3_1_1951",
             "I4_1951","SS4_1951","DensU_1951","ShapeArea")
psd <- tight %>% drop_na(all_of(ps_vars))
cat("\nStep 3 — drop rows with missing PS covariates:\n")
cat(sprintf("  n with complete covariates : %d (treated %d)\n",
            nrow(psd), sum(psd$treat_A1)))

# ---- 5. Estimate the propensity score (single model) ----------------
ps_formula <- as.formula(paste("treat_A1 ~",
                                paste(ps_vars, collapse = " + ")))
ps_fit <- glm(ps_formula, data = psd, family = binomial(link = "logit"))
psd$pscore <- predict(ps_fit, type = "response")

cat("\nStep 4 — propensity score model coefficients:\n")
print(round(summary(ps_fit)$coefficients, 4))

# Overlap stats
cat("\n  Pscore distribution by treatment status:\n")
print(psd %>% group_by(treat_A1) %>%
        summarise(n = n(),
                  min = min(pscore), p25 = quantile(pscore, .25),
                  median = median(pscore), mean = mean(pscore),
                  p75 = quantile(pscore, .75), max = max(pscore)))

# ---- 6. Common-support trim -----------------------------------------
ps_lo <- quantile(psd$pscore[psd$treat_A1 == 1], 0.01)
ps_hi <- quantile(psd$pscore[psd$treat_A1 == 1], 0.99)
trim <- psd %>% filter(pscore >= ps_lo & pscore <= ps_hi)
cat(sprintf("\nStep 5 — common-support trim at [.01, .99] of treated pscore:\n"))
cat(sprintf("  range kept: [%.4f, %.4f]\n", ps_lo, ps_hi))
cat(sprintf("  n after trim : %d (treated %d, control %d)\n",
            nrow(trim), sum(trim$treat_A1), sum(trim$treat_A1 == 0)))

# ---- 7. Run all matching variants ------------------------------------
run_match <- function(.label, ...) {
  m <- tryCatch(matchit(ps_formula, data = trim, ...),
                error = function(e) {
                  message("Matching ", .label, " failed: ", conditionMessage(e))
                  return(NULL)
                })
  if (is.null(m)) {
    return(tibble(method = .label,
                  n_treated = NA, n_control = NA,
                  n_matched_total = NA, max_abs_SMD = NA))
  }
  md <- match.data(m)
  smd_after <- summary(m, standardize = TRUE)$sum.matched
  smd_cov <- smd_after[rownames(smd_after) %in% ps_vars,
                       "Std. Mean Diff.", drop = TRUE]
  max_abs_smd <- max(abs(smd_cov), na.rm = TRUE)
  tibble(method = .label,
         n_treated = sum(md$treat_A1 == 1),
         n_control = sum(md$treat_A1 == 0),
         n_matched_total = nrow(md),
         max_abs_SMD = round(max_abs_smd, 3))
}

variants <- bind_rows(
  run_match("M1 - 1:1 NN with replacement",
            method = "nearest", ratio = 1, replace = TRUE,
            distance = "glm"),
  run_match("M2 - 1:1 NN without replacement",
            method = "nearest", ratio = 1, replace = FALSE,
            distance = "glm"),
  run_match("M3 - 1:3 NN with replacement",
            method = "nearest", ratio = 3, replace = TRUE,
            distance = "glm"),
  run_match("M4 - 1:5 NN with replacement",
            method = "nearest", ratio = 5, replace = TRUE,
            distance = "glm"),
  run_match("M5 - 1:1 NN, caliper 0.25 SD",
            method = "nearest", ratio = 1, replace = TRUE,
            distance = "glm", caliper = 0.25),
  run_match("M6 - exact macro + 1:1 NN within",
            method = "nearest", ratio = 1, replace = TRUE,
            distance = "glm", exact = "macro"),
  run_match("M7 - optimal full matching",
            method = "full", distance = "glm")
)

cat("\nStep 6 — Matching variants compared:\n")
print(variants, width = Inf)
write_csv(variants, "output/tables/psm_matching_variants.csv")

# ---- 8. Detailed run on M1 (1:1 with replacement) -------------------
cat("\nStep 7 — Detailed balance for the BASELINE 1:1 with replacement:\n")
m1 <- matchit(ps_formula, data = trim, method = "nearest", ratio = 1,
              replace = TRUE, distance = "glm")
summ <- summary(m1, standardize = TRUE)
bal <- as.data.frame(summ$sum.matched) %>%
  rownames_to_column("variable") %>%
  rename(SMD = `Std. Mean Diff.`,
         eCDFmean = `eCDF Mean`, eCDFmax = `eCDF Max`,
         pairDist = `Std. Pair Dist.`) %>%
  select(variable, `Means Treated`, `Means Control`,
         SMD, eCDFmean, eCDFmax, pairDist) %>%
  mutate(across(c(`Means Treated`, `Means Control`, SMD,
                  eCDFmean, eCDFmax, pairDist),
                ~ round(., 3)))
print(bal)
write_csv(bal, "output/tables/psm_balance_1to1.csv")

# Sample sizes per the M1 baseline
matched <- match.data(m1)
cat(sprintf("\n  Final matched sample (M1, 1:1 with replacement):\n"))
cat(sprintf("  - n treated         : %d\n", sum(matched$treat_A1 == 1)))
cat(sprintf("  - n control matches : %d unique control comuni used\n",
            length(unique(matched$PRO_COM[matched$treat_A1 == 0]))))
cat(sprintf("  - n weighted total  : %d rows in match.data\n", nrow(matched)))

# Aree interne distribution in the matched sample
cat("\n  Aree-interne distribution in the matched M1 sample:\n")
print(matched %>% count(treat_A1, Aree_Int) %>%
        pivot_wider(names_from = treat_A1, values_from = n, values_fill = 0,
                    names_prefix = "treat_"))

# Save matched keys for downstream scripts
saveRDS(matched %>% select(PRO_COM, weights, treat_A1),
        "data/matched_full_1to1.rds")

message("[11] Done.")
