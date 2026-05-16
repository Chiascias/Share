# =====================================================================
# 00_setup.R  --  global options, packages, helpers
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# All packages used here are available from the Debian/Ubuntu apt
# distribution (r-cran-*). We deliberately AVOID two CRAN-only packages:
#
#   * `fixest`       -> we use base `lm()` with explicit `factor()` FE
#                       (the design has ~30 provinces, so high-dim FE are
#                       not an issue; doing it this way also makes the
#                       econometric model fully transparent).
#   * `modelsummary` -> we write a small `regtab()` helper that takes a
#                       named list of fitted models and produces a CSV +
#                       LaTeX table by hand. Again: transparent, with no
#                       hidden defaults.
#
# Cluster-robust standard errors come from `clubSandwich::vcovCR(., type = "CR1")`,
# which is the small-sample-corrected ("Stata-like") cluster-robust
# variance estimator. It is the same estimator Ciani-de Blasio use in
# their highway papers.

suppressPackageStartupMessages({
  library(dplyr);   library(tidyr);   library(readr);   library(readxl)
  library(purrr);   library(stringr); library(tibble);  library(glue)
  library(ggplot2); library(scales);  library(broom)
  library(sandwich); library(lmtest); library(clubSandwich)
  library(kableExtra)
})

options(stringsAsFactors = FALSE)
options(scipen = 999)

# Common ggplot theme so all figures share the same style
theme_paper <- function() {
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot")
}

# ISTAT region codes for the panel
region_labels <- c("1" = "Piemonte",
                   "3" = "Lombardia",
                   "5" = "Veneto",
                   "8" = "Emilia-Romagna",
                   "9" = "Toscana",
                   "10" = "Umbria",
                   "12" = "Lazio",
                   "15" = "Campania")

# Macro-area: matches Lelo & Tani's North/Centre/South split.
# Note: Piemonte/Veneto are in the panel but are NOT crossed by the A1;
# we group them with the North.
macro_area <- function(cod_reg) {
  dplyr::case_when(
    cod_reg %in% c(1, 3, 5, 8) ~ "North",
    cod_reg %in% c(9, 10, 12)  ~ "Centre",
    cod_reg %in% c(15)         ~ "South",
    TRUE                       ~ NA_character_
  )
}

CENSUS_YEARS <- c(1951, 1961, 1971, 1981, 1991)

# Pre-treatment year. The A1 was inaugurated in October 1964, but
# sections opened progressively from 1958 (Milano-Piacenza Nord). 1961
# is the LAST census taken before the bulk of the network was opened
# (5 of the 15 sections in Table 1 were active by July 1961, accounting
# for less than 1/3 of total kilometres). We therefore treat 1961 as
# the "pre" census and 1971/1981/1991 as "post".
PRE_YEAR     <- 1961
A1_OPEN_YEAR <- 1964

# ---- regtab() : small helper to write a regression table -------------
# Inputs:
#   models     named list of lm objects. Each lm must have $cluster_vec
#              (a numeric/integer/character vector of clustering IDs of
#              length nobs(m)) attached. Use the `with_cluster()` wrapper.
#   keep       regex of coefficients to keep (default: keep "treat")
#   out_csv    path to CSV
#   out_tex    path to LaTeX (NULL to skip)
#   title      table title (for tex caption)
#
# Output:
#   prints to console and writes CSV + (optionally) LaTeX.
regtab <- function(models, cluster_by = NULL, keep = "treat",
                   out_csv = NULL, out_tex = NULL, title = "") {
  rows <- list()
  for (mname in names(models)) {
    m <- models[[mname]]
    cl_vec <- attr(m, "cluster_vec")
    if (is.null(cl_vec)) {
      stop("Model '", mname, "' has no $cluster_vec attribute. ",
           "Wrap the fit with with_cluster(lm(...), data$COD_PROV).")
    }
    V <- clubSandwich::vcovCR(m, cluster = cl_vec, type = "CR1")
    ct <- lmtest::coeftest(m, vcov. = V)
    co <- as.data.frame(unclass(ct)) %>%
      rownames_to_column("term") %>%
      filter(grepl(keep, term)) %>%
      mutate(model = mname,
             est  = sprintf("%.4f", Estimate),
             se   = sprintf("(%.4f)", `Std. Error`),
             p    = `Pr(>|t|)`,
             stars = case_when(p < 0.01 ~ "***",
                               p < 0.05 ~ "**",
                               p < 0.10 ~ "*",
                               TRUE     ~ ""),
             est_with_stars = paste0(est, stars))
    rows[[mname]] <- co %>% mutate(n = nobs(m), r2 = summary(m)$r.squared)
  }
  long <- bind_rows(rows)
  wide <- long %>%
    select(term, model, est_with_stars, se) %>%
    pivot_longer(c(est_with_stars, se), names_to = "stat") %>%
    mutate(row = ifelse(stat == "est_with_stars", term, paste0(term, "_SE"))) %>%
    select(row, model, value) %>%
    pivot_wider(names_from = model, values_from = value) %>%
    arrange(row)
  # add N and R2 rows
  diag <- bind_rows(
    long %>% distinct(model, .keep_all = TRUE) %>%
      transmute(row = "N",  model, value = as.character(n)),
    long %>% distinct(model, .keep_all = TRUE) %>%
      transmute(row = "R2", model, value = sprintf("%.3f", r2))
  ) %>% pivot_wider(names_from = model, values_from = value)
  tab <- bind_rows(wide, diag)
  print(tab, n = 100)
  if (!is.null(out_csv)) write_csv(tab, out_csv)
  if (!is.null(out_tex)) {
    tex <- kable(tab, format = "latex", booktabs = TRUE, caption = title,
                 escape = TRUE)
    writeLines(as.character(tex), out_tex)
  }
  invisible(tab)
}

# ---- with_cluster() : attach a clustering vector to a fitted model ----
# Use this whenever fitting a model whose SE will be clustered. The
# cluster vector must have one element per OBSERVATION USED IN THE FIT
# (na.action = na.omit drops rows; we account for that automatically).
with_cluster <- function(model, cluster_full, data) {
  # `cluster_full` is a vector of length nrow(data); pick the rows
  # actually used by the fit
  used <- as.integer(rownames(model.frame(model)))
  cl_vec <- cluster_full[used]
  attr(model, "cluster_vec") <- cl_vec
  model
}

# Helper: tidy frame with cluster-robust SE
tidy_cr <- function(m, cluster_vec = NULL) {
  if (is.null(cluster_vec)) cluster_vec <- attr(m, "cluster_vec")
  V <- clubSandwich::vcovCR(m, cluster = cluster_vec, type = "CR1")
  ct <- lmtest::coeftest(m, vcov. = V)
  as.data.frame(unclass(ct)) %>%
    rownames_to_column("term") %>%
    rename(estimate = Estimate, std.error = `Std. Error`,
           statistic = `t value`, p.value = `Pr(>|t|)`) %>%
    mutate(conf.low  = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
}
