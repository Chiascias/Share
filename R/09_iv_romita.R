# =====================================================================
# 09_iv_romita.R  --  Romita 1955 plan IV strategy (skeleton)
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# The "Piano Romita" (Gazzetta Ufficiale 8/6/1955 n.131, p.2036) was
# the legislative blueprint for the Italian motorway network. It listed
# the corridors to be built but did NOT specify detailed routing -- the
# detailed alignment was settled between 1956 and 1964 under political
# pressure (Fanfani curve near Arezzo, Bologna-Firenze concession
# arbitration, etc.).
#
# We use **distance from the 1955 planned corridor** as an instrument
# for A1 access (K0, K7, W2). The exclusion restriction:
#
#   The political shocks 1956-1964 (route deviations) are
#   IDIOSYNCRATIC vs comune characteristics measured in 1951 census.
#   Conditional on 1951 baseline X and province FE, the 1955-planned
#   distance affects 1961-1991 outcomes ONLY through eventual A1
#   access.
#
# This is a NEW IV not previously used in the literature. It is
# distinct from Percoco 2016 (who instruments motorway placement with
# Roman roads — too remote temporally, weaker first stage) and from
# Banerjee-Duflo-Qian 2020 / Faber 2014 LCP IVs.
#
# Implementation status -----------------------------------------------
#
# This script REQUIRES `data/comuni_centroids.csv` with columns
# (PRO_COM, lon, lat) in WGS84. The centroids must be merged from the
# ISTAT shapefile (Confini Amministrativi, available from
# www.istat.it/it/archivio/222527).
#
# If the file is missing, we fall back to a coarse province-capital
# approximation (only ~30 unique distance values, NOT publication
# quality) and print a warning. Use the fallback only for sanity-
# checking the pipeline.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- 1. Romita 1955 anchor corridor ---------------------------------
# Anchor cities of the 1955 Sole corridor (Milano - Napoli) in WGS84.
# Coordinates of city centres, retrievable from any gazetteer.
romita_anchors <- tibble::tribble(
  ~name,         ~lon,     ~lat,
  "Milano",       9.1900,  45.4640,
  "Piacenza",     9.6921,  45.0526,
  "Parma",       10.3279,  44.8015,
  "Modena",      10.9252,  44.6471,
  "Bologna",     11.3426,  44.4949,
  "Firenze",     11.2558,  43.7696,
  "Roma",        12.4964,  41.9028,
  "Caserta",     14.3320,  41.0723,
  "Napoli",      14.2681,  40.8518)

# ---- 2. Load comune centroids ---------------------------------------
centroid_path <- "data/comuni_centroids.csv"
have_centroids <- file.exists(centroid_path)

if (have_centroids) {
  centroids <- read_csv(centroid_path, show_col_types = FALSE)
  stopifnot(all(c("PRO_COM", "lon", "lat") %in% names(centroids)))
  message("[09] Using comune centroids from ", centroid_path)
} else {
  # FALLBACK: use province-capital coordinates (~30 unique values).
  # This is for pipeline-testing only, NOT publication.
  message("[09] WARNING: data/comuni_centroids.csv not found.\n",
          "    Falling back to province-capital coordinates (~30 ",
          "unique values).\n",
          "    Replace with ISTAT comune centroids before publishing.")
  province_caps <- tibble::tribble(
    ~COD_PROV, ~lon,    ~lat,
     6,         8.6126,  44.9136,    # Alessandria (PI)
    15,         9.1900, 45.4640,    # Milano
    12,         9.0260, 45.7378,    # Varese
    13,         9.0832, 45.8081,    # Como
    14,         9.8755, 46.1700,    # Sondrio
    16,         9.6671, 45.6982,    # Bergamo
    17,        10.2185, 45.5416,    # Brescia
    18,         9.5045, 45.3107,    # Pavia (Lodi was created in 1992)
    19,        10.0282, 45.7619,    # Cremona
    20,        10.7914, 45.1564,    # Mantova
    23,        12.3155, 45.4408,    # Venezia
    28,        11.8767, 45.4064,    # Padova
    29,        11.0049, 45.4385,    # Verona
    33,         9.6921, 45.0526,    # Piacenza
    34,        10.3279, 44.8015,    # Parma
    35,        10.6315, 44.6989,    # Reggio Emilia
    36,        10.9252, 44.6471,    # Modena
    37,        11.3426, 44.4949,    # Bologna
    38,        11.6168, 44.8381,    # Ferrara
    39,        12.2017, 44.4173,    # Ravenna
    40,        12.5683, 44.0678,    # Forli
    45,         9.8264, 44.1024,    # Massa-Carrara
    46,        10.5036, 43.8430,    # Lucca
    47,        10.4030, 43.7228,    # Pistoia
    48,        11.2558, 43.7696,    # Firenze
    49,        10.4017, 43.7228,    # Livorno
    50,        10.4017, 43.7228,    # Pisa
    51,        11.8807, 43.4632,    # Arezzo
    52,        11.3309, 43.3188,    # Siena
    53,        11.1167, 42.7726,    # Grosseto
    54,        12.3886, 43.1107,    # Perugia
    55,        12.6448, 42.5636,    # Terni
    56,        12.1042, 42.4174,    # Viterbo
    57,        12.8593, 42.4040,    # Rieti
    58,        12.4964, 41.9028,    # Roma
    59,        12.9024, 41.4671,    # Latina
    60,        13.3500, 41.6396,    # Frosinone
    61,        14.3320, 41.0723,    # Caserta
    62,        14.7821, 41.1297,    # Benevento
    63,        14.2681, 40.8518,    # Napoli
    64,        15.7917, 40.6824,    # Avellino
    65,        14.7659, 40.6824)    # Salerno
  centroids <- panel %>% distinct(PRO_COM, COD_PROV) %>%
    left_join(province_caps, by = "COD_PROV")
}

# ---- 3. Distance from each comune to the 1955 corridor ---------------
# Convert anchor list to corridor segments (Milano -> Piacenza -> ...
# -> Napoli). For each comune, compute the minimum perpendicular
# distance to any segment of the polyline.

# Great-circle (haversine) distance helper, km
haversine_km <- function(lon1, lat1, lon2, lat2) {
  R <- 6371
  dlon <- (lon2 - lon1) * pi/180
  dlat <- (lat2 - lat1) * pi/180
  a <- sin(dlat/2)^2 +
       cos(lat1*pi/180) * cos(lat2*pi/180) * sin(dlon/2)^2
  R * 2 * asin(pmin(1, sqrt(a)))
}

# Distance from point P to line segment AB, in km (planar approx
# valid for distances < ~500 km in Italy; for full Italy use full
# spherical projection)
dist_point_to_segment <- function(px, py, ax, ay, bx, by) {
  # All in lon/lat. Approximate as planar at the segment midpoint
  # latitude.
  lat0 <- (ay + by) / 2
  kx_per_deg <- 111.32 * cos(lat0 * pi/180)  # km per deg lon
  ky_per_deg <- 110.57                       # km per deg lat
  # project to local plane (km)
  Px <- (px - ax) * kx_per_deg; Py <- (py - ay) * ky_per_deg
  Bx <- (bx - ax) * kx_per_deg; By <- (by - ay) * ky_per_deg
  # parametrise segment as A + t * AB, t in [0, 1]
  ab2 <- Bx^2 + By^2
  if (ab2 < 1e-9) {
    return(sqrt(Px^2 + Py^2))
  }
  t <- pmax(0, pmin(1, (Px * Bx + Py * By) / ab2))
  Cx <- t * Bx; Cy <- t * By
  sqrt((Px - Cx)^2 + (Py - Cy)^2)
}

# Romita corridor segments
n_anchors <- nrow(romita_anchors)
segments <- tibble(
  ax = romita_anchors$lon[1:(n_anchors-1)],
  ay = romita_anchors$lat[1:(n_anchors-1)],
  bx = romita_anchors$lon[2:n_anchors],
  by = romita_anchors$lat[2:n_anchors])

# For each comune compute min distance to any segment
centroids <- centroids %>% drop_na(lon, lat)
centroids$dist_romita_km <- sapply(seq_len(nrow(centroids)), function(i) {
  pt <- centroids[i, ]
  min(sapply(seq_len(nrow(segments)), function(j) {
    s <- segments[j, ]
    dist_point_to_segment(pt$lon, pt$lat, s$ax, s$ay, s$bx, s$by)
  }))
})

message(glue("[09] Distance from Romita 1955 corridor:\n",
             "    n comuni    : {nrow(centroids)}\n",
             "    median (km) : {sprintf('%.1f', median(centroids$dist_romita_km))}\n",
             "    p10  (km)   : {sprintf('%.1f', quantile(centroids$dist_romita_km, .10))}\n",
             "    p90  (km)   : {sprintf('%.1f', quantile(centroids$dist_romita_km, .90))}"))

write_csv(centroids %>% select(PRO_COM, dist_romita_km),
          "data/dist_romita.csv")

# ---- 4. First-stage check: does dist_romita predict K0 / W2 / K7? ----
xs <- panel %>% filter(year == 1991) %>%
  distinct(PRO_COM, COD_PROV, treat_A1, cohort, W2, K7_open) %>%
  left_join(centroids %>% select(PRO_COM, dist_romita_km), by = "PRO_COM") %>%
  drop_na(dist_romita_km)

cat("\n--- First-stage: K0 = a + b * dist_romita + prov FE ---\n")
fs1 <- with_cluster(
  lm(treat_A1 ~ dist_romita_km + factor(COD_PROV), data = xs),
  xs$COD_PROV, xs)
print(tidy_cr(fs1) %>% filter(term == "dist_romita_km"))

cat("\n--- First-stage: W2 = a + b * dist_romita + prov FE ---\n")
fs2 <- with_cluster(
  lm(W2 ~ dist_romita_km + factor(COD_PROV), data = xs),
  xs$COD_PROV, xs)
print(tidy_cr(fs2) %>% filter(term == "dist_romita_km"))

# F-stat (cluster-robust) at first stage
F_fs1 <- (coef(fs1)["dist_romita_km"] /
          sqrt(clubSandwich::vcovCR(fs1,
                cluster = attr(fs1, "cluster_vec"),
                type = "CR1")["dist_romita_km", "dist_romita_km"]))^2
F_fs2 <- (coef(fs2)["dist_romita_km"] /
          sqrt(clubSandwich::vcovCR(fs2,
                cluster = attr(fs2, "cluster_vec"),
                type = "CR1")["dist_romita_km", "dist_romita_km"]))^2
cat(glue("\nFirst-stage cluster-robust F-stats:\n",
         "    K0  on dist_romita : F = {sprintf('%.1f', F_fs1)}\n",
         "    W2  on dist_romita : F = {sprintf('%.1f', F_fs2)}\n",
         "Rule of thumb: F > 10 => not weak; F > 50 => strong.\n"))

# ---- 5. 2SLS for the long DiD ---------------------------------------
# Reduced form: Δlog y_{61-91} on dist_romita_km and controls
# IV-2SLS:   Δlog y on (treat_A1 = fitted from dist_romita) + ctrls

wide <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  select(PRO_COM, COD_PROV, COD_REG, macro, treat_A1, W2, year,
         lP1, lUT, lAT, I4, SS4, L15, L16, L17) %>%
  pivot_wider(names_from = year,
              values_from = c(lP1, lUT, lAT, I4, SS4, L15, L16, L17),
              names_sep = "_") %>%
  mutate(d_lP = lP1_1991 - lP1_1961,
         d_lU = lUT_1991 - lUT_1961,
         d_lA = lAT_1991 - lAT_1961) %>%
  inner_join(centroids %>% select(PRO_COM, dist_romita_km), by = "PRO_COM") %>%
  filter(!is.na(d_lP), !is.na(dist_romita_km))

ctrls <- c("lP1_1961", "I4_1961", "SS4_1961",
           "L15_1961", "L16_1961", "L17_1961")

# Reduced form on dist_romita
cat("\n--- Reduced form (Δlog y on dist_romita_km, no treat_A1) ---\n")
rf_models <- list(
  `d log Pop`   = with_cluster(lm(reformulate(c("dist_romita_km",
                                                 "factor(COD_PROV)", ctrls),
                                               "d_lP"), data = wide),
                                wide$COD_PROV, wide),
  `d log Units` = with_cluster(lm(reformulate(c("dist_romita_km",
                                                 "factor(COD_PROV)", ctrls),
                                               "d_lU"), data = wide),
                                wide$COD_PROV, wide),
  `d log Emp`   = with_cluster(lm(reformulate(c("dist_romita_km",
                                                 "factor(COD_PROV)", ctrls),
                                               "d_lA"), data = wide),
                                wide$COD_PROV, wide))
regtab(rf_models, keep = "^dist_romita_km$",
       out_csv = "output/tables/iv_romita_reduced_form.csv",
       out_tex = "output/tables/iv_romita_reduced_form.tex",
       title   = "Reduced form: Δlog outcome on dist from Romita 1955 corridor")

# Manual 2SLS: predict treat_A1 from dist_romita_km, then 2nd stage
fs_K0 <- lm(treat_A1 ~ dist_romita_km + factor(COD_PROV), data = wide)
wide$treat_hat <- predict(fs_K0)

cat("\n--- 2SLS structural: Δlog y on instrumented treat_A1 ---\n")
iv_models <- list(
  `d log Pop`   = with_cluster(lm(reformulate(c("treat_hat",
                                                 "factor(COD_PROV)", ctrls),
                                               "d_lP"), data = wide),
                                wide$COD_PROV, wide),
  `d log Units` = with_cluster(lm(reformulate(c("treat_hat",
                                                 "factor(COD_PROV)", ctrls),
                                               "d_lU"), data = wide),
                                wide$COD_PROV, wide),
  `d log Emp`   = with_cluster(lm(reformulate(c("treat_hat",
                                                 "factor(COD_PROV)", ctrls),
                                               "d_lA"), data = wide),
                                wide$COD_PROV, wide))
regtab(iv_models, keep = "^treat_hat$",
       out_csv = "output/tables/iv_romita_2sls.csv",
       out_tex = "output/tables/iv_romita_2sls.tex",
       title   = "IV-2SLS: structural effect of treat_A1 instrumented by dist Romita")

# (note: SE here are NAIVE 2SLS SE -- they ignore the first-stage
# noise. For publication, switch to AER::ivreg or estimatr::iv_robust
# which compute correct asymptotic SE. The naive SE here are a
# placeholder for sanity-checking the magnitude.)

message("\n[09] Done.")
if (!have_centroids) {
  message("REMINDER: results above use province-capital centroids ",
          "(coarse). For publication, build data/comuni_centroids.csv ",
          "from the ISTAT comuni shapefile and re-run.")
}
