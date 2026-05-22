# =====================================================================
# 12_map_dataset.R  --  CLEAN province-polygon map of the sample
# =====================================================================
#
# Replaces the earlier jittered-point version. Uses the `maps` package
# Italian province shapes to draw a proper choropleth-style map showing:
#
#   (i)  which provinces are in the analysis sample (any of 41)
#   (ii) which provinces host an A1 toll booth (16 of 41)
#   (iii) treated comuni overlaid as points (using province capital
#         coordinates, no random jitter)

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(sf)
  library(maps)
  library(rnaturalearthdata)
})

panel <- readRDS("data/panel_long.rds")

# ---- 1. Italian provinces from `maps` --------------------------------
italy_map <- maps::map("italy", plot = FALSE, fill = TRUE)
italy_sf <- st_as_sf(italy_map)
# Collapse "Venezia:Lido" etc. onto the primary province name
italy_sf$prov <- sub(":.*$", "", italy_sf$ID)
italy_sf <- italy_sf %>%
  group_by(prov) %>%
  summarise(geometry = sf::st_union(geom), .groups = "drop")
italy_sf <- st_make_valid(italy_sf)

# ---- 2. Province coordinates -> COD_PROV crosswalk -------------------
# Cross-reference province names (italian) with ISTAT province codes
# used in our panel. Only need the 41 provinces in the sample.
prov_xwalk <- tribble(
  ~COD_PROV, ~prov_name_maps,
   3, "Cuneo",          6, "Alessandria",
  12, "Varese",        13, "Como",       14, "Sondrio",
  15, "Milano",        16, "Bergamo",    17, "Brescia",
  18, "Pavia",         19, "Cremona",    20, "Mantova",
  23, "Venezia",       28, "Padova",     29, "Verona",
  33, "Piacenza",      34, "Parma",
  35, "Reggio Emilia",   36, "Modena",     37, "Bologna",
  38, "Ferrara",       39, "Ravenna",    40, "Forli'",
  45, "Massa-Carrara", 46, "Lucca",      47, "Pistoia",
  48, "Firenze",       49, "Livorno",    50, "Pisa",
  51, "Arezzo",        52, "Siena",      53, "Grosseto",
  54, "Perugia",       55, "Terni",
  56, "Viterbo",       57, "Rieti",      58, "Roma",
  59, "Latina",        60, "Frosinone",
  61, "Caserta",       62, "Benevento",  63, "Napoli",
  64, "Avellino",      65, "Salerno")

# ---- 3. Province categorisation --------------------------------------
provs_in_sample <- panel %>% filter(year == 1951) %>%
  distinct(COD_PROV) %>% pull(COD_PROV)
treated_provs <- panel %>% filter(treat_A1 == 1) %>%
  distinct(COD_PROV) %>% pull(COD_PROV)

# Counts per province
prov_counts <- panel %>% filter(year == 1951) %>%
  group_by(COD_PROV) %>%
  summarise(n_comuni = n(),
            n_treated = sum(treat_A1 == 1),
            .groups = "drop")

italy_status <- italy_sf %>%
  left_join(prov_xwalk %>% rename(prov = prov_name_maps), by = c("prov" = "prov")) %>%
  left_join(prov_counts, by = "COD_PROV") %>%
  mutate(status = case_when(
    is.na(COD_PROV)                       ~ "Non nel campione",
    COD_PROV %in% treated_provs            ~ "Provincia con casello A1",
    COD_PROV %in% provs_in_sample          ~ "Provincia nel campione, no A1",
    TRUE                                   ~ "Non nel campione"),
    status = factor(status, levels = c("Non nel campione",
                                       "Provincia nel campione, no A1",
                                       "Provincia con casello A1")))

# Check unmatched provinces (could be name spelling mismatches)
unmatched <- prov_xwalk %>%
  anti_join(italy_sf %>% st_drop_geometry(), by = c("prov_name_maps" = "prov"))
if (nrow(unmatched) > 0) {
  message("WARNING: provinces not found in maps data:")
  print(unmatched)
}

# Province capital coordinates (for labels and treated-comune points)
prov_caps <- tribble(
  ~COD_PROV, ~cap_name,             ~lon,     ~lat,
   3, "Cuneo",                       7.5429,  44.3841,
   6, "Alessandria",                 8.6126,  44.9136,
  12, "Varese",                      8.8252,  45.8205,
  13, "Como",                        9.0832,  45.8081,
  14, "Sondrio",                     9.8755,  46.1700,
  15, "Milano",                      9.1900,  45.4640,
  16, "Bergamo",                     9.6671,  45.6982,
  17, "Brescia",                    10.2185,  45.5416,
  18, "Pavia",                       9.1561,  45.1847,
  19, "Cremona",                    10.0282,  45.1335,
  20, "Mantova",                    10.7914,  45.1564,
  23, "Venezia",                    12.3155,  45.4408,
  28, "Padova",                     11.8767,  45.4064,
  29, "Verona",                     11.0049,  45.4385,
  33, "Piacenza",                    9.6921,  45.0526,
  34, "Parma",                      10.3279,  44.8015,
  35, "Reggio Emilia",              10.6315,  44.6989,
  36, "Modena",                     10.9252,  44.6471,
  37, "Bologna",                    11.3426,  44.4949,
  38, "Ferrara",                    11.6168,  44.8381,
  39, "Ravenna",                    12.2017,  44.4173,
  40, "Forlì",                      12.0407,  44.2226,
  45, "Massa-Carrara",               9.8264,  44.1024,
  46, "Lucca",                      10.5036,  43.8430,
  47, "Pistoia",                    10.9176,  43.9335,
  48, "Firenze",                    11.2558,  43.7696,
  49, "Livorno",                    10.3094,  43.5485,
  50, "Pisa",                       10.4017,  43.7228,
  51, "Arezzo",                     11.8807,  43.4632,
  52, "Siena",                      11.3309,  43.3188,
  53, "Grosseto",                   11.1167,  42.7726,
  54, "Perugia",                    12.3886,  43.1107,
  55, "Terni",                      12.6448,  42.5636,
  56, "Viterbo",                    12.1042,  42.4174,
  57, "Rieti",                      12.8593,  42.4040,
  58, "Roma",                       12.4964,  41.9028,
  59, "Latina",                     12.9024,  41.4671,
  60, "Frosinone",                  13.3500,  41.6396,
  61, "Caserta",                    14.3320,  41.0723,
  62, "Benevento",                  14.7821,  41.1297,
  63, "Napoli",                     14.2681,  40.8518,
  64, "Avellino",                   14.7889,  40.9145,
  65, "Salerno",                    14.7659,  40.6824)

# Treated comune coordinates: use the province capital of the comune
# (without jitter, just placed on the capital — coarse but clean)
treated_comuni_pts <- panel %>% filter(treat_A1 == 1, year == 1991) %>%
  distinct(PRO_COM, COMUNE, COD_PROV, cohort) %>%
  left_join(prov_caps, by = "COD_PROV") %>%
  # Add a small deterministic offset by sequence within province so
  # multiple treated in the same province don't fully overlap
  group_by(COD_PROV) %>%
  mutate(rank = row_number(),
         angle = (rank - 1) * pi/3,
         lon_p = lon + cos(angle) * 0.08,
         lat_p = lat + sin(angle) * 0.06) %>%
  ungroup()

# Bounding box of the sample
bbox <- c(xmin = 6.5, xmax = 16.5, ymin = 38.5, ymax = 47.5)

# ---- 4. MAP A — province choropleth ----------------------------------
p_A <- ggplot() +
  geom_sf(data = italy_status,
          aes(fill = status),
          colour = "grey50", linewidth = 0.18) +
  scale_fill_manual(
    values = c("Non nel campione"                = "grey92",
               "Provincia nel campione, no A1"   = "#a6cee3",
               "Provincia con casello A1"        = "#e31a1c"),
    name = NULL,
    drop = FALSE) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  labs(title = "Province italiane nel campione di analisi",
       subtitle = "41 province (di cui 16 con casello A1) su 6 regioni attraversate dall'A1 + 2 buffer (Piemonte, Veneto)",
       caption = "Fonte: ISTAT Ottomila + database caselli A1 (Lelo & Tani 2026).",
       x = NULL, y = NULL) +
  theme_paper() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10))

ggsave("output/figures/fig_map_01_provinces.png", p_A,
       width = 9, height = 10, dpi = 220)
ggsave("output/figures/fig_map_01_provinces.pdf", p_A,
       width = 9, height = 10)

# ---- 5. MAP B — choropleth + treated province names ------------------
treated_caps <- prov_caps %>% filter(COD_PROV %in% treated_provs)
p_B <- ggplot() +
  geom_sf(data = italy_status, aes(fill = status),
          colour = "grey50", linewidth = 0.18) +
  scale_fill_manual(
    values = c("Non nel campione"                = "grey92",
               "Provincia nel campione, no A1"   = "#cfe2f3",
               "Provincia con casello A1"        = "#fdcdb9"),
    name = NULL, drop = FALSE) +
  geom_point(data = treated_caps,
             aes(x = lon, y = lat),
             colour = "#a50f15", size = 2.4) +
  ggrepel::geom_text_repel(
    data = treated_caps,
    aes(x = lon, y = lat, label = cap_name),
    size = 3.1, colour = "#a50f15", fontface = "bold",
    box.padding = 0.3, point.padding = 0.2,
    segment.colour = "grey60", segment.size = 0.3,
    max.overlaps = Inf, seed = 42) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  labs(title = "Le 16 province con casello A1",
       subtitle = "Capoluoghi etichettati. Le altre 25 province del campione (azzurro) servono come controllo within-corridor.",
       x = NULL, y = NULL) +
  theme_paper() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10))

# Fall back if ggrepel not available
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  p_B <- ggplot() +
    geom_sf(data = italy_status, aes(fill = status),
            colour = "grey50", linewidth = 0.18) +
    scale_fill_manual(
      values = c("Non nel campione"                = "grey92",
                 "Provincia nel campione, no A1"   = "#cfe2f3",
                 "Provincia con casello A1"        = "#fdcdb9"),
      name = NULL, drop = FALSE) +
    geom_point(data = treated_caps, aes(x = lon, y = lat),
               colour = "#a50f15", size = 2.4) +
    geom_text(data = treated_caps,
              aes(x = lon, y = lat, label = cap_name),
              size = 2.8, colour = "#a50f15", fontface = "bold",
              hjust = -0.15, vjust = 0.5) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = FALSE) +
    labs(title = "Le 16 province con casello A1",
         subtitle = "Capoluoghi etichettati. Le altre 25 province del campione (azzurro) servono come controllo within-corridor.",
         x = NULL, y = NULL) +
    theme_paper() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          legend.position = "bottom")
}
ggsave("output/figures/fig_map_02_treated_provinces.png", p_B,
       width = 9, height = 10, dpi = 220)
ggsave("output/figures/fig_map_02_treated_provinces.pdf", p_B,
       width = 9, height = 10)

# ---- 6. MAP C — treated comuni overlay, per cohort ------------------
p_C <- ggplot() +
  geom_sf(data = italy_status, aes(fill = status),
          colour = "grey60", linewidth = 0.18) +
  scale_fill_manual(
    values = c("Non nel campione"                = "grey95",
               "Provincia nel campione, no A1"   = "white",
               "Provincia con casello A1"        = "#fff5e6"),
    name = NULL, drop = FALSE, guide = "none") +
  geom_point(data = treated_comuni_pts,
             aes(x = lon_p, y = lat_p, colour = cohort, shape = cohort),
             size = 2.5, stroke = 0.7) +
  scale_colour_manual(
    values = c("A" = "#1b9e77", "B" = "#d95f02"),
    labels = c("A" = "Coorte A (K7 1959-60, n=19)",
               "B" = "Coorte B (K7 1962-64, n=34)"),
    name = "Coorte di apertura") +
  scale_shape_manual(
    values = c("A" = 16, "B" = 17),
    labels = c("A" = "Coorte A (K7 1959-60, n=19)",
               "B" = "Coorte B (K7 1962-64, n=34)"),
    name = "Coorte di apertura") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  labs(title = "I 53 comuni con casello A1, per coorte di apertura",
       subtitle = "Coorte A (verde): Milano-Bologna axis, 1959-60.  Coorte B (arancio): Bologna-Firenze + Roma-Napoli, 1962-64.",
       caption = "Punti posizionati approssimativamente sul capoluogo provinciale (per multipli trattati: leggero offset deterministico).",
       x = NULL, y = NULL) +
  theme_paper() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10))
ggsave("output/figures/fig_map_03_treated_comuni.png", p_C,
       width = 9, height = 10, dpi = 220)
ggsave("output/figures/fig_map_03_treated_comuni.pdf", p_C,
       width = 9, height = 10)

# Drop the old cluttered fig_map_01_dataset
file.remove("output/figures/fig_map_01_dataset.png")
file.remove("output/figures/fig_map_01_dataset.pdf")

message("[12] Done.  3 clean province-polygon maps saved.")
