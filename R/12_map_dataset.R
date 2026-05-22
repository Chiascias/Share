# =====================================================================
# 12_map_dataset.R  --  map of the comuni in the 1951 dataset
# =====================================================================
#
# Purpose: visualise the geographic scope of the analysis sample so
# the reader can see immediately:
#   * which regions the panel covers (8 out of 20 Italian regions)
#   * which provinces host A1 toll booths (16 provinces)
#   * where the 53 treated comuni are located along the A1 spine
#
# Because we do NOT yet have comune-level centroids from the ISTAT
# shapefile, we use province-capital coordinates as a coarse proxy
# (hardcoded in R/09_iv_romita.R). For the all-comuni dot we plot a
# small jitter around the province capital so the visual density
# reflects the number of comuni per province.

source("R/00_setup.R")
suppressPackageStartupMessages({
  library(sf); library(rnaturalearthdata)
})

panel <- readRDS("data/panel_long.rds")

# ---- 1. Italy outline ------------------------------------------------
data("countries50")
italy <- countries50[countries50$sovereignt == "Italy", ]
italy_sf <- st_as_sf(italy)

# Bounding box around 8-region sample (approx)
bbox <- c(xmin = 6.5, xmax = 16.5, ymin = 38.5, ymax = 47.5)

# ---- 2. Province-capital coordinates (hardcoded) ---------------------
province_caps <- tibble::tribble(
  ~COD_PROV, ~lon,    ~lat,    ~name,
   6,         8.6126,  44.9136, "Alessandria",
  12,         8.8252, 45.8205,  "Varese",
  13,         9.0832, 45.8081,  "Como",
  14,         9.8755, 46.1700,  "Sondrio",
  15,         9.1900, 45.4640,  "Milano",
  16,         9.6671, 45.6982,  "Bergamo",
  17,        10.2185, 45.5416,  "Brescia",
  18,         9.5045, 45.3107,  "Pavia",
  19,        10.0282, 45.1335,  "Cremona",
  20,        10.7914, 45.1564,  "Mantova",
  23,        12.3155, 45.4408,  "Venezia",
  28,        11.8767, 45.4064,  "Padova",
  29,        11.0049, 45.4385,  "Verona",
  33,         9.6921, 45.0526,  "Piacenza",
  34,        10.3279, 44.8015,  "Parma",
  35,        10.6315, 44.6989,  "Reggio Emilia",
  36,        10.9252, 44.6471,  "Modena",
  37,        11.3426, 44.4949,  "Bologna",
  38,        11.6168, 44.8381,  "Ferrara",
  39,        12.2017, 44.4173,  "Ravenna",
  40,        12.5683, 44.0678,  "Forli",
  45,         9.8264, 44.1024,  "Massa-Carrara",
  46,        10.5036, 43.8430,  "Lucca",
  47,        10.4030, 43.7228,  "Pistoia",
  48,        11.2558, 43.7696,  "Firenze",
  49,        10.4017, 43.7228,  "Livorno",
  50,        10.4017, 43.7228,  "Pisa",
  51,        11.8807, 43.4632,  "Arezzo",
  52,        11.3309, 43.3188,  "Siena",
  53,        11.1167, 42.7726,  "Grosseto",
  54,        12.3886, 43.1107,  "Perugia",
  55,        12.6448, 42.5636,  "Terni",
  56,        12.1042, 42.4174,  "Viterbo",
  57,        12.8593, 42.4040,  "Rieti",
  58,        12.4964, 41.9028,  "Roma",
  59,        12.9024, 41.4671,  "Latina",
  60,        13.3500, 41.6396,  "Frosinone",
  61,        14.3320, 41.0723,  "Caserta",
  62,        14.7821, 41.1297,  "Benevento",
  63,        14.2681, 40.8518,  "Napoli",
  64,        14.7659, 40.9145,  "Avellino",
  65,        14.7659, 40.6824,  "Salerno")

# ---- 3. Tag each comune with its province capital coords + jitter ---
set.seed(42)
xs <- panel %>% filter(year == 1951) %>%
  distinct(PRO_COM, COD_PROV, COD_REG, treat_A1, cohort, COMUNE) %>%
  left_join(province_caps, by = "COD_PROV") %>%
  filter(!is.na(lon)) %>%
  mutate(lon_j = lon + rnorm(n(), 0, 0.10),
         lat_j = lat + rnorm(n(), 0, 0.08))

cat("Comuni mapped to province coords: ", nrow(xs), "\n")
cat("By treatment status:\n")
print(xs %>% count(treat_A1))
cat("By region:\n")
print(xs %>% count(COD_REG, name = "n_comuni"))

# ---- 4. Region labels for the legend ---------------------------------
region_labels <- c("1" = "Piemonte (1)",
                   "3" = "Lombardia (3)",
                   "5" = "Veneto (5)",
                   "8" = "Emilia-Romagna (8)",
                   "9" = "Toscana (9)",
                   "10" = "Umbria (10)",
                   "12" = "Lazio (12)",
                   "15" = "Campania (15)")
xs <- xs %>% mutate(region = region_labels[as.character(COD_REG)])

# ---- 5. Map A: all comuni in the 1951 dataset, by region ------------
p_A <- ggplot() +
  geom_sf(data = italy_sf, fill = "grey95", colour = "grey60", linewidth = 0.3) +
  geom_point(data = xs %>% filter(treat_A1 == 0),
             aes(x = lon_j, y = lat_j, colour = region),
             alpha = 0.35, size = 0.6) +
  geom_point(data = xs %>% filter(treat_A1 == 1),
             aes(x = lon_j, y = lat_j),
             colour = "black", fill = "#c0392b", shape = 21,
             size = 2, stroke = 0.4) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  scale_colour_brewer(palette = "Set2", name = "Regione") +
  labs(title = "Comuni nel campione (n = 3.242, 8 regioni)",
       subtitle = "Punti grigio-colorati: 3.189 comuni non trattati. Punti rossi: 53 comuni con casello A1.",
       x = NULL, y = NULL,
       caption = "Coordinate approssimate (capoluogo di provincia con jitter casuale)") +
  theme_paper() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "right")

ggsave("output/figures/fig_map_01_dataset.png", p_A,
       width = 8, height = 9, dpi = 200)
ggsave("output/figures/fig_map_01_dataset.pdf", p_A, width = 8, height = 9)

# ---- 6. Map B: treated provinces highlighted ------------------------
treated_provs <- xs %>% filter(treat_A1 == 1) %>% distinct(COD_PROV) %>% pull(COD_PROV)
prov_in_sample <- xs %>% distinct(COD_PROV) %>% pull(COD_PROV)
caps_in_sample <- province_caps %>% filter(COD_PROV %in% prov_in_sample) %>%
  mutate(treat_prov = COD_PROV %in% treated_provs)

p_B <- ggplot() +
  geom_sf(data = italy_sf, fill = "grey95", colour = "grey60", linewidth = 0.3) +
  geom_point(data = caps_in_sample,
             aes(x = lon, y = lat, colour = treat_prov, size = treat_prov)) +
  geom_text(data = caps_in_sample %>% filter(treat_prov),
            aes(x = lon, y = lat, label = name),
            hjust = -0.15, vjust = 0.5, size = 2.8, colour = "#c0392b") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  scale_colour_manual(values = c("TRUE" = "#c0392b", "FALSE" = "grey60"),
                      labels = c("FALSE" = "Provincia nel campione, no A1",
                                 "TRUE"  = "Provincia con casello A1 (n=16)"),
                      name = NULL) +
  scale_size_manual(values = c("TRUE" = 3.2, "FALSE" = 1.5), guide = "none") +
  labs(title = "Province nel campione (n = 41) e province con A1 (n = 16)",
       subtitle = "Le 16 province con almeno un casello A1 sono il dominio del confronto within-province",
       x = NULL, y = NULL) +
  theme_paper() +
  theme(panel.background = element_rect(fill = "white"))
ggsave("output/figures/fig_map_02_treated_provinces.png", p_B,
       width = 8, height = 9, dpi = 200)
ggsave("output/figures/fig_map_02_treated_provinces.pdf", p_B, width = 8, height = 9)

# ---- 7. Map C: treated comuni only with cohort labels ---------------
treated_comuni <- xs %>% filter(treat_A1 == 1)
p_C <- ggplot() +
  geom_sf(data = italy_sf, fill = "grey95", colour = "grey60", linewidth = 0.3) +
  geom_point(data = caps_in_sample %>% filter(treat_prov),
             aes(x = lon, y = lat),
             colour = "grey80", size = 5, alpha = 0.5) +
  geom_point(data = treated_comuni,
             aes(x = lon_j, y = lat_j, colour = cohort, shape = cohort),
             size = 3, stroke = 0.5) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
  scale_colour_manual(values = c("A" = "#16a085", "B" = "#c0392b"),
                      labels = c("A" = "Coorte A (K7 1959-60, n=19)",
                                 "B" = "Coorte B (K7 1962-64, n=34)"),
                      name = "Coorte di apertura") +
  scale_shape_manual(values = c("A" = 16, "B" = 17),
                     labels = c("A" = "Coorte A (K7 1959-60, n=19)",
                                "B" = "Coorte B (K7 1962-64, n=34)"),
                     name = "Coorte di apertura") +
  labs(title = "I 53 comuni con casello A1, per coorte di apertura",
       subtitle = "Coorte A: caselli aperti 1959-60 (Milano-Bologna axis). Coorte B: aperti 1962-64 (Bologna-Firenze passes + Roma-Napoli).",
       x = NULL, y = NULL,
       caption = "Cerchi grigi sullo sfondo: capoluoghi delle 16 province trattate.") +
  theme_paper() +
  theme(panel.background = element_rect(fill = "white"))
ggsave("output/figures/fig_map_03_treated_comuni.png", p_C,
       width = 8, height = 9, dpi = 200)
ggsave("output/figures/fig_map_03_treated_comuni.pdf", p_C, width = 8, height = 9)

# ---- 8. Summary table of sample composition --------------------------
summary_tab <- xs %>% group_by(region) %>%
  summarise(n_comuni = n(),
            n_treated = sum(treat_A1 == 1),
            n_control = sum(treat_A1 == 0),
            n_provinces = n_distinct(COD_PROV),
            .groups = "drop") %>%
  arrange(desc(n_comuni))
print(summary_tab)
write_csv(summary_tab, "output/tables/dataset_sample_by_region.csv")

message("[12] Done.  3 maps + summary table saved.")
