# =====================================================================
# 02_descriptive.R  --  descriptive evidence (replicates Lelo Tab 2-5)
# =====================================================================
#
# Cluster analysis 1951-1991 on (I4, SS4, L15, L16, L17) and
# intercensal growth differentials by treatment status. Mirrors the
# tables in Section "Some descriptive evidence" of the draft.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# ---- (a) k-means cluster analysis -----------------------------------
cluster_year <- function(y) {
  d <- panel %>% filter(year == y) %>%
    select(PRO_COM, I4, SS4, L15, L16, L17) %>% drop_na()
  if (nrow(d) < 50) return(NULL)
  X <- scale(d %>% select(-PRO_COM))
  set.seed(42)
  km <- kmeans(X, centers = 4, nstart = 25, iter.max = 100)
  centres <- km$centers
  scores <- as_tibble(centres, rownames = "id") %>%
    mutate(id = as.integer(id),
           agri = (I4 + L15) / 2,
           indu = L16,
           tert = (SS4 + L17) / 2)
  lab <- character(4)
  ag <- which.max(scores$agri);  lab[ag]  <- "agricultural/traditional"
  rest <- setdiff(1:4, ag)
  in_ <- rest[which.max(scores$indu[rest])]; lab[in_] <- "industrial"
  rest <- setdiff(rest, in_)
  te <- rest[which.max(scores$tert[rest])]; lab[te] <- "tertiary/educated"
  lab[rest[rest != te]] <- "intermediate/low"
  tibble(PRO_COM = d$PRO_COM,
         profile = lab[km$cluster], year = y)
}
cl <- bind_rows(lapply(CENSUS_YEARS, cluster_year))
tab_cluster <- cl %>%
  count(year, profile) %>%
  group_by(year) %>%
  mutate(share = round(n / sum(n) * 100, 2)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = year, values_from = share, values_fill = 0)
write_csv(tab_cluster, "output/tables/tab01_cluster_shares.csv")
print(tab_cluster)

# Save 1961 cluster -- used as baseline covariate later
saveRDS(cl %>% filter(year == 1961) %>%
          select(PRO_COM, profile_1961 = profile),
        "data/cluster_1961.rds")

# ---- (b) Intercensal growth rates by treatment ----------------------
growth <- panel %>%
  filter(sample_tight, year %in% c(1961, 1971, 1981, 1991)) %>%
  arrange(PRO_COM, year) %>%
  group_by(PRO_COM) %>%
  mutate(P1_l = lag(P1), UT_l = lag(UT), AT_l = lag(AT),
         g_P1 = ifelse(P1_l > 0, (P1 / P1_l - 1) * 100, NA_real_),
         g_UT = ifelse(UT_l > 0, (UT / UT_l - 1) * 100, NA_real_),
         g_AT = ifelse(AT_l > 0, (AT / AT_l - 1) * 100, NA_real_)) %>%
  ungroup()
tab_growth <- growth %>% filter(year > 1961) %>%
  group_by(year, treat_A1) %>%
  summarise(g_P1 = mean(g_P1, na.rm = TRUE),
            g_UT = mean(g_UT, na.rm = TRUE),
            g_AT = mean(g_AT, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(group = ifelse(treat_A1 == 1, "A1 treated", "Same-prov control")) %>%
  select(year, group, g_P1, g_UT, g_AT)
write_csv(tab_growth, "output/tables/tab02_intercensal_growth.csv")
print(tab_growth)

# ---- (c) Cumulative 1961-1991 growth by treat × cohort --------------
cum <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  select(PRO_COM, treat_A1, cohort, year, P1, UT, AT) %>%
  pivot_wider(names_from = year, values_from = c(P1, UT, AT)) %>%
  mutate(d_P = ifelse(P1_1961 > 0, (P1_1991/P1_1961 - 1) * 100, NA_real_),
         d_U = ifelse(UT_1961 > 0, (UT_1991/UT_1961 - 1) * 100, NA_real_),
         d_A = ifelse(AT_1961 > 0, (AT_1991/AT_1961 - 1) * 100, NA_real_))
tab_cum <- cum %>% group_by(cohort) %>%
  summarise(n = n(),
            `Pop %`        = round(mean(d_P, na.rm=TRUE), 1),
            `Local units %` = round(mean(d_U, na.rm=TRUE), 1),
            `Employees %`  = round(mean(d_A, na.rm=TRUE), 1))
write_csv(tab_cum, "output/tables/tab03_cum_growth_by_cohort.csv")
print(tab_cum)

# ---- (d) Growth distribution figure ---------------------------------
p_dist <- cum %>%
  ggplot(aes(x = d_P, fill = cohort)) +
  geom_density(alpha = 0.45) +
  scale_fill_manual(values = c("Never" = "grey60",
                               "A"     = "#16a085",
                               "B"     = "#c0392b"), name = "Cohort") +
  coord_cartesian(xlim = c(-100, 250)) +
  labs(x = "1961-1991 population growth (%)", y = "Density",
       title = "1961-1991 population growth by cohort",
       subtitle = "Same-prov tight sample") +
  theme_paper()
ggsave("output/figures/fig01_growth_distribution.png", p_dist,
       width = 7, height = 4.5, dpi = 200)
ggsave("output/figures/fig01_growth_distribution.pdf", p_dist,
       width = 7, height = 4.5)

message("[02] Done.")
