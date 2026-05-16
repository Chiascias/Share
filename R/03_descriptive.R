# =====================================================================
# 03_descriptive.R  --  cluster analysis + intercensal growth
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Two descriptive blocks:
#
# (a) k-means cluster analysis on (I4, SS4, L15, L16, L17), separately
#     for each census year. Replicates Table 3 of the draft (4-cluster
#     profile shares 1951..1991). The 1961 cluster will also be used in
#     R/05_heterogeneous.R as a baseline development covariate.
#
# (b) Intercensal growth rates of P1, UT, AT for treated vs same-
#     province control, plus the cumulative 1961-1991 growth differential
#     that gives the headline "+27% / +51.9% / +56.4%" of the draft.

source("R/00_setup.R")
panel <- readRDS("data/panel_long.rds")

# -------------------------------------------------------------------------
# (a) Cluster analysis
# -------------------------------------------------------------------------
# 4 centres, k-means with z-standardised inputs (so no variable dominates
# by scale), seed fixed for reproducibility.
cluster_year <- function(y) {
  d <- panel %>% filter(year == y) %>%
    select(PRO_COM, I4, SS4, L15, L16, L17) %>%
    drop_na()
  if (nrow(d) < 50) return(NULL)
  X <- scale(d %>% select(-PRO_COM))
  set.seed(42)
  km <- kmeans(X, centers = 4, nstart = 25, iter.max = 100)
  centres <- km$centers   # 4 x 5 matrix of z-scores
  # Assign each cluster a label using a 4-rule decision tree on the
  # centre's z-scores so we always end up with 4 distinct profiles
  # matching Lelo & Tani Table 2 (agric / intermediate / industrial /
  # tertiary).
  scores <- as_tibble(centres, rownames = "cluster_id") %>%
    mutate(cluster_id = as.integer(cluster_id),
           agri_score = (I4 + L15) / 2,
           indu_score = L16,
           tert_score = (SS4 + L17) / 2)
  # Rank: highest agric -> "agricultural/traditional",
  #       highest indu  -> "industrial",
  #       highest tert  -> "tertiary/educated",
  #       remaining     -> "intermediate/low".
  lab <- character(4)
  ag  <- which.max(scores$agri_score)
  lab[ag]  <- "agricultural/traditional"
  remaining <- setdiff(seq_len(4), ag)
  ind <- remaining[which.max(scores$indu_score[remaining])]
  lab[ind] <- "industrial"
  remaining <- setdiff(remaining, ind)
  tert <- remaining[which.max(scores$tert_score[remaining])]
  lab[tert] <- "tertiary/educated"
  lab[remaining[remaining != tert]] <- "intermediate/low"
  tibble(PRO_COM = d$PRO_COM,
         cluster = km$cluster,
         profile = lab[km$cluster],
         year    = y)
}

cluster_all <- bind_rows(lapply(CENSUS_YEARS, cluster_year))

# Add an explicit "intermediate/low" for clusters that don't get a
# distinctive top feature (i.e. flat profiles)
flat_profile <- cluster_all %>%
  group_by(year, cluster) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(share = n / sum(n))

# Profile shares (Lelo & Tani Table 3)
tab3 <- cluster_all %>%
  count(year, profile) %>%
  group_by(year) %>%
  mutate(share = round(n / sum(n) * 100, 2)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = year, values_from = share, values_fill = 0)
write_csv(tab3, "output/tables/tab03_cluster_shares.csv")
print(tab3)
message("[03] Cluster shares saved -> output/tables/tab03_cluster_shares.csv")

# Save the 1961 cluster label so it can be used as a baseline covariate
baseline_cluster <- cluster_all %>% filter(year == 1961) %>%
  select(PRO_COM, profile_1961 = profile)
saveRDS(baseline_cluster, "data/cluster_1961.rds")

# -------------------------------------------------------------------------
# (b) Intercensal growth: treated vs. same-province control
# -------------------------------------------------------------------------
growth_panel <- panel %>%
  filter(sample_tight, year %in% c(1961, 1971, 1981, 1991)) %>%
  arrange(PRO_COM, year) %>%
  group_by(PRO_COM) %>%
  mutate(P1_l = lag(P1), UT_l = lag(UT), AT_l = lag(AT),
         g_P1 = ifelse(P1_l > 0, (P1 / P1_l - 1) * 100, NA_real_),
         g_UT = ifelse(UT_l > 0, (UT / UT_l - 1) * 100, NA_real_),
         g_AT = ifelse(AT_l > 0, (AT / AT_l - 1) * 100, NA_real_)) %>%
  ungroup()

avg_growth <- growth_panel %>%
  filter(year > 1961) %>%
  group_by(year, treat_A1) %>%
  summarise(g_P1 = mean(g_P1, na.rm = TRUE),
            g_UT = mean(g_UT, na.rm = TRUE),
            g_AT = mean(g_AT, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(group = ifelse(treat_A1 == 1, "A1 treated", "Same-prov control"))

tab_growth <- avg_growth %>%
  select(year, group, g_P1, g_UT, g_AT) %>%
  pivot_wider(names_from = group, values_from = c(g_P1, g_UT, g_AT)) %>%
  arrange(year)
write_csv(tab_growth, "output/tables/tab04_intercensal_growth.csv")
print(tab_growth)
message("[03] Intercensal growth saved -> output/tables/tab04_intercensal_growth.csv")

# Cumulative 1961 -> 1991 growth difference (the +27% / +51.9% / +56.4%
# numbers reported in the draft)
cum <- panel %>%
  filter(year %in% c(1961, 1991), sample_tight) %>%
  select(PRO_COM, treat_A1, COD_PROV, year, P1, UT, AT) %>%
  pivot_wider(names_from = year, values_from = c(P1, UT, AT)) %>%
  mutate(d_P = ifelse(P1_1961 > 0, (P1_1991 / P1_1961 - 1) * 100, NA_real_),
         d_U = ifelse(UT_1961 > 0, (UT_1991 / UT_1961 - 1) * 100, NA_real_),
         d_A = ifelse(AT_1961 > 0, (AT_1991 / AT_1961 - 1) * 100, NA_real_))

tab_cum <- cum %>%
  group_by(treat_A1) %>%
  summarise(`Δ Pop (%)`        = mean(d_P, na.rm = TRUE),
            `Δ Local units (%)` = mean(d_U, na.rm = TRUE),
            `Δ Employees (%)`  = mean(d_A, na.rm = TRUE),
            n = n()) %>%
  mutate(group = ifelse(treat_A1 == 1, "A1 treated", "Same-prov control")) %>%
  select(group, n, everything(), -treat_A1)
write_csv(tab_cum, "output/tables/tab05_cum_growth_1961_1991.csv")
print(tab_cum)
message("[03] 1961-1991 cumulative growth saved -> output/tables/tab05_cum_growth_1961_1991.csv")

# Figure: density of 1961-1991 population growth, by treatment status
p_dist <- cum %>%
  ggplot(aes(x = d_P, fill = factor(treat_A1))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("0" = "grey60", "1" = "#c0392b"),
                    labels = c("Same-prov control", "A1 treated"),
                    name = NULL) +
  coord_cartesian(xlim = c(-100, 250)) +
  labs(x = "1961-1991 population growth (%)", y = "Density",
       title = "Distribution of 1961-1991 population growth",
       subtitle = "Comuni in A1 provinces") +
  theme_paper()

ggsave("output/figures/fig01_growth_distribution.png", p_dist,
       width = 7, height = 4.5, dpi = 200)
ggsave("output/figures/fig01_growth_distribution.pdf", p_dist,
       width = 7, height = 4.5)

message("[03] Done.")
