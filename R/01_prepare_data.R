# =====================================================================
# 01_prepare_data.R  --  build the long panel
# =====================================================================
#
# Rationale ------------------------------------------------------------
#
# Each xlsx file holds one census year (1951..1991). Variable names use
# a `<YY>_<code>` convention (e.g. 61_P1, 61_UT). To make the panel
# usable we:
#
#   1. read each year separately, keeping only the columns we need;
#   2. strip the year prefix so the column name is the same across years
#      (P1, UT, AT, ...);
#   3. add a `year` column;
#   4. row-bind everything into a single long-format panel.
#
# We keep:
#   - identifiers       PRO_COM, COMUNE, COD_PROV, COD_REG, COD_RIP, Shape_Area
#   - main outcomes     P1 (pop), UT (units), AT (employees), F1 (families)
#   - composition       L15/L16/L17 (sector emp. shares), I4 (illit.), SS4 (edu)
#   - sector detail     U2..U9, A2_1..A9_1 (when available in that year)
#
# We also compute log-transformed outcomes (we use log(1+x) to keep the
# rare zeros) and an employment rate AT/P1. log1p is preferred over log
# because a handful of micro-comuni have UT=0 or AT=0 in 1951 and 1961.

source("R/00_setup.R")

read_census <- function(year) {
  yy   <- substr(as.character(year), 3, 4)
  path <- file.path("archivio", paste0(year, ".xlsx"))
  sheet <- paste0("comuni_", yy)
  raw  <- read_excel(path, sheet = sheet)

  keep <- c(P1   = paste0(yy, "_P1"),
            F1   = paste0(yy, "_F1"),
            I4   = paste0(yy, "_I4"),
            SS4  = paste0(yy, "_SS4"),
            L15  = paste0(yy, "_L15"),
            L16  = paste0(yy, "_L16"),
            L17  = paste0(yy, "_L17"),
            UT   = paste0(yy, "_UT"),
            AT   = paste0(yy, "_AT"))
  ucols <- intersect(paste0(yy, "_U", 2:9),       names(raw))
  acols <- intersect(paste0(yy, "_A", 2:9, "_1"), names(raw))

  out <- raw %>%
    select(COD_RIP, COD_REG, COD_PROV, PRO_COM, COMUNE, Shape_Area,
           any_of(unname(keep)), any_of(c(ucols, acols))) %>%
    rename(any_of(keep))

  # Strip year suffix from any leftover sector columns
  names(out) <- sub(paste0("^", yy, "_"), "", names(out))

  # Force numeric on outcome columns (some xlsx cells came in as text)
  num_cols <- intersect(c("P1","F1","I4","SS4","L15","L16","L17","UT","AT",
                          paste0("U", 2:9), paste0("A", 2:9, "_1")),
                        names(out))
  out[num_cols] <- lapply(out[num_cols], function(x) suppressWarnings(as.numeric(x)))

  out$year <- year
  out
}

# Read all five years and bind them
panel_list <- lapply(CENSUS_YEARS, read_census)

# Some sector columns are missing in some years - fill the gaps with NA
# so bind_rows works.
all_cols <- Reduce(union, lapply(panel_list, names))
panel_list <- lapply(panel_list, function(d) {
  miss <- setdiff(all_cols, names(d))
  for (m in miss) d[[m]] <- NA_real_
  d[, all_cols]
})
panel <- bind_rows(panel_list)

# Province / region IDs sometimes get reorganised across censuses (e.g.
# the Province of Lodi was created in 1992). To keep the cross-section
# of identifiers stable we anchor on 1961.
ids_1961 <- panel %>% filter(year == 1961) %>%
  select(PRO_COM,
         COD_RIP_61  = COD_RIP,
         COD_REG_61  = COD_REG,
         COD_PROV_61 = COD_PROV,
         COMUNE_61   = COMUNE)

panel <- panel %>%
  left_join(ids_1961, by = "PRO_COM") %>%
  mutate(COD_REG  = coalesce(COD_REG_61,  COD_REG),
         COD_PROV = coalesce(COD_PROV_61, COD_PROV),
         COMUNE   = coalesce(COMUNE_61,   COMUNE),
         macro    = macro_area(COD_REG),
         # log outcomes (log1p handles zeros gracefully)
         lP1 = log1p(P1),
         lUT = log1p(UT),
         lAT = log1p(AT),
         emp_rate = ifelse(P1 > 0, AT / P1, NA_real_)) %>%
  select(-ends_with("_61"))

stopifnot(all(CENSUS_YEARS %in% unique(panel$year)))

message(glue("Panel built: {nrow(panel)} obs, ",
             "{length(unique(panel$PRO_COM))} comuni, ",
             "{length(CENSUS_YEARS)} years."))

saveRDS(panel, "data/panel_long.rds")
