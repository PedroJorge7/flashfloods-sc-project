# ============================================================================
# Loads the raw establishment dataset and builds (and caches) the two panels
# used across the establishment-level exhibits:
#   - main_panel:     0-5 km treatment vs. 50-80 km control, years 2003-2012
#   - extended_panel: same treatment/control, years 2003-2016 (Appendix C.4)
# ============================================================================

log_msg("=== 00a_load_establishment_data.R: start ===")

FORCE_REBUILD <- FALSE  # set TRUE to ignore any cached panel and rebuild

raw_path <- data_path("Natural Disastrer Santa Catarina - Dataset.dta")
if (!file.exists(raw_path)) stop("Input file not found: ", raw_path)

finfo <- file.info(raw_path)
log_msg("Input file: %s (%.1f MB, modified %s)", raw_path, finfo$size / 1e6, format(finfo$mtime))

t0 <- Sys.time()
raw_data <- haven::read_dta(raw_path)
log_msg("Loaded raw dataset: %d rows, %d columns, in %.1f sec",
        nrow(raw_data), ncol(raw_data), as.numeric(difftime(Sys.time(), t0, units = "secs")))

treated_rule_5km <- function(d) d <= 5

main_panel <- build_or_load_panel(
  file.path(cache_dir, "main_panel_5km_2003_2012.rds"),
  function() build_establishment_panel(raw_data, treated_rule_5km, "0-5 km (main, 2003-2012)"),
  force_rebuild = FORCE_REBUILD
)

extended_panel <- build_or_load_panel(
  file.path(cache_dir, "extended_panel_5km_2003_2016.rds"),
  function() build_establishment_panel(raw_data, treated_rule_5km, "0-5 km (extended, 2003-2016)",
                                        year_min = 2003, year_max = 2016, dummy_years = 2003:2016),
  force_rebuild = FORCE_REBUILD
)

log_msg("main_panel: %d rows | extended_panel: %d rows", nrow(main_panel), nrow(extended_panel))
log_msg("=== 00a_load_establishment_data.R: done ===")

# raw_data is kept in memory: several Appendix C scripts build their own
# panels with different ring/radius definitions from the same raw source.
