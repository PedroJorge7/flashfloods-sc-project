# ============================================================================
# Loads the worker-level panel (workers_clean_data.rds) once and sources the
# shared worker helper functions (output_empregados/gen_table/
# event_study_plot), so every downstream worker-level script reuses the same
# in-memory data instead of re-reading the file from disk.
#
# Treatment: 0-5 km from the flood spots. Control: 50-80 km. Standard errors
# clustered at the census-tract level.
# ============================================================================

log_msg("=== 00b_load_worker_data.R: start ===")

# worker_functions.R does not load MatchIt itself; loaded once here since
# every downstream worker script needs it.
library(MatchIt)

source(file.path(script_dir, "utils", "worker_functions.R"))

worker_path <- data_path("workers_clean_data.rds")
if (!file.exists(worker_path)) stop("Input file not found: ", worker_path)

finfo <- file.info(worker_path)
log_msg("Input file: %s (%.1f MB, modified %s)", worker_path, finfo$size / 1e6, format(finfo$mtime))

t0 <- Sys.time()
dados_raw <- readRDS(worker_path)
log_msg("Loaded worker panel: %d rows, %d columns, in %.1f sec (year range %d-%d)",
        nrow(dados_raw), ncol(dados_raw), as.numeric(difftime(Sys.time(), t0, units = "secs")),
        min(dados_raw$year, na.rm = TRUE), max(dados_raw$year, na.rm = TRUE))

# [CORRECTED, 2nd pass] The original pre-filter restricted the sample to
# workers employed at the same establishment in BOTH 2006 and 2007
# (emprego_06_07 == 1 & mesma_empresa_06_07 == TRUE). That restriction makes
# `empregado` (employment status) exactly 1 for 100% of the sample in both
# 2006 and 2007 by construction -- confirmed empirically (zero variation in
# either year). Since the event-study spec includes 2006 as a pre-treatment
# period, its treat x year2006 coefficient was mechanically unidentified
# (zero, not a substantive finding about pre-trends). Removed here so 2006
# (and other pre-period years) carry real employment variation.
dados <- dados_raw
log_msg("No emprego_06_07/mesma_empresa_06_07 pre-filter applied: %d rows", nrow(dados))

# The raw file spans 2002-2016. Most worker-level scripts use 2002-2012
# (`dados_main`); the extended post-treatment-period script (Appendix G.3)
# uses the full 2002-2016 range (`dados`).
dados_main <- dados %>% filter(between(year, 2002, 2012))
log_msg("dados_main (2002-2012): %d rows | dados (full, 2002-2016): %d rows", nrow(dados_main), nrow(dados))

WORKER_MIN_TREAT <- 0
WORKER_MAX_TREAT <- 5
WORKER_MIN_CTRL  <- 50
WORKER_MAX_CTRL  <- 80
WORKER_CLUSTER_FORMULA <- ~code_tract

log_msg("=== 00b_load_worker_data.R: done ===")
