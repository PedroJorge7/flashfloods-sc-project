# ============================================================================
# run_main_estimates.R
#
# Runs every script that produces a table or figure appearing in the main
# text of the paper, in the exact order those results appear (Table 1,
# Figure 2, Table 2, Figure 4, Table 3, Table 4, Table 5, Table 6, Figure 5).
# See README.md for details and required inputs.
# ============================================================================

run_start_time <- Sys.time()

resolve_project_root <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) {
    return(normalizePath(dirname(ofile), winslash = "/", mustWork = TRUE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

project_root <- resolve_project_root()
setwd(project_root)
if (!dir.exists(file.path(project_root, "results", "code")) || !dir.exists(file.path(project_root, "data"))) {
  stop("Could not confirm project_root at: ", project_root)
}

script_dir  <- file.path(project_root, "results", "code")
main_dir    <- file.path(script_dir, "Main Estimates")
output_root <- file.path(project_root, "results")
tables_dir  <- file.path(output_root, "analysis")
figures_dir <- file.path(output_root, "analysis")
logs_dir    <- file.path(output_root, "logs")
cache_dir   <- file.path(output_root, "cache")
for (d in c(tables_dir, figures_dir, logs_dir, cache_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

source(file.path(script_dir, "utils", "path_utils.R"))
source(file.path(script_dir, "utils", "utils_establishment.R"))

steps <- c(
  "01_figure1_geographical_distribution_affected_area.R",
  "00a_load_establishment_data.R",
  "00b_load_worker_data.R",
  "01_table1_summary_statistics.R",
  "02_figure2_evolution_aggregate_outcomes.R",
  "03_table2_establishment_adjustment.R",
  "04_figure4_event_study_establishments.R",
  "05_table3_effect_by_sector.R",
  "06_table4_effect_by_business_size.R",
  "07_table5_hazard_establishment_closure.R",
  "08_table6_dismissed_workers.R",
  "09_figure5_event_study_workers.R"
)

for (f in steps) {
  log_msg("--- Running Main Estimates/%s ---", f)
  source(file.path(main_dir, f))
}

log_msg("run_main_estimates.R: all steps complete in %.1f min",
        as.numeric(difftime(Sys.time(), run_start_time, units = "mins")))
