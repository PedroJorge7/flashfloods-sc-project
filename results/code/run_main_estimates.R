# ============================================================================
# run_main_estimates.R  [main estimates]
#
# Re-runs every main-text table/figure using the same scripts as
# Main Estimates/, using utils_establishment.R and
# worker_functions.R: treatment status is fixed
# from each unit's 2007 baseline (instead of being recomputed every year from
# that year's distance to the flood spots), and outcomes are no longer
# nulled to NA when a unit's current-year distance falls outside both the
# treated and control bands (e.g. because it relocated, or a worker moved
# outside Santa Catarina) -- they track the unit's true RAIS history for as
# long as it appears in the data.
#
# Reads input data from data/ at the project root and writes generated files
# to results/analysis/.
# ============================================================================

run_start_time <- Sys.time()

resolve_paths <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
    if (length(file_arg) > 0) ofile <- file_arg[1]
  }
  if (!is.null(ofile) && nzchar(ofile) && file.exists(ofile)) {
    code_dir          <- normalizePath(dirname(ofile), winslash = "/", mustWork = TRUE)
    results_dir       <- dirname(code_dir)
    true_project_root <- dirname(results_dir)
  } else {
    # Last-resort fallback: assume Rscript was invoked at the project root.
    true_project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    results_dir       <- file.path(true_project_root, "results")
    code_dir          <- file.path(results_dir, "code")
  }
  list(code_dir = code_dir, results_dir = results_dir, project_root = true_project_root)
}
paths <- resolve_paths()

project_root <- paths$project_root   # used by data_path() -- true top-level folder with data/
script_dir   <- paths$code_dir
if (!dir.exists(file.path(project_root, "data"))) stop("Could not confirm project_root (data/ not found) at: ", project_root)

main_dir    <- file.path(script_dir, "Main Estimates")
output_root <- file.path(paths$results_dir, "analysis")
tables_dir  <- file.path(output_root, "tables")
figures_dir <- file.path(output_root, "figures")
logs_dir    <- file.path(output_root, "logs")
cache_dir   <- file.path(output_root, "cache")
for (d in c(tables_dir, figures_dir, logs_dir, cache_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

source(file.path(script_dir, "utils", "path_utils.R"))
source(file.path(script_dir, "utils", "utils_establishment.R"))

steps <- c(
  "00a_load_establishment_data.R",
  "00b_load_worker_data.R",
  "01_figure1_geographical_distribution_affected_area.R",
  "01_table1_summary_statistics.R",
  "02_figure2_evolution_aggregate_outcomes.R",
  "03_table2_establishment_adjustment.R",
  "04_figure4_event_study_establishments.R",
  "05_table3_effect_by_sector.R",
  "06_table4_effect_by_business_size.R",
  "07_table5_hazard_establishment_closure.R",
  "08_table6_dismissed_workers.R",
  "09_figure5_event_study_workers.R",
  "10_figure5b_event_study_workers_by_skill.R"
)

for (f in steps) {
  log_msg("--- Running Main Estimates/%s ---", f)
  source(file.path(main_dir, f))
}

log_msg("run_main_estimates.R: all steps complete in %.1f min",
        as.numeric(difftime(Sys.time(), run_start_time, units = "mins")))
