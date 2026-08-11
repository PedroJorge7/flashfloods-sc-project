# ============================================================================
# run_single_appendix_step.R  [one-off runner]
#
# Runs exactly ONE appendix script in an isolated Rscript process, so its
# memory is fully released when the process exits before the next step
# starts. Needed because the combined multi-script run
# (run_workers_full_update.R) accumulates raw_data (1.35M x 75), dados_raw
# (12M x 29), main_panel, extended_panel, dados/dados_main, and every fitted
# model from EVERY prior script in the SAME R session (nothing is ever
# rm()'d between sourced scripts) -- which exhausted this machine's 7.7 GB
# RAM during F.1's exposed-workers PSM (std::bad_alloc in MatchIt's C++
# nearest-neighbor matcher) once the emprego_06_07 filter no longer bounds
# the worker population.
#
# Usage: Rscript run_single_appendix_step.R <relative/path/to/script.R> [needs_00a]
#   needs_00a: "1" to also source Main Estimates/00a (only H.1 needs this,
#              for `main_panel`); omit or "0" otherwise.
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript run_single_appendix_step.R <relative/path/to/script.R> [needs_00a]")
target_step <- args[1]
needs_00a <- length(args) >= 2 && args[2] == "1"

resolve_paths <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) {
    all_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- sub("^--file=", "", all_args[grepl("^--file=", all_args)])
    if (length(file_arg) > 0) ofile <- file_arg[1]
  }
  if (!is.null(ofile) && nzchar(ofile) && file.exists(ofile)) {
    code_dir          <- normalizePath(dirname(ofile), winslash = "/", mustWork = TRUE)
    results_dir       <- dirname(code_dir)
    true_project_root <- dirname(results_dir)
  } else {
    true_project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    results_dir       <- file.path(true_project_root, "results")
    code_dir          <- file.path(results_dir, "code")
  }
  list(code_dir = code_dir, results_dir = results_dir, project_root = true_project_root)
}
paths <- resolve_paths()

project_root <- paths$project_root
script_dir   <- paths$code_dir
if (!dir.exists(file.path(project_root, "data"))) stop("Could not confirm project_root (data/ not found) at: ", project_root)

main_dir     <- file.path(script_dir, "Main Estimates")
appendix_dir <- file.path(script_dir, "Appendix")
output_root  <- file.path(paths$results_dir, "analysis")
tables_dir   <- file.path(output_root, "tables")
figures_dir  <- file.path(output_root, "figures")
logs_dir     <- file.path(output_root, "logs")
cache_dir    <- file.path(output_root, "cache")
for (d in c(tables_dir, figures_dir, logs_dir, cache_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

source(file.path(script_dir, "utils", "path_utils.R"))
source(file.path(script_dir, "utils", "utils_establishment.R"))

if (needs_00a) {
  source(file.path(main_dir, "00a_load_establishment_data.R"))
}
source(file.path(main_dir, "00b_load_worker_data.R"))

log_msg("--- Running (isolated) %s ---", target_step)
source(file.path(appendix_dir, target_step))
log_msg("--- Done (isolated) %s ---", target_step)
