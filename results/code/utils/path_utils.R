# ============================================================================
# path_utils.R
#
# Small path helper. Requires `project_root` to already be defined in the
# calling environment.
# ============================================================================

data_path <- function(...) {
  file.path(project_root, "data", ...)
}
