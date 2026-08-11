# data_path() helper. Requires `project_root` to already be in scope.

data_path <- function(...) {
  file.path(project_root, "data", ...)
}
