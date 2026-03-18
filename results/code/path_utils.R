resolve_project_root <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  candidates <- c(
    if (!is.null(ofile) && nzchar(ofile)) dirname(ofile) else NULL,
    getwd()
  )

  for (candidate in candidates) {
    current <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    repeat {
      if (file.exists(file.path(current, "flashfloods-sc-project.Rproj"))) {
        return(current)
      }

      parent <- dirname(current)
      if (identical(parent, current)) {
        break
      }
      current <- parent
    }
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

project_root <- resolve_project_root()

project_path <- function(..., must_work = FALSE) {
  normalizePath(
    file.path(project_root, ...),
    winslash = "/",
    mustWork = must_work
  )
}

resolve_data_root <- function() {
  configured <- Sys.getenv(
    "FLASHFLOODS_DATA_DIR",
    unset = getOption("flashfloods.data_dir", "")
  )

  if (!nzchar(configured)) {
    configured <- if (dir.exists(project_path("data_mask", must_work = FALSE))) {
      "data_mask"
    } else {
      "data"
    }
  }

  is_absolute <- grepl("^[A-Za-z]:[/\\]", configured) ||
    grepl("^[/\\]{2}", configured) ||
    grepl("^/", configured)

  if (is_absolute) {
    normalizePath(configured, winslash = "/", mustWork = FALSE)
  } else {
    project_path(configured, must_work = FALSE)
  }
}

data_path <- function(..., must_work = FALSE) {
  normalizePath(
    file.path(resolve_data_root(), ...),
    winslash = "/",
    mustWork = must_work
  )
}
