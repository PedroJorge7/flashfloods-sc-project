rm(list = ls())

library(haven)

source("./results/code/path_utils.R")

source_dir <- project_path("data", must_work = TRUE)
source_dir_norm <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
target_dir <- project_path("data_mask", must_work = FALSE)

dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

tracked_ids <- list(
  id_estab = list(prefix = "firm", alias = "firm_id"),
  cpf = list(prefix = "worker", alias = "worker_id"),
  pis = list(prefix = "pis", alias = "pis_id")
)

sensitive_patterns <- c(
  "nome",
  "name",
  "razao",
  "social",
  "fantasia"
)

sensitive_exact_names <- c(
  "email",
  "telefone",
  "celular",
  "logradouro",
  "endereco",
  "bairro",
  "cep"
)

all_files <- list.files(
  source_dir,
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = FALSE
)

supported_extensions <- c("rds", "dta", "xlsx")
data_files <- all_files[tolower(tools::file_ext(all_files)) %in% supported_extensions]

if (length(data_files) == 0) {
  stop("Nenhum arquivo suportado foi encontrado em ./data.")
}

value_to_key <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(x)
  }

  if (inherits(x, "Date")) {
    out <- format(x, "%Y-%m-%d")
  } else if (is.numeric(x)) {
    out <- trimws(format(x, scientific = FALSE, trim = TRUE, digits = 22))
  } else {
    out <- as.character(x)
  }

  out <- iconv(out, from = "", to = "UTF-8", sub = "byte")
  out <- trimws(out)
  out[out %in% c("", "NA", "<NA>")] <- NA_character_
  out
}

read_supported_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext == "rds") {
    return(readRDS(path))
  }

  if (ext == "dta") {
    return(haven::read_dta(path))
  }

  if (ext == "xlsx") {
    return(NULL)
  }

  stop(sprintf("Formato nao suportado: %s", path))
}

copy_as_is <- function(source_path, target_path) {
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(source_path, target_path, overwrite = TRUE)
  if (!ok) {
    stop(sprintf("Falha ao copiar arquivo: %s", source_path))
  }
}

relative_to_data <- function(path) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  substring(normalized_path, nchar(source_dir_norm) + 2L)
}

collect_sensitive_columns <- function(paths) {
  sensitive <- character()

  for (path in paths) {
    obj <- read_supported_file(path)
    if (!is.data.frame(obj)) {
      next
    }

    for (nm in names(obj)) {
      nm_lower <- tolower(nm)
      if (nm %in% names(tracked_ids)) {
        sensitive <- union(sensitive, nm)
        next
      }

      if ((nm_lower %in% sensitive_exact_names) || any(vapply(sensitive_patterns, function(pattern) grepl(pattern, nm_lower, ignore.case = TRUE), logical(1)))) {
        sensitive <- union(sensitive, nm)
      }
    }
  }

  sensitive
}

build_mapping <- function(paths, column, prefix) {
  values <- character()

  for (path in paths) {
    obj <- read_supported_file(path)
    if (!is.data.frame(obj) || !(column %in% names(obj))) {
      next
    }

    values <- c(values, value_to_key(obj[[column]]))
  }

  values <- sort(unique(values[!is.na(values)]))
  width <- max(7L, nchar(as.character(length(values))))
  masked <- sprintf("%s_%0*d", prefix, width, seq_along(values))
  stats::setNames(masked, values)
}

apply_mapping <- function(x, mapping) {
  key <- value_to_key(x)
  out <- rep(NA_character_, length(key))
  keep <- !is.na(key)
  out[keep] <- unname(mapping[key[keep]])
  out
}

sensitive_columns <- collect_sensitive_columns(data_files)

column_mappings <- list()
for (column in sensitive_columns) {
  prefix <- if (column %in% names(tracked_ids)) {
    tracked_ids[[column]]$prefix
  } else {
    gsub("[^a-z0-9]+", "_", tolower(column))
  }
  column_mappings[[column]] <- build_mapping(data_files, column, prefix)
}

for (source_path in all_files) {
  relative_path <- relative_to_data(source_path)
  target_path <- file.path(target_dir, relative_path)
  extension <- tolower(tools::file_ext(source_path))

  if (!(extension %in% supported_extensions) || extension == "xlsx") {
    copy_as_is(source_path, target_path)
    next
  }

  obj <- read_supported_file(source_path)
  if (!is.data.frame(obj)) {
    copy_as_is(source_path, target_path)
    next
  }

  for (column in names(column_mappings)) {
    if (!(column %in% names(obj))) {
      next
    }

    masked <- apply_mapping(obj[[column]], column_mappings[[column]])
    obj[[column]] <- masked

    if (column %in% names(tracked_ids)) {
      alias <- tracked_ids[[column]]$alias
      obj[[alias]] <- masked
    }
  }

  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  if (extension == "rds") {
    saveRDS(obj, target_path)
  } else if (extension == "dta") {
    haven::write_dta(obj, target_path, version = 14)
  }
}

metadata <- data.frame(
  column = names(column_mappings),
  unique_values = vapply(column_mappings, length, integer(1)),
  stringsAsFactors = FALSE
)

write.csv(
  metadata,
  file = file.path(target_dir, "mask_metadata.csv"),
  row.names = FALSE
)

message(sprintf("Mascara criada em: %s", target_dir))
