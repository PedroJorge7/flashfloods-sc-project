############################################################
## Table D.1: Comparison of Variables Before and After Data
## Balancing
##
## The original worker-side balance table was produced from
## a legacy PSM workflow outside the current repo. The
## reproducible artifact that matches the published paper is
## `results_psm.csv`, which is searched for below and then
## rendered into the paper-style LaTeX table.
############################################################

rm(list = ls())

source("./results/code/path_utils.R")

find_results_psm_csv <- function() {
  user_name <- Sys.info()[["user"]]

  candidates <- c(
    file.path(
      "C:/Users", user_name, "OneDrive", "Pedro Jorge", "Artigos",
      "Concluídos", "Desastre Natural e sobrevivência da firma - Santa Catarina",
      "Estimação final", "data empregados", "results_psm.csv"
    ),
    file.path(
      "C:/Users", user_name, "OneDrive", "Pedro Jorge", "Artigos",
      "Concluidos", "Desastre Natural e sobrevivencia da firma - Santa Catarina",
      "Estimacao final", "data empregados", "results_psm.csv"
    )
  )

  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) {
    return(normalizePath(existing[[1]], winslash = "/", mustWork = TRUE))
  }

  search_root <- file.path("C:/Users", user_name, "OneDrive")
  if (dir.exists(search_root)) {
    hits <- list.files(
      search_root,
      pattern = "^results_psm\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    hits <- hits[grepl("data empregados", hits, fixed = TRUE)]
    if (length(hits) > 0) {
      return(normalizePath(hits[[1]], winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    "Could not find the legacy `results_psm.csv` artifact used to render Table D.1."
  )
}

escape_tex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

legacy_csv <- find_results_psm_csv()

tab_d1 <- read.csv2(
  legacy_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
) |>
  transform(
    variavel_corrigido = ifelse(
      variavel == "rem_med_r",
      "Wage Value",
      variavel_corrigido
    )
  )

expected_order <- c(
  "tamestab", "temp_empr", "rem_med_r", "idade", "male",
  "educ_d1", "educ_d2", "educ_d3", "educ_d4",
  "Comercio", "Construcao", "Industria", "servicos", "transporte"
)

missing_rows <- setdiff(expected_order, tab_d1$variavel)
if (length(missing_rows) > 0) {
  stop(
    "The legacy `results_psm.csv` artifact is missing rows for: ",
    paste(missing_rows, collapse = ", ")
  )
}

tab_d1 <- tab_d1[match(expected_order, tab_d1$variavel), ]

section_map <- c(
  tamestab = "General Characteristics",
  temp_empr = "General Characteristics",
  rem_med_r = "General Characteristics",
  idade = "General Characteristics",
  male = "General Characteristics",
  educ_d1 = "Worker's Education",
  educ_d2 = "Worker's Education",
  educ_d3 = "Worker's Education",
  educ_d4 = "Worker's Education",
  Comercio = "Employment Sector",
  Construcao = "Employment Sector",
  Industria = "Employment Sector",
  servicos = "Employment Sector",
  transporte = "Employment Sector"
)

tab_d1$section <- unname(section_map[tab_d1$variavel])

build_rows <- function(df) {
  sections <- unique(df$section)
  out <- character(0)

  for (sec in sections) {
    out <- c(
      out,
      paste0("    \\multicolumn{7}{l}{\\textbf{", escape_tex(sec), "}}\\\\")
    )

    sec_df <- df[df$section == sec, ]
    for (i in seq_len(nrow(sec_df))) {
      out <- c(
        out,
        paste0(
          "    ",
          escape_tex(sec_df$variavel_corrigido[[i]]), " & ",
          sec_df$mean_treat_unmatched[[i]], " & ",
          sec_df$mean_control_unmatched[[i]], " & ",
          sec_df$diff_unmatched[[i]], " & ",
          sec_df$mean_treat_matched[[i]], " & ",
          sec_df$mean_control_matched[[i]], " & ",
          sec_df$diff_matched[[i]], " \\\\"
        )
      )
    }

    if (!identical(sec, tail(sections, 1))) {
      out <- c(out, "    \\\\")
    }
  }

  out
}

latex_lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{Comparison of Variables Before and After Data Balancing}",
  "  \\label{tab:comparison_variables_before_after_balancing}",
  "  \\scalebox{0.80}{",
  "  \\begin{threeparttable}",
  "    \\begin{tabular}{l c c c c c c}",
  "    \\toprule",
  "      & \\multicolumn{3}{c}{Unmatched Data} & \\multicolumn{3}{c}{Matched Data} \\\\",
  "      \\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
  "      & Treated & Control & Difference & Treated & Control & Difference \\\\",
  "    \\midrule",
  build_rows(tab_d1),
  "    \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  paste0(
    "    \\item \\small This table compares worker characteristics before and ",
    "after the balancing step used in the paper's worker-side analysis. The ",
    "published values are read from the legacy artifact ",
    "\\texttt{results\\_psm.csv}, which was produced by the original PSM ",
    "workflow. *** p$<$0.01, ** p$<$0.05, * p$<$0.1."
  ),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "  }",
  "\\end{table}"
)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

csv_output <- "./results/analysis/Tab_D1_Comparison_Variables_Before_After_Data_Balancing.csv"
if (file.exists(csv_output)) {
  file.remove(csv_output)
}

writeLines(
  latex_lines,
  "./results/analysis/Tab_D1_Comparison_Variables_Before_After_Data_Balancing.tex"
)

cat("Saved LaTeX to ./results/analysis/Tab_D1_Comparison_Variables_Before_After_Data_Balancing.tex\n")
