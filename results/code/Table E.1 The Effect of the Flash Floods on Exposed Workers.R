###############################################################################
# Table E.1 - The Effect of the Flash Floods on Exposed Workers (TREND ONLY)
# (universo de trabalhadores expostos na Ã¡rea tratada com matching)
# - Usa output_empregados(..., exposed_workers = TRUE, trend = TRUE)
# - SEM migration
# - Gera LaTeX direto do objeto `tab` (sem XLSX)
# - Salva em: ./results/analysis/Tab_E1_Effect_Exposed_Workers.tex
###############################################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(MatchIt)

# ---------------------------------------------------------
# 0) Output dir
# ---------------------------------------------------------
OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Dados + filtros (seu padrÃ£o)
# ---------------------------------------------------------
dados <- readRDS(data_path("workers_clean_data.rds")) %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# ---------------------------------------------------------
# 2) FunÃ§Ãµes auxiliares do projeto
# ---------------------------------------------------------
source("./results/code/read_functions.R")

# ---------------------------------------------------------
# 3) Rodar outputs (TREND ONLY)
# ---------------------------------------------------------
output_trend <- output_empregados(dados, 0, 12.5, 50, 80, exposed_workers = TRUE, trend = TRUE)
output_trend_table <- gen_table(output_trend)

# ---------------------------------------------------------
# 4) Montar tabela wide (TREND ONLY; SEM migration)
#    gen_table geralmente retorna colunas: term | Employment | log.Wage | ...
# ---------------------------------------------------------
tab <- data.frame(
  term             = output_trend_table[, 1],
  Employment_trend = output_trend_table[, 2],
  log.Wage_trend   = output_trend_table[, 3],
  stringsAsFactors = FALSE
)

need_cols <- c("term", "Employment_trend", "log.Wage_trend")
miss_cols <- setdiff(need_cols, names(tab))
if (length(miss_cols) > 0) {
  stop(paste0("Faltam colunas em `tab`: ", paste(miss_cols, collapse = ", ")))
}

# ---------------------------------------------------------
# 5) FunÃ§Ã£o: gerar LaTeX no formato do principal (TREND ONLY, 2 colunas)
# ---------------------------------------------------------
make_tex_from_table_trend_only <- function(tab,
                                           outfile,
                                           caption,
                                           label = "robA11:exposed_workers",
                                           years = 2008:2012) {
  
  obs_row <- nrow(tab)
  
  obs_emp_trend  <- tab$Employment_trend[obs_row]
  obs_wage_trend <- tab$log.Wage_trend[obs_row]
  
  body <- tab[-obs_row, , drop = FALSE]
  
  get_pair_safe <- function(term_value) {
    i <- which(trimws(body$term) == term_value)
    if (length(i) == 0 || i == nrow(body)) {
      # SAFE: se nÃ£o achar termo ou faltar linha do SE, devolve vazio
      est <- data.frame(term = term_value, Employment_trend = "", log.Wage_trend = "", stringsAsFactors = FALSE)
      se  <- data.frame(term = "",        Employment_trend = "", log.Wage_trend = "", stringsAsFactors = FALSE)
      return(list(est = est, se = se))
    }
    list(est = body[i, , drop = FALSE],
         se  = body[i + 1, , drop = FALSE])
  }
  
  # Panel A
  A <- get_pair_safe("Flash Flood Post")
  
  # Panel B
  B <- lapply(years, function(y) get_pair_safe(paste0("Flash Flood ", y)))
  
  fmt_obs <- function(x) {
    x <- as.character(x)
    gsub(",", "{,}", x, fixed = TRUE)
  }
  
  pad <- "       "
  row_gap2 <- function(label, v) {
    stopifnot(length(v) == 2)
    paste0(label, " & ", v[1], " &", pad, "& ", v[2], "\\\\")
  }
  
  latex <- c(
    "\\begin{supptable}[H]",
    "  \\centering",
    paste0("  \\tabcaption{", caption, "} \\label{", label, "}"),
    "  \\scalebox{0.80}{",
    " \\begin{threeparttable}",
    "    \\begin{tabular}{lccc}",
    "    \\toprule",
    "          & (1)   &       & (2) \\\\",
    "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
    "  \\multicolumn{4}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
    " Dep. Var:   & Employment &       & log Wage \\\\",
    "    \\midrule",
    row_gap2("    Flash Flood Post", c(A$est$Employment_trend, A$est$log.Wage_trend)),
    row_gap2("                    ", c(A$se$Employment_trend,  A$se$log.Wage_trend)),
    row_gap2("    Observations", c(fmt_obs(obs_emp_trend), fmt_obs(obs_wage_trend))),
    row_gap2("    Census Tract Trend", c("Yes", "Yes")),
    "  \\midrule",
    "  \\multicolumn{4}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
    " Dep. Var:   & Employment &       & log Wage \\\\",
    "    \\midrule"
  )
  
  for (k in seq_along(B)) {
    est <- B[[k]]$est
    se  <- B[[k]]$se
    latex <- c(
      latex,
      row_gap2(paste0("    ", est$term), c(est$Employment_trend, est$log.Wage_trend)),
      row_gap2("                    ", c(se$Employment_trend,  se$log.Wage_trend))
    )
  }
  
  latex <- c(
    latex,
    "    \\midrule",
    row_gap2("    Observations", c(fmt_obs(obs_emp_trend), fmt_obs(obs_wage_trend))),
    row_gap2("    Census Tract Trend", c("Yes", "Yes")),
    "    \\bottomrule",
    "    \\end{tabular}%",
    "    \\begin{tablenotes}[flushleft]",
    "    \\item \\small \\textit{Notes:} This table reports worker-level difference-in-differences estimates for exposed workers (treatment area universe with matching). The outcomes are employment (column (1)) and log wage (column (2)). Panel A uses a single post-treatment dummy, whereas Panel B uses time-varying post-treatment dummies (2008-2012), with 2007 as the omitted year. Worker and year fixed effects are included in all specifications. Census-tract trend specifications include a tract-specific linear trend. Two-way clustered-robust standard errors (establishment and year) are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
    "    \\end{tablenotes}",
    " \\end{threeparttable}",
    " }",
    "\\end{supptable}%"
  )
  
  writeLines(latex, outfile)
  cat("OK: salvo em ", outfile, "\n", sep = "")
}

# ---------------------------------------------------------
# 6) Escrever .tex
# ---------------------------------------------------------
make_tex_from_table_trend_only(
  tab     = tab,
  outfile = file.path(OUT_DIR, "Tab_E1_Effect_Exposed_Workers.tex"),
  caption = "The Effect of the Flash Floods on Exposed Workers"
)

