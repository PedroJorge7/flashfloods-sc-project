###############################################################################
# Table F.1 - Worker Level Estimates Increasing Post-Treatment Period (through 2016)
# TREND ONLY:
# - Panel A + Panel B
# - Two columns: Emp_trend / logW_trend
# - Excludes migration
# - Builds LaTeX directly from `tab`, without an intermediate XLSX file
# - Output: ./results/analysis/Tab_F1_Worker_Level_Longer_Panel.tex
###############################################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(MatchIt)

# -------------------------------------------------------------------
# 0) Set paths and ensure the output directory exists
# -------------------------------------------------------------------
OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# 1) Load data with the sample extended through 2016
# -------------------------------------------------------------------
dados <- readRDS(data_path("workers_clean_data.rds")) %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2016))

# -------------------------------------------------------------------
# 2) Load helper functions (output_empregados + gen_table)
# -------------------------------------------------------------------
source("./results/code/read_functions.R")

# -------------------------------------------------------------------
# 3) Outputs (TREND ONLY)
# -------------------------------------------------------------------
output_trend <- output_empregados(dados, 0, 12.5, 50, 80, trend = TRUE)

# -------------------------------------------------------------------
# 4) Build the table from gen_table for the trend-only specification
#    Expected columns: term | Employment | log.Wage | ...
# -------------------------------------------------------------------
output_trend_table <- gen_table(output_trend)

tab <- data.frame(
  term             = output_trend_table[, 1],
  Employment_trend = output_trend_table[, 2],
  log.Wage_trend   = output_trend_table[, 3],
  stringsAsFactors = FALSE
)

need_cols <- c("term", "Employment_trend", "log.Wage_trend")
miss_cols <- setdiff(need_cols, names(tab))
if (length(miss_cols) > 0) {
  stop(paste0("Missing columns in `tab`: ", paste(miss_cols, collapse = ", ")))
}

# -------------------------------------------------------------------
# 5) Generate LaTeX in the main-table format (Panels A and B) for the trend-only specification
#    - Panel A: Flash Flood Post
#    - Panel B: Flash Flood 2008 ... Flash Flood 2016
#    - Observations are stored in the last row returned by gen_table
# -------------------------------------------------------------------
make_workers_tex_expand_post_trend_only <- function(tab,
                                                    years = 2008:2016,
                                                    outfile = file.path(OUT_DIR, "Tab_F1_Worker_Level_Longer_Panel.tex"),
                                                    caption = "Worker-Level Estimates Increasing the Post-Treatment Period",
                                                    label   = "rob3:worker") {
  
  obs_row <- nrow(tab)
  
  obs_emp_trend  <- tab$Employment_trend[obs_row]
  obs_wage_trend <- tab$log.Wage_trend[obs_row]
  
  body <- tab[-obs_row, , drop = FALSE]
  
  # Leave cells blank when the term or standard-error row is missing
  get_pair_safe <- function(term_value) {
    i <- which(trimws(body$term) == term_value)
    if (length(i) == 0 || i == nrow(body)) {
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
    row_gap2("                    ", c(A$se$Employment_trend, A$se$log.Wage_trend)),
    row_gap2("    Observations", c(fmt_obs(obs_emp_trend), fmt_obs(obs_wage_trend))),
    row_gap2("    Census Tract Trend", c("Yes","Yes")),
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
      row_gap2("                    ", c(se$Employment_trend, se$log.Wage_trend))
    )
  }
  
  latex <- c(
    latex,
    "    \\midrule",
    row_gap2("    Observations", c(fmt_obs(obs_emp_trend), fmt_obs(obs_wage_trend))),
    row_gap2("    Census Tract Trend", c("Yes","Yes")),
    "    \\bottomrule",
    "    \\end{tabular}%",
    "    \\begin{tablenotes}[flushleft]",
    "    \\item \\small \\textit{Notes:} This table extends the post-treatment period through 2016 and reports only the census-tract trend specifications. The outcomes are employment (column (1)) and log wage (column (2)). Panel A uses a single post-treatment dummy, whereas Panel B uses time-varying post-treatment dummies (2008-2016), with 2007 as the omitted year. Worker fixed effects and year fixed effects are included in all estimations. Census-tract trend specifications include a tract-specific linear trend. Two-way clustered standard errors (year and CPF) are shown in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
    "    \\end{tablenotes}",
    " \\end{threeparttable}",
    " }",
    "\\end{supptable}%"
  )
  
  writeLines(latex, outfile)
  cat("Saved LaTeX to: ", outfile, "\n", sep = "")
}

make_workers_tex_expand_post_trend_only(tab)

