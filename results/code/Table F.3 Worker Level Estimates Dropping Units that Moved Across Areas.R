############################################################
## Table F.3 - Worker Level Estimates Dropping Units that Moved Across Areas
## Trend-only specification without Migration
## - remove_treat_control_mob = TRUE
## - Table outcomes: Employment and log.Wage
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(MatchIt)

source("./results/code/read_functions.R")
dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 0) Load data and apply the worker sample filters
# ---------------------------------------------------------
dados <- readRDS(data_path("workers_clean_data.rds")) %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# ---------------------------------------------------------
# 1) Run the trend-only specification with the mobility filter
# ---------------------------------------------------------
output_trend <- output_empregados(
  dados, 0, 12.5, 50, 80,
  remove_treat_control_mob = TRUE,
  trend = TRUE
)

tab_trend <- as.data.frame(gen_table(output_trend), stringsAsFactors = FALSE)

# Keep only the outcomes needed for the table
need_cols <- c("term", "Employment", "log.Wage")
miss <- setdiff(need_cols, names(tab_trend))
if (length(miss) > 0) stop(paste0("Missing columns in `tab_trend`: ", paste(miss, collapse = ", ")))

tab <- tab_trend[, need_cols, drop = FALSE]

# ---------------------------------------------------------
# 2) Generate the two-column LaTeX output
# ---------------------------------------------------------
make_workers_tex_trend_only_2cols <- function(
    tab,
    outfile = "./results/analysis/Tab_F2_Worker_Level_Dropping_Units_that_Moved_Across_Areas.tex",
    caption = "Worker Level Estimates Dropping Units that Moved Across Areas",
    label   = "tab:F2_worker_remove_mob"
) {
  obs_row <- nrow(tab)
  
  obs_emp  <- tab$Employment[obs_row]
  obs_wage <- tab$`log.Wage`[obs_row]
  
  body <- tab[-obs_row, , drop = FALSE]
  
  get_pair <- function(term_value) {
    i <- which(trimws(body$term) == term_value)
    if (length(i) == 0) stop(paste0("Term not found in `tab`: '", term_value, "'"))
    if (i == nrow(body)) stop(paste0("Term '", term_value, "' is on the last row; the standard-error row is missing."))
    list(est = body[i, ], se = body[i + 1, ])
  }
  
  A <- get_pair("Flash Flood Post")
  years <- 2008:2012
  B <- lapply(years, function(y) get_pair(paste0("Flash Flood ", y)))
  
  fmt_obs <- function(x) gsub(",", "{,}", as.character(x), fixed = TRUE)
  
  row_gap2 <- function(label, v) {
    stopifnot(length(v) == 2)
    sprintf("%s & %s &  & %s\\\\", label, v[1], v[2])
  }
  
  latex <- c(
    "\\begin{table}[htb]",
    "  \\centering",
    paste0("  \\tabcaption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    "  \\scalebox{0.80}{",
    "  \\begin{threeparttable}",
    "    \\begin{tabular}{l c p{0.60cm} c}",
    "    \\toprule",
    "          & (1)   &       & (2)   \\\\",
    " \\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
    "  \\multicolumn{4}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
    " Dep. Var:   & Employment &       & log Wage \\\\",
    "    \\midrule",
    row_gap2("    Flash Flood Post", c(A$est$Employment, A$est$`log.Wage`)),
    row_gap2("                    ", c(A$se$Employment, A$se$`log.Wage`)),
    row_gap2("    Observations", c(fmt_obs(obs_emp), fmt_obs(obs_wage))),
    row_gap2("    Census Tract Trend", c("Yes", "Yes")),
    "  \\midrule",
    "  \\multicolumn{4}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
    " Dep. Var:   & Employment &       & log Wage \\\\",
    "    \\midrule"
  )
  
  for (k in seq_along(B)) {
    est <- B[[k]]$est
    se  <- B[[k]]$se
    latex <- c(latex,
               row_gap2(paste0("    ", est$term), c(est$Employment, est$`log.Wage`)),
               row_gap2("                    ", c(se$Employment, se$`log.Wage`)))
  }
  
  latex <- c(
    latex,
    "    \\midrule",
    row_gap2("    Observations", c(fmt_obs(obs_emp), fmt_obs(obs_wage))),
    row_gap2("    Census Tract Trend", c("Yes", "Yes")),
    "    \\bottomrule",
    "    \\end{tabular}%",
    "    \\begin{tablenotes}[flushleft]",
    "    \\item \\small Notes: This table uses the main workers sample restrictions and additionally applies \\texttt{remove\\_treat\\_control\\_mob = TRUE}. Only the trend specification is reported (Census tract trend enabled). Panel A uses a single post-treatment dummy; Panel B uses time-varying post-treatment dummies (2008--2012), with 2007 as the omitted year. Two-way clustered-robust standard errors (year and CPF) are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
    "    \\end{tablenotes}",
    "  \\end{threeparttable}",
    "  }",
    "\\end{table}"
  )
  
  writeLines(latex, outfile)
  cat("Saved LaTeX to:", outfile, "\n")
}

make_workers_tex_trend_only_2cols(tab)

