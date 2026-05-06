############################################################
## Table 7 (Workers) generated directly from the `table` object
## - No intermediate XLSX file
## - Uses a wider spacer column between models (1) and (2) via p{0.60cm}
## - Displays the employment and wage estimates directly from the models
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(readr)
library(MatchIt)

source("./results/code/read_functions.R")

# -------------------------------------------------------------------
# 0) Load data and apply the worker sample filters
# -------------------------------------------------------------------
dados <- readRDS(data_path("workers_clean_data.rds")) %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# -------------------------------------------------------------------
# 1) Run the project output functions
# -------------------------------------------------------------------
output <- output_empregados(
  dados, 0, 12.5, 50, 80
)
output_trend <- output_empregados(
  dados, 0, 12.5, 50, 80,
  trend = TRUE
)

output_table       <- gen_table(output)
output_trend_table <- gen_table(output_trend)

# -------------------------------------------------------------------
# 2) Build the wide table for the displayed worker outcomes
# -------------------------------------------------------------------
table <- cbind(
  output_table[, 1:2],
  Employment_trend = output_trend_table[, 2],
  log.Wage         = output_table[, 3],
  log.Wage_trend   = output_trend_table[, 3]
)

# Force a character data.frame to avoid tibble/matrix binding issues
tab <- as.data.frame(table, stringsAsFactors = FALSE)

# Basic sanity check
need_cols <- c("term", "Employment", "Employment_trend", "log.Wage", "log.Wage_trend")
miss_cols <- setdiff(need_cols, names(tab))
if (length(miss_cols) > 0) {
  stop(paste0("Missing columns in `table`: ", paste(miss_cols, collapse = ", ")))
}

# -------------------------------------------------------------------
# 3) Generate LaTeX from the table object
# -------------------------------------------------------------------
make_workers_tex_from_table <- function(tab,
                                        outfile = "./results/analysis/Tab_07_Effect_Dismissed_Workers.tex",
                                        caption = "The Effect of Disaster-induced Closures on the Dismissed Workers",
                                        label   = "tab:workers_results") {
  
  # The last row usually stores the observation counts.
  obs_row <- nrow(tab)
  
  # Observations are stored as character strings
  obs_emp        <- tab$Employment[obs_row]
  obs_emp_trend  <- tab$Employment_trend[obs_row]
  obs_wage       <- tab$`log.Wage`[obs_row]
  obs_wage_trend <- tab$log.Wage_trend[obs_row]
  
  # Remove the observation row from the body
  body <- tab[-obs_row, , drop = FALSE]
  
  # Helper to retrieve a coefficient row and the standard-error row below it
  get_pair <- function(term_value) {
    i <- which(trimws(body$term) == term_value)
    if (length(i) == 0) stop(paste0("Term not found in `table`: '", term_value, "'"))
    if (i == nrow(body)) stop(paste0("Term '", term_value, "' is on the last row of the body; the standard-error row is missing."))
    list(est = body[i, ],
         se  = body[i + 1, ])
  }
  
  # Panel A
  A <- get_pair("Flash Flood Post")
  
  # Panel B
  years <- 2008:2012
  B <- lapply(years, function(y) get_pair(paste0("Flash Flood ", y)))
  
  # Format observation counts with {,} for LaTeX
  fmt_obs <- function(x) {
    x <- as.character(x)
    x <- gsub(",", "{,}", x, fixed = TRUE)
    x
  }
  
  # Build LaTeX with blank spacer columns between models
  # Use a wider spacer between columns (1) and (2) through p{0.60cm}
  latex <- c(
    "\\begin{table}[htb]",
    "  \\centering",
    paste0("  \\tabcaption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    "  \\scalebox{0.75}{",
    " \\begin{threeparttable}",
    "    \\begin{tabular}{l c p{0.60cm} c p{0.25cm} c p{0.25cm} c}",
    "    \\toprule",
    "          & (1)   &       & (2)   &       & (3)   &       & (4) \\\\",
    "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
    "  \\multicolumn{8}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
    " Dep. Var:   & Employment &       & Employment &       & log Wage &       & log Wage \\\\",
    "    \\midrule",
    paste0("    Flash Flood Post & ",
           A$est$Employment, " & & ",
           A$est$Employment_trend, " & & ",
           A$est$`log.Wage`, " & & ",
           A$est$log.Wage_trend, " \\\\"),
    paste0("                    & ",
           A$se$Employment, " & & ",
           A$se$Employment_trend, " & & ",
           A$se$`log.Wage`, " & & ",
           A$se$log.Wage_trend, " \\\\"),
    paste0("    Observations            & ",
           fmt_obs(obs_emp), " & & ",
           fmt_obs(obs_emp_trend), " & & ",
           fmt_obs(obs_wage), " & & ",
           fmt_obs(obs_wage_trend), " \\\\"),
    "    Census Tract Trend      & No          & & Yes         & & No         & & Yes \\\\",
    "  \\midrule",
    "  \\multicolumn{8}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
    " Dep. Var:   & Employment &       & Employment &       & log Wage &       & log Wage \\\\",
    "    \\midrule"
  )
  
  for (k in seq_along(B)) {
    est <- B[[k]]$est
    se  <- B[[k]]$se
    latex <- c(latex,
               paste0("    ", est$term, " & ",
                      est$Employment, " & & ",
                      est$Employment_trend, " & & ",
                      est$`log.Wage`, " & & ",
                      est$log.Wage_trend, " \\\\"),
               paste0("                    & ",
                      se$Employment, " & & ",
                      se$Employment_trend, " & & ",
                      se$`log.Wage`, " & & ",
                      se$log.Wage_trend, " \\\\")
    )
  }
  
  latex <- c(latex,
             "    \\midrule",
             paste0("    Observations            & ",
                    fmt_obs(obs_emp), " & & ",
                    fmt_obs(obs_emp_trend), " & & ",
                    fmt_obs(obs_wage), " & & ",
                    fmt_obs(obs_wage_trend), " \\\\"),
             "    Census Tract Trend      & No          & & Yes         & & No         & & Yes \\\\",
             "    \\bottomrule",
             "    \\end{tabular}%",
             "    \\begin{tablenotes}[flushleft]",
             "    \\item \\small This table shows estimates from a difference-in-differences model for employment (columns (1) and (2)) and log wage (columns (3) and (4)). Panel A uses a single post-treatment dummy, whereas Panel B uses time-varying post-treatment dummies (2008--2012), with 2007 as the omitted year. The sample is constructed from dismissed workers under the restrictions \\texttt{emprego\\_06\\_07 = 1} and \\texttt{mesma\\_empresa\\_06\\_07 = TRUE}. Worker fixed effects and year fixed effects are included in all estimations. Two-way clustered-robust standard errors (year and CPF) are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
             "    \\end{tablenotes}",
             "    \\end{threeparttable}",
             "    }",
             "\\end{table}"
  )
  
  writeLines(latex, outfile)
  cat("Saved LaTeX to:", outfile, "\n")
}

# -------------------------------------------------------------------
# 4) Write the .tex output
# -------------------------------------------------------------------
make_workers_tex_from_table(tab)
