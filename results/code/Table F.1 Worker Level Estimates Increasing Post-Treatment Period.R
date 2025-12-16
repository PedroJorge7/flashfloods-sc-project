###############################################################################
# Robustez 3 – Worker Level Estimates Increasing Post-Treatment Period (até 2016)
# - Igual ao principal (Panel A + Panel B; 4 colunas: Emp / Emp_trend / logW / logW_trend)
# - SEM migration
# - Gera LaTeX direto do objeto `table` (sem XLSX)
# - Salva em: ./results/analysis/Tab_A12_Worker_Level_Longer_Panel.tex
###############################################################################

rm(list = ls())

library(dplyr)

# -------------------------------------------------------------------
# 0) Paths + garante pasta de saída
# -------------------------------------------------------------------
OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# 1) Dados (expandindo até 2016)
# -------------------------------------------------------------------
dados <- readRDS("./data/workers_clean_data.rds") %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2016))

# -------------------------------------------------------------------
# 2) Carrega funções (output_empregados + gen_table)
# -------------------------------------------------------------------
source("./results/code/Aux.R")

# -------------------------------------------------------------------
# 3) Outputs (sem/ com trend)
# -------------------------------------------------------------------
output       <- output_empregados(dados, 0, 12.5, 50, 80)
output_trend <- output_empregados(dados, 0, 12.5, 50, 80, trend = TRUE)

# -------------------------------------------------------------------
# 4) Tabelas (gen_table) e montagem wide (SEM migration)
#    Esperado:
#      output_table:  term | Employment | log.Wage | (Migration...)
#      output_trend_table: term | Employment | log.Wage | (Migration...)
# -------------------------------------------------------------------
output_table       <- gen_table(output)
output_trend_table <- gen_table(output_trend)

# monta exatamente como você fez no principal, mas parando em log.Wage
table <- cbind(
  output_table[, 1:2],
  Employment_trend = output_trend_table[, 2],
  log.Wage         = output_table[, 3],
  log.Wage_trend   = output_trend_table[, 3]
)

tab <- as.data.frame(table, stringsAsFactors = FALSE)

# sanity check
need_cols <- c("term", "Employment", "Employment_trend", "log.Wage", "log.Wage_trend")
miss_cols <- setdiff(need_cols, names(tab))
if (length(miss_cols) > 0) {
  stop(paste0("Faltam colunas em `table`: ", paste(miss_cols, collapse = ", ")))
}

# -------------------------------------------------------------------
# 5) Gerar LaTeX no formato do “principal” (Panel A + Panel B)
#    - Panel A: Flash Flood Post
#    - Panel B: Flash Flood 2008 ... Flash Flood 2016
#    - Observations: última linha do gen_table (como no seu print)
# -------------------------------------------------------------------
make_workers_tex_expand_post <- function(tab,
                                         years = 2008:2016,
                                         outfile = file.path(OUT_DIR, "Tab_A12_Worker_Level_Longer_Panel.tex"),
                                         caption = "Worker-Level Estimates Increasing the Post-Treatment Period",
                                         label   = "rob3:worker") {
  
  # última linha = Observations (padrão do seu gen_table)
  obs_row <- nrow(tab)
  
  obs_emp        <- tab$Employment[obs_row]
  obs_emp_trend  <- tab$Employment_trend[obs_row]
  obs_wage       <- tab$`log.Wage`[obs_row]
  obs_wage_trend <- tab$log.Wage_trend[obs_row]
  
  body <- tab[-obs_row, , drop = FALSE]
  
  get_pair <- function(term_value) {
    i <- which(trimws(body$term) == term_value)
    if (length(i) == 0) stop(paste0("Termo não encontrado em `table`: '", term_value, "'"))
    if (i == nrow(body)) stop(paste0("Termo '", term_value, "' está na última linha do corpo; faltou a linha do SE."))
    list(est = body[i, ],
         se  = body[i + 1, ])
  }
  
  # Panel A
  A <- get_pair("Flash Flood Post")
  
  # Panel B
  B <- lapply(years, function(y) get_pair(paste0("Flash Flood ", y)))
  
  fmt_obs <- function(x) {
    x <- as.character(x)
    x <- gsub(",", "{,}", x, fixed = TRUE)
    x
  }
  
  latex <- c(
    "\\begin{supptable}[H]",
    "  \\centering",
    paste0("  \\tabcaption{", caption, "} \\label{", label, "}"),
    "  \\scalebox{0.75}{",
    " \\begin{threeparttable}",
    "    \\begin{tabular}{lccccccc}",
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
             "    \\item \\small \\textit{Notes:} This table replicates the main worker-level specifications but extends the post-treatment period through 2016. The outcomes are employment (columns (1) and (2)) and log wage (columns (3) and (4)). Panel A uses a single post-treatment dummy, whereas Panel B uses time-varying post-treatment dummies (2008–2016), with 2007 as the omitted year. Worker fixed effects and year fixed effects are included in all estimations. Census-tract trend specifications include a tract-specific linear trend. Two-way clustered-robust standard errors (establishment and year) are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
             "    \\end{tablenotes}",
             " \\end{threeparttable}",
             " }",
             "\\end{supptable}%"
  )
  
  writeLines(latex, outfile)
  cat("OK: salvou LaTeX em: ", outfile, "\n", sep = "")
}

make_workers_tex_expand_post(tab)
