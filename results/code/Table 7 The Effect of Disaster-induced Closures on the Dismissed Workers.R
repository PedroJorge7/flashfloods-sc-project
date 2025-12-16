############################################################
## Table 7 (Workers) – gerar LaTeX direto do objeto "table"
## (sem XLSX, sem migration)
## - AJUSTE: espaço MAIOR entre (1) e (2) via p{0.60cm}
############################################################

rm(list = ls())

library(dplyr)
library(readr)
library(MatchIt)

source("./results/code/Aux.R")

# -------------------------------------------------------------------
# 0) Carregar dados + filtros (EXATAMENTE como você definiu)
# -------------------------------------------------------------------
dados <- readRDS("./data/workers_clean_data.rds") %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# -------------------------------------------------------------------
# 1) Rodar outputs (essas funções você já tem no projeto)
# -------------------------------------------------------------------
output       <- output_empregados(dados, 0, 12.5, 50, 80)
output_trend <- output_empregados(dados, 0, 12.5, 50, 80, trend = TRUE)

output_table       <- gen_table(output)
output_trend_table <- gen_table(output_trend)

# -------------------------------------------------------------------
# 2) Montar a tabela “wide” exatamente como você mostrou
#    Colunas: term | Employment | Employment_trend | log.Wage | log.Wage_trend
# -------------------------------------------------------------------
table <- cbind(
  output_table[, 1:2],
  Employment_trend = output_trend_table[, 2],
  log.Wage         = output_table[, 3],
  log.Wage_trend   = output_trend_table[, 3]
)

# garante data.frame character (evita bugs de cbind tibble/matrix)
tab <- as.data.frame(table, stringsAsFactors = FALSE)

# sanity check mínimo
need_cols <- c("term", "Employment", "Employment_trend", "log.Wage", "log.Wage_trend")
miss_cols <- setdiff(need_cols, names(tab))
if (length(miss_cols) > 0) {
  stop(paste0("Faltam colunas em `table`: ", paste(miss_cols, collapse = ", ")))
}

# -------------------------------------------------------------------
# 3) Gerar LaTeX a partir do objeto tab
# -------------------------------------------------------------------
make_workers_tex_from_table <- function(tab,
                                        outfile = "./results/analysis/Tab_07_Effect_Dismissed_Workers.tex",
                                        caption = "The Effect of Disaster-induced Closures on the Dismissed Workers",
                                        label   = "tab:workers_results") {
  
  # Em geral, o último registro é Observations (como no seu print).
  obs_row <- nrow(tab)
  
  # Observations (vem como char)
  obs_emp        <- tab$Employment[obs_row]
  obs_emp_trend  <- tab$Employment_trend[obs_row]
  obs_wage       <- tab$`log.Wage`[obs_row]
  obs_wage_trend <- tab$log.Wage_trend[obs_row]
  
  # remove obs row do corpo
  body <- tab[-obs_row, , drop = FALSE]
  
  # helpers para pegar linha de coef e a linha imediatamente abaixo (SE)
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
  years <- 2008:2012
  B <- lapply(years, function(y) get_pair(paste0("Flash Flood ", y)))
  
  # formata obs com {,} (estilo LaTeX do seu exemplo)
  fmt_obs <- function(x) {
    x <- as.character(x)
    x <- gsub(",", "{,}", x, fixed = TRUE)
    x
  }
  
  # Monta LaTeX (mesmo estilo com colunas “vazias” entre modelos)
  # AJUSTE: espaço MAIOR entre (1) e (2) usando p{0.60cm} na 3ª coluna
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
             "    \\item \\small This table shows estimates from a difference-in-differences model for employment (columns (1) and (2)) and log wage (columns (3) and (4)). Panel A uses a single post-treatment dummy, whereas Panel B uses time-varying post-treatment dummies (2008--2012), with 2007 as the omitted year. The sample is constructed from dismissed workers under the restrictions \\texttt{emprego\\_06\\_07 = 1} and \\texttt{mesma\\_empresa\\_06\\_07 = TRUE}. Worker fixed effects and year fixed effects are included in all estimations. Two-way clustered-robust standard errors (establishment and year) are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
             "    \\end{tablenotes}",
             "    \\end{threeparttable}",
             "    }",
             "\\end{table}"
  )
  
  writeLines(latex, outfile)
  cat("OK: LaTeX salvo em:", outfile, "\n")
}

# -------------------------------------------------------------------
# 4) Gerar arquivo .tex
# -------------------------------------------------------------------
make_workers_tex_from_table(tab)
