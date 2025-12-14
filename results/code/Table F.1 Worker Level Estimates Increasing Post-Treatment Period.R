###############################################################################
# Table A.12 – Worker-Level Estimates Increasing Post-Treatment Period
###############################################################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(broom)
library(stringr)
library(purrr)

# ---------------------------------------------------------------------------
# 1) Funções auxiliares
# ---------------------------------------------------------------------------

add_stars <- function(p) {
  ifelse(p < 0.01, "***",
         ifelse(p < 0.05, "**",
                ifelse(p < 0.10, "*", "")))
}

fmt_coef <- function(beta, p) {
  sprintf("%.5f%s", beta, add_stars(p))
}

fmt_se <- function(se) {
  sprintf("(%.5f)", se)
}

# extrai uma única variável (Flash Flood Post)
extract_post <- function(mod, var = "treat_B_agg") {
  tt <- broom::tidy(mod)
  tt <- tt[tt$term == var, ]
  tibble(
    coef  = tt$estimate,
    se    = tt$std.error,
    pval  = tt$p.value,
    coef_str = fmt_coef(coef, pval),
    se_str   = fmt_se(se)
  )
}

# extrai as dummies anuais treat_B_2008 ... treat_B_2016
extract_years <- function(mod) {
  tt <- broom::tidy(mod) %>%
    filter(str_detect(term, "^treat_B_20")) %>%
    mutate(
      year = as.integer(str_remove(term, "treat_B_")),
      row_label = paste0("Flash Flood ", year),
      coef_str = fmt_coef(estimate, p.value),
      se_str   = fmt_se(std.error)
    ) %>%
    arrange(year)
  
  tt[, c("year", "row_label", "coef_str", "se_str")]
}

# ---------------------------------------------------------------------------
# 2) Carregar dados
# ---------------------------------------------------------------------------

data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta")

# ---------------------------------------------------------------------------
# 3) Preparação da base – painel longo até 2016
# ---------------------------------------------------------------------------

data <- data %>%
  filter(year >= 2003 & year <= 2016) %>%
  arrange(id_worker, year)

# tratamento: anel tratado vs controle
data <- data %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    )
  )

# forward fill de treat_B dentro do trabalhador
data <- data %>%
  group_by(id_worker) %>%
  fill(treat_B, .direction = "down") %>%
  ungroup()

# variável agregada pós-2008
data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)            ~ 0,
      TRUE                       ~ NA_real_
    )
  )

# dummies de ano para o evento
for (y in 2008:2016) {
  var_name <- paste0("treat_B_", y)
  data[[var_name]] <- ifelse(
    data$year == y & data$treat_B == 1, 1,
    ifelse(data$year == y & data$treat_B == 0, 0, 0)
  )
}

# (census_trend foi criado na sua versão; aqui não usamos nessa tabela)

# ---------------------------------------------------------------------------
# 4) Outcomes
# ---------------------------------------------------------------------------

outcomes <- c("wage", "employment", "hours_worked")
outcome_labels <- c("Wage", "Employment", "Hours Worked")
names(outcome_labels) <- outcomes

treat_vars <- paste0("treat_B_", 2008:2016, collapse = " + ")

# ---------------------------------------------------------------------------
# 5) Rodar modelos e extrair resultados
# ---------------------------------------------------------------------------

models <- list()
res_post  <- list()
res_years <- list()
n_obs     <- list()

for (yvar in outcomes) {
  
  # modelo agregado: Flash Flood Post
  f_post  <- as.formula(paste(yvar, "~ treat_B_agg"))
  m_post  <- feols(
    f_post,
    data = data,
    fixef = c("id_worker", "year"),
    cluster = c("id_worker", "year"),
    lean = TRUE
  )
  
  # modelo com dummies anuais
  f_year  <- as.formula(paste(yvar, "~", treat_vars))
  m_year  <- feols(
    f_year,
    data = data,
    fixef = c("id_worker", "year"),
    cluster = c("id_worker", "year"),
    lean = TRUE
  )
  
  models[[yvar]]   <- list(post = m_post, year = m_year)
  res_post[[yvar]] <- extract_post(m_post)
  res_years[[yvar]]<- extract_years(m_year)
  n_obs[[yvar]]    <- nobs(m_year)
}

# garantir que as linhas de anos estão na mesma ordem para todos
years_df <- res_years[[outcomes[1]]][, c("year", "row_label")]
for (yvar in outcomes[-1]) {
  tmp <- res_years[[yvar]]
  years_df <- years_df %>%
    left_join(tmp[, c("year", "coef_str", "se_str")],
              by = "year",
              suffix = c("", paste0("_", yvar)))
}

# montar estrutura final de linhas (Flash Flood Post + 2008–2016)
table_rows <- character(0)

# 5.1) Linha Flash Flood Post
row_label_post <- "Flash Flood Post"

coef_post_1 <- res_post[[outcomes[1]]]$coef_str
se_post_1   <- res_post[[outcomes[1]]]$se_str

coef_post_2 <- res_post[[outcomes[2]]]$coef_str
se_post_2   <- res_post[[outcomes[2]]]$se_str

coef_post_3 <- res_post[[outcomes[3]]]$coef_str
se_post_3   <- res_post[[outcomes[3]]]$se_str

table_rows <- c(
  table_rows,
  sprintf("    %s & %s &       & %s &       & %s \\\\",
          row_label_post, coef_post_1, coef_post_2, coef_post_3),
  sprintf("          & %s &       & %s &       & %s \\\\",
          se_post_1, se_post_2, se_post_3)
)

# 5.2) Linhas anuais 2008–2016
for (i in seq_len(nrow(years_df))) {
  year_i  <- years_df$year[i]
  label_i <- years_df$row_label[i]
  
  coef_1 <- res_years[[outcomes[1]]]$coef_str[res_years[[outcomes[1]]]$year == year_i]
  se_1   <- res_years[[outcomes[1]]]$se_str  [res_years[[outcomes[1]]]$year == year_i]
  
  coef_2 <- res_years[[outcomes[2]]]$coef_str[res_years[[outcomes[2]]]$year == year_i]
  se_2   <- res_years[[outcomes[2]]]$se_str  [res_years[[outcomes[2]]]$year == year_i]
  
  coef_3 <- res_years[[outcomes[3]]]$coef_str[res_years[[outcomes[3]]]$year == year_i]
  se_3   <- res_years[[outcomes[3]]]$se_str  [res_years[[outcomes[3]]]$year == year_i]
  
  table_rows <- c(
    table_rows,
    sprintf("    %s & %s &       & %s &       & %s \\\\",
            label_i, coef_1, coef_2, coef_3),
    sprintf("          & %s &       & %s &       & %s \\\\",
            se_1, se_2, se_3)
  )
}

# ---------------------------------------------------------------------------
# 6) Montar LaTeX completo (supptable)
# ---------------------------------------------------------------------------

obs_1 <- n_obs[[outcomes[1]]]
obs_2 <- n_obs[[outcomes[2]]]
obs_3 <- n_obs[[outcomes[3]]]

header <- c(
  "\\begin{supptable}[H]",
  "  \\centering",
  "  \\tabcaption{Worker-Level Estimates Increasing the Post-Treatment Period} \\label{rob5:worker}",
  "  \\scalebox{0.7}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccc}",
  "    \\toprule",
  "          & (1)   &       & (2)   &       & (3) \\\\",
  sprintf("\\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}          & %s &       & %s &       & %s \\\\",
          outcome_labels[outcomes[1]],
          outcome_labels[outcomes[2]],
          outcome_labels[outcomes[3]]),
  "    \\midrule"
)

footer <- c(
  "    \\midrule",
  sprintf("    Observations & %s &       & %s &       & %s \\\\", obs_1, obs_2, obs_3),
  "    Worker FE & Yes   &       & Yes   &       & Yes \\\\",
  "    Year FE & Yes   &       & Yes   &       & Yes \\\\",
  "    \\bottomrule",
  "    \\end{tabular}%",
  "   \\begin{tablenotes}[flushleft]",
  "     \\item \\small \\textit{Notes:} This table shows estimates of equation (\\ref{eq.eq1}) using worker-level outcomes: wage, an indicator for employment, and hours worked. The treatment group comprises workers in establishments located up to 12.5 km from the flood spots, and the control group comprises workers in establishments between 50 and 80 km from the flood spots. All specifications include worker and year fixed effects. Standard errors are two-way clustered by worker and year and reported in parentheses. *** indicates $p<0.01$, ** indicates $p<0.05$, and * indicates $p<0.10$.",
  "   \\end{tablenotes}",
  " \\end{threeparttable}",
  " }",
  "\\end{supptable}%"
)

tex_lines <- c(header, table_rows, footer)

# ---------------------------------------------------------------------------
# 7) Gravar em arquivo .tex
# ---------------------------------------------------------------------------


writeLines(tex_lines, "workers/analysis/Tab_A12_Worker_Level_Longer_Panel.tex")
