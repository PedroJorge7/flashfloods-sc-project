############################################################
## Table B.8 - Establishment-Level Estimates Dropping Units that Moved Across Areas
## Arquivo mantido neste caminho legado, mas o conteudo pertence ao apendice B.
## - Apenas especificaÃ§Ã£o COM tendÃªncia (Census tract trend)
## - Outcomes: Closure (morte) e Relocation (reloc_tract_tminus1)
## - Mesma construÃ§Ã£o do treat_B da Tabela 3
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(haven)
library(fixest)
library(broom)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load (nÃ£o corta antes de construir t-1)
# ---------------------------------------------------------
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

stopifnot(all(c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract") %in% names(data)))

# ---------------------------------------------------------
# 2) ConstruÃ§Ã£o do tratamento (igual seu script principal)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  mutate(
    # Stata: replace treat_B = treat_B[_n-1] if treat_B==. & mover_ano_mun==1
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) tb[i] <- tb[i - 1]
      }
      tb
    }
  ) %>%
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.1) Relocation (Census Tract, t-1) via code_tract
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct     = as.character(code_tract),
    ct_lag = lag(ct, 1),
    diff_tract = case_when(
      is.na(ct) | is.na(ct_lag) ~ NA_real_,
      ct != ct_lag              ~ 1,
      TRUE                      ~ 0
    ),
    reloc_tract_tminus1 = lead(diff_tract, 1)
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    reloc_tract_tminus1 = as.numeric(reloc_tract_tminus1)
  ) %>%
  select(-ct, -ct_lag, -diff_tract)

# regra do 1Âº ano do id_estab
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    )
  ) %>%
  ungroup()

# sua regra: se morte Ã© NA, reloc tambÃ©m NA (e define NA -> 0 antes disso, como no seu script)
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1), 0, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  )

# ---------------------------------------------------------
# 3) Dummies + tendÃªncia (igual seu script)
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) x_num <- as.numeric(factor(x_chr))
  x_num
}

data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    ),
    treat_trend    = if_else(year >= 2008, 1, 0),
    code_tract_num = make_tract_num(code_tract)
  )

for (y in 2008:2012) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 4) Janela da tabela + FILTRO treat_max==treat_min
# ---------------------------------------------------------
data_tab3 <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  group_by(id_estab) %>%
  mutate(
    treat_max = if_else(all(is.na(treat_B)), NA_real_, max(treat_B, na.rm = TRUE)),
    treat_min = if_else(all(is.na(treat_B)), NA_real_, min(treat_B, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  filter(!is.na(treat_max) & treat_max == treat_min) %>%
  select(-treat_max, -treat_min)

# ---------------------------------------------------------
# 5) RegressÃµes (APENAS COM tendÃªncia)
# ---------------------------------------------------------
# Panel A (agg)
mA_closure <- feols(morte ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
                    data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)

mA_reloc   <- feols(reloc_tract_tminus1 ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
                    data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)

# Panel B (time-varying)
treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")

mB_closure <- feols(as.formula(paste0("morte ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]")),
                    data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)

mB_reloc   <- feols(as.formula(paste0("reloc_tract_tminus1 ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]")),
                    data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)

# ---------------------------------------------------------
# 6) LaTeX (2 colunas: Closure+Trend | Reloc+Trend)
# ---------------------------------------------------------
get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate","std.error","p.value")]
  if (nrow(out) == 0) stop(paste("Termo nÃ£o encontrado:", term))
  out
}
stars <- function(p) ifelse(p < 0.01,"***", ifelse(p < 0.05,"**", ifelse(p < 0.1,"*","")))
fmt_coef <- function(est,p) sprintf("%.5f%s", est, stars(p))
fmt_se   <- function(se) sprintf("(%.5f)", se)
fmt_obs  <- function(n) gsub(",", "{,}", format(n, big.mark = ",", scientific = FALSE), fixed = TRUE)
row_gap2 <- function(label, v) sprintf("%s & %s &  & %s\\\\", label, v[1], v[2])

mods_A <- list(mA_closure, mA_reloc)
mods_B <- list(mB_closure, mB_reloc)

lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{Robustness: Removing establishments that switch between treated and control areas}",
  "  \\label{tab:B8_remove_mobility_estab}",
  "  \\scalebox{0.80}{",
  "  \\begin{threeparttable}",
  "    \\begin{tabular}{l c p{0.35cm} c}",
  "    \\toprule",
  "          & (1)   &       & (2)   \\\\",
  " \\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
  "    \\multicolumn{4}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  "    Dep. Var: & Closure & & Relocation (Tract, t-1) \\\\",
  "    \\midrule"
)

# Panel A coef
rowA  <- lapply(mods_A, get_est, term = "treat_B_agg")
coefA <- sapply(rowA, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(rowA, function(x) fmt_se(x$std.error))

lines <- c(lines,
           row_gap2("    Flash Flood Post", coefA),
           row_gap2("                     ", seA),
           row_gap2("    Observations", sapply(sapply(mods_A, nobs), fmt_obs)),
           row_gap2("    Census Tract Trend", c("Yes","Yes")),
           "    \\midrule",
           "    \\multicolumn{4}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
           "    Dep. Var: & Closure & & Relocation (Tract, t-1) \\\\",
           "    \\midrule"
)

for (yr in 2008:2012) {
  term <- paste0("treat_B_", yr)
  rowB  <- lapply(mods_B, get_est, term = term)
  coefB <- sapply(rowB, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(rowB, function(x) fmt_se(x$std.error))
  
  lines <- c(lines,
             row_gap2(sprintf("    Flash Flood %d", yr), coefB),
             row_gap2("                     ", seB))
}

lines <- c(lines,
           "    \\midrule",
           row_gap2("    Observations", sapply(sapply(mods_B, nobs), fmt_obs)),
           row_gap2("    Census Tract Trend", c("Yes","Yes")),
           "    \\bottomrule",
           "    \\end{tabular}%",
           "    \\begin{tablenotes}[flushleft]",
           "    \\item \\small Sample restriction: establishments with \\texttt{treat\\_max == treat\\_min} (i.e., they never switch between treated and control rings over the panel window). All models include establishment and year fixed effects, plus Census Tract trend implemented as \\texttt{treat\\_trend[code\\_tract\\_num]}. Two-way clustered standard errors (establishment and year) in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
           "    \\end{tablenotes}",
           "  \\end{threeparttable}",
           "  }",
           "\\end{table}"
)

outfile <- "./results/analysis/Tab_B8_Removing_Mobility_TreatControl_Establishments.tex"
writeLines(lines, outfile)



