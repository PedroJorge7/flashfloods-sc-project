############################################################
## Appendix – Table B.3 (R)
## Establishment-Level Estimates Increasing the Post-Treatment Period
## - Janela: 2003–2016
## - IGUAL Table 3: 4 colunas (Closure, Closure+Trend, Reloc, Reloc+Trend)
## - ONLY: morte e reloc_tract_tminus1
## - Relocation = Census Tract, t-1 (via code_tract), igual Table 3 que você fixou
## - Cluster 2-way: id_estab + year
## - SAFE: se algum termo (ex.: treat_B_2016) for dropado, NÃO quebra (deixa vazio)
## - Salva com o NOME CERTO:
##   ./results/analysis/Table_B3_Establishment_Level_Estimates_Increasing_Post_Treatment_Period.tex
############################################################

rm(list = ls())

library(dplyr)
library(haven)
library(fixest)
library(broom)

# ---------------------------------------------------------
# 0) Output dir + nome CERTO do .tex
# ---------------------------------------------------------
OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTFILE <- file.path(
  OUT_DIR,
  "Table_B3_Establishment_Level_Estimates_Increasing_Post_Treatment_Period.tex"
)

# ---------------------------------------------------------
# 1) Load data (NÃO corta antes de construir t-1)
# ---------------------------------------------------------
data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

need <- c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract")
miss <- setdiff(need, names(data))
if (length(miss) > 0) stop("Faltam colunas na base: ", paste(miss, collapse = ", "))

# ---------------------------------------------------------
# 2) Replicar Stata treatment construction (igual Table 3)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  mutate(
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
# 2.1) Construir reloc_tract_tminus1 via code_tract (Census Tract, t-1)
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

# ---------------------------------------------------------
# 2.2) REGRA do primeiro ano observado
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.3) Regra de amostra (igual Table 3):
#      (i) NA do relocation vira 0
#      (ii) se Closure é NA => Relocation vira NA
# ---------------------------------------------------------
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1), 0, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  )

# ---------------------------------------------------------
# 3) treat_B_agg + dummies 2008–2016 + trend vars
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) {
    x_num <- as.numeric(factor(x_chr))
  }
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

for (y in 2008:2016) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 4) Restrict 2003–2016
# ---------------------------------------------------------
data_tab <- data %>%
  filter(year >= 2003 & year <= 2016)

# ---------------------------------------------------------
# 5) Estimate models (4 colunas, igual Table 3)
# ---------------------------------------------------------
treat_vars_B <- paste0("treat_B_", 2008:2016, collapse = " + ")

# Panel A
mA1_cl <- feols(morte ~ treat_B_agg | id_estab + year,
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)
mA2_cl <- feols(morte ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)
mA1_rl <- feols(reloc_tract_tminus1 ~ treat_B_agg | id_estab + year,
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)
mA2_rl <- feols(reloc_tract_tminus1 ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)

# Panel B
mB1_cl <- feols(as.formula(paste0("morte ~ ", treat_vars_B, " | id_estab + year")),
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)
mB2_cl <- feols(as.formula(paste0("morte ~ ", treat_vars_B, " | id_estab + year + treat_trend[code_tract_num]")),
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)
mB1_rl <- feols(as.formula(paste0("reloc_tract_tminus1 ~ ", treat_vars_B, " | id_estab + year")),
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)
mB2_rl <- feols(as.formula(paste0("reloc_tract_tminus1 ~ ", treat_vars_B, " | id_estab + year + treat_trend[code_tract_num]")),
                data = data_tab, cluster = ~ id_estab + year, lean = TRUE
)

mods_A <- list(mA1_cl, mA2_cl, mA1_rl, mA2_rl)
mods_B <- list(mB1_cl, mB2_cl, mB1_rl, mB2_rl)

# ---------------------------------------------------------
# 6) Helpers (SAFE p/ termo dropado)
# ---------------------------------------------------------
stars <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.1)  "*"
  else ""
}
fmt_coef <- function(est, p) if (is.na(est)) "" else sprintf("%.5f%s", est, stars(p))
fmt_se   <- function(se) if (is.na(se)) "" else sprintf("(%.5f)", se)
fmt_obs  <- function(n) gsub(",", "{,}", format(n, big.mark = ",", scientific = FALSE), fixed = TRUE)

get_est_safe <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate","std.error","p.value")]
  if (nrow(out) == 0) return(data.frame(estimate=NA_real_, std.error=NA_real_, p.value=NA_real_))
  out[1, , drop = FALSE]
}

pad <- "       "
row_gap4 <- function(label, v) {
  stopifnot(length(v) == 4)
  paste0(label, " & ", v[1], " &", pad, "& ", v[2], " &", pad, "& ", v[3], " &", pad, "& ", v[4], "\\\\")
}

# ---------------------------------------------------------
# 7) Build LaTeX
# ---------------------------------------------------------
lines <- c(
  "\\begin{supptable}[H]",
  "  \\centering",
  "  \\tabcaption{Establishment-Level Estimates Increasing the Post-Treatment Period}",
  "  \\label{rob5:est}",
  "  \\scalebox{0.70}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccc}",
  "    \\toprule",
  "          & (1)   &       & (2)   &       & (3)   &       & (4) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  "    \\multicolumn{8}{l}{\\textbf{Panel A: Time-Agregatted DiD}}\\\\",
  "    Dep. Var: & Closure & & Closure & & Relocation (Tract, t-1) & & Relocation (Tract, t-1) \\\\",
  "    \\midrule"
)

rowA  <- lapply(mods_A, get_est_safe, term = "treat_B_agg")
coefA <- sapply(rowA, \(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(rowA, \(x) fmt_se(x$std.error))

lines <- c(lines,
           row_gap4("    Flash Flood Post", coefA),
           row_gap4("                     ", seA)
)

obsA <- sapply(mods_A, nobs)
lines <- c(lines,
           row_gap4("    Observations", sapply(obsA, fmt_obs)),
           row_gap4("    Census Tract Trend", c("No","Yes","No","Yes")),
           "    \\midrule",
           "    \\multicolumn{8}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
           "    Dep. Var: & Closure & & Closure & & Relocation & & Relocation \\\\",
           "    \\midrule"
)

for (yr in 2008:2016) {
  term <- paste0("treat_B_", yr)
  rowB  <- lapply(mods_B, get_est_safe, term = term)
  coefB <- sapply(rowB, \(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(rowB, \(x) fmt_se(x$std.error))
  lines <- c(lines,
             row_gap4(sprintf("    Flash Flood %d", yr), coefB),
             row_gap4("                     ", seB)
  )
}

obsB <- sapply(mods_B, nobs)
lines <- c(lines,
           row_gap4("    Observations", sapply(obsB, fmt_obs)),
           row_gap4("    Census Tract Trend", c("No","Yes","No","Yes")),
           "    \\bottomrule",
           "    \\end{tabular}%",
           "   \t\\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table extends the post-treatment period through 2016 relative to the baseline specification. Closure is measured by \\textit{morte}. Relocation is defined as \\textit{Census Tract, t-1}: $reloc\\_tract\\_tminus1(t)=1$ if the establishment changes census tract between $t$ and $t+1$ (constructed from \\texttt{code\\_tract} using a lead of the tract-change indicator), with the first observed year forced to have $reloc\\_tract\\_tminus1=0$ when it would otherwise be 1. Panel A uses a single post-treatment dummy; Panel B uses time-varying treatment dummies (2008--2016). The treatment radius ranges from 0 to 12.5 km and the control ring is 50--80 km. Establishment and year fixed effects are included in all estimations. The census tract trend is included in columns (2) and (4). Two-way clustered-robust standard errors (establishment and year) in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
           "   \t\\end{tablenotes}",
           "   \t\\end{threeparttable}",
           "   \t}",
           "\\end{supptable}%"
)

writeLines(lines, OUTFILE)
message("OK — salvou: ", OUTFILE)
