############################################################
## Table 3 – The Effect of the Flash Floods on the Spatial
##            Distribution of Establishments (R version)
##            ONLY: Closure (morte) and Relocation (reloc_tract_tminus1)
##            Relocation = Census Tract, t-1 (via code_tract)
##            LaTeX with blank spacer columns between models
############################################################

rm(list = ls())

library(dplyr)
library(haven)
library(fixest)
library(broom)

# ---------------------------------------------------------
# 0) Output dir
# ---------------------------------------------------------
dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load data (NÃO corta antes de construir t-1)
# ---------------------------------------------------------
data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

stopifnot(all(c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract") %in% names(data)))

# ---------------------------------------------------------
# 2) Replicate Stata treatment construction
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    # band rule
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    # ensure numeric (avoid haven_labelled issues)
    morte         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    
    # outcomes become NA where treat_B is missing (Stata)
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  # Stata: bysort id_estab (year): replace treat_B = treat_B[_n-1]
  #        if treat_B==. & mover_ano_mun==1
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # Stata: replace mover_ano_mun = . if treat_B == .
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.1) Construir reloc_tract_tminus1 via code_tract (Census Tract, t-1)
#   diff_tract(t) = 1 se ct(t) != ct(t-1)
#   reloc_tract_tminus1(t) = diff_tract(t+1)  (mudança entre t e t+1)
#   SEM default=0; fica NA no último ano do id_estab
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
# 2.2) REGRA QUE VOCÊ MANDOU:
#   no primeiro ano observado do estabelecimento:
#   se reloc_tract_tminus1 == 1 -> vira 0
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
# 2.3) Sua regra (ONLY this direction):
#      if Closure is NA, Relocation must be NA
# ---------------------------------------------------------
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  )

# ---------------------------------------------------------
# 3) Create aggregated post + annual dummies + "trend control"
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  
  # se muita coisa não parseia, usa fator -> numérico (mantém todos)
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
    treat_trend    = if_else(year >= 2008, 1, 0), # 0/1
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
# 4) Restrict to 2003–2012 (Table 3 window)
# ---------------------------------------------------------
data_tab3 <- data %>%
  filter(year >= 2003 & year <= 2012)


# ---------------------------------------------------------
# 5) Estimate models (two-way clustered SE: id_estab + year)
#    "Census Tract Trend" (Stata-style i.treat_trend#c.code_tract):
#      fixest varying slopes: treat_trend[code_tract_num]
# ---------------------------------------------------------
outcomes <- c("morte", "reloc_tract_tminus1")

panelA <- list()
panelB <- list()

for (outcome in outcomes) {
  
  # Panel A (aggregated)
  fml_A1 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year"))
  fml_A2 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num]"))
  
  mA1 <- feols(fml_A1, data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)
  mA2 <- feols(fml_A2, data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)
  
  panelA[[outcome]] <- list(no_trend = mA1, trend = mA2)
  
  # Panel B (time-varying dummies)
  treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")
  fml_B1 <- as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year"))
  fml_B2 <- as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]"))
  
  mB1 <- feols(fml_B1, data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)
  mB2 <- feols(fml_B2, data = data_tab3, cluster = ~ id_estab + year, lean = TRUE)
  
  panelB[[outcome]] <- list(no_trend = mB1, trend = mB2)
}

# Order columns: (1) Closure, (2) Closure+Trend, (3) Reloc_tract_tminus1, (4) Reloc+Trend
mods_A <- list(
  panelA[["morte"]]$no_trend,
  panelA[["morte"]]$trend,
  panelA[["reloc_tract_tminus1"]]$no_trend,
  panelA[["reloc_tract_tminus1"]]$trend
)

mods_B <- list(
  panelB[["morte"]]$no_trend,
  panelB[["morte"]]$trend,
  panelB[["reloc_tract_tminus1"]]$no_trend,
  panelB[["reloc_tract_tminus1"]]$trend
)

# ---------------------------------------------------------
# 6) Helpers
# ---------------------------------------------------------
get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate", "std.error", "p.value")]
  if (nrow(out) == 0) stop(paste("Termo não encontrado:", term))
  out
}

stars <- function(p) {
  ifelse(p < 0.01, "***",
         ifelse(p < 0.05, "**",
                ifelse(p < 0.1, "*", "")))
}

fmt_coef <- function(est, p) sprintf("%.5f%s", est, stars(p))
fmt_se   <- function(se) sprintf("(%.5f)", se)

fmt_obs <- function(n) {
  s <- format(n, big.mark = ",", scientific = FALSE)
  gsub(",", "{,}", s, fixed = TRUE)
}

row_gap4 <- function(label, v) {
  stopifnot(length(v) == 4)
  sprintf("%s & %s &  & %s &  & %s &  & %s\\\\", label, v[1], v[2], v[3], v[4])
}

# ---------------------------------------------------------
# 7) Build LaTeX (ONLY closure + relocation tract t-1)
# ---------------------------------------------------------
lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on the Spatial Distribution of Establishments}",
  "  \\label{tab3: main_results}",
  "  \\scalebox{0.75}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccc}",
  "    \\toprule",
  "          & (1)   &       & (2)   &       & (3)   &       & (4) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  "    \\multicolumn{8}{l}{\\textbf{Panel A: Time-Agregatted DiD}}\\\\",
  "    Dep. Var: & Closure & & Closure & & Relocation (Tract, t-1) & & Relocation (Tract, t-1) \\\\",
  "   \\midrule"
)

# Panel A: Flash Flood Post (treat_B_agg)
rowA  <- lapply(mods_A, get_est, term = "treat_B_agg")
coefA <- sapply(rowA, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(rowA, function(x) fmt_se(x$std.error))

lines <- c(lines,
           row_gap4("    Flash Flood Post", coefA),
           row_gap4("                     ", seA))

obsA <- sapply(mods_A, nobs)
lines <- c(lines,
           row_gap4("    Observations", sapply(obsA, fmt_obs)),
           row_gap4("    Census Tract Trend", c("No","Yes","No","Yes")),
           "    \\midrule",
           "    \\multicolumn{8}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
           "    Dep. Var: & Closure & & Closure & & Relocation  & & Relocation \\\\",
           "    \\midrule"
)

# Panel B: 2008–2012
years_evt <- 2008:2012
terms_evt <- paste0("treat_B_", years_evt)

for (k in seq_along(years_evt)) {
  yr   <- years_evt[k]
  term <- terms_evt[k]
  
  rowB  <- lapply(mods_B, get_est, term = term)
  coefB <- sapply(rowB, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(rowB, function(x) fmt_se(x$std.error))
  
  lines <- c(lines,
             row_gap4(sprintf("    Flash Flood %d", yr), coefB),
             row_gap4("                     ", seB))
}

obsB <- sapply(mods_B, nobs)
lines <- c(
  lines,
  row_gap4("    Observations", sapply(obsB, fmt_obs)),
  row_gap4("    Census Tract Trend", c("No","Yes","No","Yes")),
  "    \\bottomrule",
  "    \\end{tabular}%",
  "   \t\\begin{tablenotes}[flushleft] \\item \\small Notes: This table shows difference-in-differences estimates for closure (columns (1) and (2)) and relocation (columns (3) and (4)). Relocation is defined as \\textit{Census Tract, t-1}: $reloc\\_tract\\_tminus1(t)=1$ if the establishment changes census tract between $t$ and $t+1$ (constructed from \\texttt{code\\_tract} using a lead of the tract-change indicator). Additionally, for each establishment the first observed year is forced to have $reloc\\_tract\\_tminus1=0$ when it would otherwise be 1. Panel A uses a single post-treatment dummy; Panel B uses time-varying treatment dummies. The treatment radius ranges from 0 to 12.5 km to flood spots and the control ring is between 50--80 km. Establishment and year fixed effects are included in all estimations. The two-way clustered-robust standard errors at the establishment and year level are in parentheses. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1.",
  "   \t\\end{tablenotes}",
  "   \t\\end{threeparttable}",
  "   \t}",
  "\\end{table}%"
)

# ---------------------------------------------------------
# 8) Save LaTeX
# ---------------------------------------------------------
writeLines(lines, "./results/analysis/Tab_03_Effect_Spatial_Distribution.tex")
