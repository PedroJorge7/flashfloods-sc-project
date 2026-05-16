############################################################
## Auxiliary establishment robustness check:
## Removing control firms in municipalities declaring public calamity (ECP)
## - Uses the baseline establishment difference-in-differences design:
##   * Outcomes: morte (closure) and municipal relocation
##   * Aggregated DiD (treat_B_agg) plus annual DiD dummies (2008-2012)
##   * FE: id_estab + year
##   * Census tract trend: i.treat_trend#c.code_tract  ->  treat_trend[code_tract_num]
##   * Cluster 2-way: id_estab + year
##
## - This robustness check:
##   * Removes control-group observations (treat_B == 0) when the establishment
##     is located in a municipality that declared public calamity (ECP).
##     This is equivalent to excluding observations with ecp == 1 and treat_B == 0.
##
## - Municipal relocation definition: mover_ano_mun
##
## - Output: ./results/analysis/Aux_Removing_Control_Firms_in_ECP_Municipalities.tex
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(haven)
library(fixest)
library(broom)

# ---------------------------------------------------------
# 0) Output dir
# ---------------------------------------------------------
dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load data and prepare the establishment panel
# ---------------------------------------------------------
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

req <- c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract","mun")
miss <- setdiff(req, names(data))
if (length(miss) > 0) stop(paste("Missing required columns in the dataset:", paste(miss, collapse = ", ")))

# ---------------------------------------------------------
# 2) Build treatment variables with forward carry for municipal relocations
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    morte         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    # Set outcomes to missing outside the treatment and control bands
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  # Carry treatment status forward only for establishments that relocate across municipalities
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
  # Set mover_ano_mun to missing outside the treatment and control bands
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.1) ECP filter:
#      remove observations only when ecp == 1 and treat_B == 0
#      observations with treat_B == NA remain in the sample
# ---------------------------------------------------------
ecp_muns <- c(
  420220, 420240, 420290, 420320, 420590, 420710, 420820,
  420845, 421150, 421320, 421470, 421510, 421820
)

to_num_safe <- function(x) {
  x_chr <- as.character(haven::zap_labels(x))
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
    mun_num = to_num_safe(mun),
    ecp     = as.integer(mun_num %in% ecp_muns),
    remove_ecp = (ecp == 1 & !is.na(treat_B) & treat_B == 0)
  )

# ---------------------------------------------------------
# 3) Create the aggregated post variable, annual dummies, and trend control
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

for (y in 2008:2012) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                   ~ NA_real_
  )
}

# ---------------------------------------------------------
# 4) Restrict to the 2003-2012 table window and apply the ECP filter
# ---------------------------------------------------------
data_tab <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  filter(!remove_ecp)

# ---------------------------------------------------------
# 5) Estimate the models for:
#    morte (closure) and mover_ano_mun (original relocation definition)
# ---------------------------------------------------------
outcomes <- c("morte", "mover_ano_mun")

panelA <- list()
panelB <- list()

for (outcome in outcomes) {
  
  # Panel A (aggregated)
  fml_A1 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year"))
  fml_A2 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num]"))
  
  mA1 <- feols(fml_A1, data = data_tab, cluster = ~ id_estab + year, lean = TRUE)
  mA2 <- feols(fml_A2, data = data_tab, cluster = ~ id_estab + year, lean = TRUE)
  
  panelA[[outcome]] <- list(no_trend = mA1, trend = mA2)
  
  # Panel B (time-varying dummies)
  treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")
  fml_B1 <- as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year"))
  fml_B2 <- as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]"))
  
  mB1 <- feols(fml_B1, data = data_tab, cluster = ~ id_estab + year, lean = TRUE)
  mB2 <- feols(fml_B2, data = data_tab, cluster = ~ id_estab + year, lean = TRUE)
  
  panelB[[outcome]] <- list(no_trend = mB1, trend = mB2)
}

# Column order: (1) Closure, (2) Closure+Trend, (3) Original Relocation, (4) Original Relocation+Trend
mods_A <- list(
  panelA[["morte"]]$no_trend,
  panelA[["morte"]]$trend,
  panelA[["mover_ano_mun"]]$no_trend,
  panelA[["mover_ano_mun"]]$trend
)

mods_B <- list(
  panelB[["morte"]]$no_trend,
  panelB[["morte"]]$trend,
  panelB[["mover_ano_mun"]]$no_trend,
  panelB[["mover_ano_mun"]]$trend
)

# ---------------------------------------------------------
# 6) Helper functions
# ---------------------------------------------------------
get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate", "std.error", "p.value")]
  if (nrow(out) == 0) stop(paste("Term not found:", term))
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
# 7) Build LaTeX
# ---------------------------------------------------------
lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{Removing Control Firms in Municipalities Declaring Public Calamity (ECP)}",
  "  \\label{tab: removing_ecp}",
  "  \\scalebox{0.75}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccc}",
  "    \\toprule",
  "          & (1)   &       & (2)   &       & (3)   &       & (4) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  "    \\multicolumn{8}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  "    Dep. Var: & Closure & & Closure & & Municipal Relocation & & Municipal Relocation \\\\",
  "   \\midrule"
)

# Panel A: treat_B_agg
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
           "    Dep. Var: & Closure & & Closure & & Municipal Relocation & & Municipal Relocation \\\\",
           "    \\midrule"
)

# Panel B: 2008-2012
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

n_removed <- sum((data$ecp == 1 & !is.na(data$treat_B) & data$treat_B == 0) &
                   (data$year >= 2003 & data$year <= 2012), na.rm = TRUE)

lines <- c(
  lines,
  row_gap4("    Observations", sapply(obsB, fmt_obs)),
  row_gap4("    Census Tract Trend", c("No","Yes","No","Yes")),
  "    \\bottomrule",
  "    \\end{tabular}%",
  "   \t\\begin{tablenotes}[flushleft] \\item \\small Notes: This table excludes observations in which control firms (treat\\_B=0) are located in municipalities that declared public calamity (ECP). The excluded municipalities are identified by the variable \\texttt{mun}. The filter removes observations with \\texttt{ecp==1} and \\texttt{treat\\_B==0}. Municipal relocation corresponds to \\texttt{mover\\_ano\\_mun}. Panel A uses a single post-treatment dummy; Panel B uses time-varying treatment dummies (2008--2012). The treatment radius is 0--12.5 km and the control ring is 50--80 km. Establishment and year fixed effects are included in all estimations. Two-way clustered standard errors (establishment and year) are shown in parentheses. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. ",
  paste0("   \t\\item \\small Control observations excluded due to ECP rule (within 2003--2012 window): ", n_removed, "."),
  "   \t\\end{tablenotes}",
  "   \t\\end{threeparttable}",
  "   \t}",
  "\\end{table}%"
)

# ---------------------------------------------------------
# 8) Save the LaTeX output
# ---------------------------------------------------------
outfile <- "./results/analysis/Aux_Removing_Control_Firms_in_ECP_Municipalities.tex"
writeLines(lines, outfile)
message("Saved: ", outfile)

