############################################################
## Figure 4 - Event Study: Effect of the Flash Floods
##             on the Spatial Distribution of Establishments
##
## Relocation = Census Tract, t-1 (via code_tract)
## - Defines treatment with the 0-12.5 km exposure radius and the 50-80 km control ring
## - Defines reloc_tract_tminus1 as 1 in year t when the establishment moves between t and t+1
##   using the lead of the contemporaneous relocation indicator, with no default fill
## - Uses fixed effects: id_estab + year + treat_trend_f[code_tract_num]
## - Sets the y-axis from the confidence interval bounds with a small margin
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(ggpubr)
library(broom)

# ---------------------------------------------------------
# 0) Ensure the output directories exist
# ---------------------------------------------------------
dir.create("./establishments/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load the full panel before constructing t-1 measures
# ---------------------------------------------------------
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

stopifnot(all(c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract") %in% names(data)))

# ---------------------------------------------------------
# 2) Build the treatment variables
# ---------------------------------------------------------
data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = dplyr::case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    # Coerce to numeric to avoid haven_labelled issues
    morte        = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    
    # Set outcomes to missing outside the analysis bands
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  mutate(
    # Carry treatment status forward only for establishments that relocate across municipalities
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun  # Use the original municipal relocation indicator here
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  mutate(
    # Set mover_ano_mun to missing outside the analysis bands
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# Apply the same restriction to these variables if they are available
if ("mover_ano_tract" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}
if ("mover_ano_cep" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# Post-shock trend indicator (year >= 2008)
data <- data %>%
  mutate(
    treat_trend   = if_else(year >= 2008, 1, 0),
    treat_trend_f = factor(treat_trend)
  )

# ---------------------------------------------------------
# 3) Construct reloc_tract_tminus1 from code_tract (Census Tract, t-1)
#     - diff_tract(t) = 1 if ct(t) != ct(t-1), and NA when it cannot be defined
#     - reloc_tract_tminus1(t) = diff_tract(t+1), capturing moves between t and t+1
#       with no default fill, so the last year remains NA
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct     = as.character(code_tract),
    ct_lag = dplyr::lag(ct, 1),
    
    diff_tract = dplyr::case_when(
      is.na(ct) | is.na(ct_lag) ~ NA_real_,
      ct != ct_lag              ~ 1,
      TRUE                      ~ 0
    ),
    
    reloc_tract_tminus1 = dplyr::lead(diff_tract, 1)  # Remains NA in the last observed year for each establishment
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    reloc_tract_tminus1 = as.numeric(reloc_tract_tminus1)
  ) %>%
  select(-ct, -ct_lag, -diff_tract)

# ---------------------------------------------------------
# 4) Restrict to the 2003-2012 sample with defined treat_B
# ---------------------------------------------------------
data_es <- data %>%
  group_by(id_estab) |> 
  mutate(reloc_tract_tminus1 = ifelse(year == min(year, na.rm = T) & reloc_tract_tminus1 == 1,0,reloc_tract_tminus1)) |> 
  filter(year >= 2003 & year <= 2012, !is.na(treat_B))

# Optionally align the relocation sample with closure by setting relocation to NA when closure is NA
data_es <- data_es %>%
  mutate(reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
         reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1))

# ---------------------------------------------------------
# 4.1) Prepare code_tract_num for varying slopes in fixest
# ---------------------------------------------------------
data_es <- data_es %>%
  mutate(code_tract_num = suppressWarnings(as.numeric(as.character(code_tract))))

# ---------------------------------------------------------
# 5) Year-specific treatment dummies (baseline = 2007)
# ---------------------------------------------------------
for (y in 2003:2012) {
  var <- paste0("treat_B_", y)
  data_es[[var]] <- dplyr::case_when(
    data_es$year == y & !is.na(data_es$treat_B) ~ data_es$treat_B,
    !is.na(data_es$treat_B) & data_es$year != y ~ 0,
    TRUE                                        ~ NA_real_
  )
}

treat_years <- c(2003:2006, 2008:2012)

# ---------------------------------------------------------
# 6) Estimate the event-study models for closure and Census Tract relocation
# ---------------------------------------------------------
outcomes <- c("morte", "reloc_tract_tminus1")
outcome_labels <- c(
  "morte"               = "A - Establishment Closure (0/1)",
  "reloc_tract_tminus1" = "B - Establishment Relocation (0/1)"
)

all_coefs <- data.frame()

for (outcome in outcomes) {
  
  lab <- outcome_labels[[outcome]]
  yv  <- data_es[[outcome]]
  y_non_na <- yv[!is.na(yv)]
  
  if (length(y_non_na) < 2 || length(unique(y_non_na)) <= 1) {
    message("Outcome '", outcome, "' is constant or nearly constant; skipping.")
    next
  }
  
  treat_vars_str <- paste0("treat_B_", treat_years, collapse = " + ")
  formula_str    <- paste(outcome, "~", treat_vars_str)
  
  # Fixed effects: id_estab + year + treat_trend_f with varying slopes in code_tract_num
  fml <- as.formula(paste0(
    formula_str,
    " | id_estab + year + treat_trend_f[code_tract_num]"
  ))
  
  model <- feols(
    fml,
    data    = data_es,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  
  coef_summary <- broom::tidy(model, conf.int = TRUE)
  
  event_coefs <- coef_summary %>%
    filter(grepl("^treat_B_", term)) %>%
    mutate(
      year       = as.numeric(gsub("treat_B_", "", term)),
      parmseq    = year - 2007,
      Regression = lab,
      IC         = "95%"
    ) %>%
    select(Regression, parmseq, estimate,
           min = conf.low, max = conf.high, IC)
  
  baseline_row <- data.frame(
    Regression = lab,
    parmseq    = 0,
    estimate   = 0,
    min        = 0,
    max        = 0,
    IC         = "95%",
    stringsAsFactors = FALSE
  )
  
  event_coefs <- bind_rows(event_coefs, baseline_row)
  all_coefs   <- bind_rows(all_coefs, event_coefs)
}

if (nrow(all_coefs) == 0) {
  stop("No outcome has enough variation to estimate the event study.")
}

output <- all_coefs %>% arrange(Regression, parmseq)

# ---------------------------------------------------------
# 7) Set y-axis limits from the confidence interval bounds
# ---------------------------------------------------------
y_min   <- min(output$min, na.rm = TRUE)
y_max   <- max(output$max, na.rm = TRUE)
y_range <- y_max - y_min

pad <- if (is.finite(y_range) && y_range > 0) 0.05 * y_range else 0
y_limits <- c(y_min - pad, y_max + pad)

# ---------------------------------------------------------
# 8) Plot with dynamic y-axis limits
# ---------------------------------------------------------
event_study_plot <- function(reg_name) {
  output %>%
    filter(Regression == reg_name) %>%
    ggplot(aes(x = parmseq, y = estimate)) +
    geom_ribbon(aes(ymax = max, ymin = min), fill = "grey90", alpha = 0.5) +
    geom_line(aes(parmseq, max), color = "grey30", size = 0.1) +
    geom_line(aes(parmseq, min), color = "grey30", size = 0.1) +
    geom_point(size = 2, color = "firebrick", fill = "black") +
    geom_line(color = "firebrick", size = 1) +
    scale_x_continuous(breaks = -4:5, labels = 2003:2012) +
    scale_y_continuous(limits = y_limits) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(
      x = "Years",
      y = "Coefficient",
      title = reg_name
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

reg_names  <- unique(output$Regression)
plots_list <- lapply(reg_names, event_study_plot)

fig_event <- ggpubr::ggarrange(
  plotlist      = plots_list,
  nrow          = 1,
  ncol          = length(plots_list),
  common.legend = FALSE
)

ggsave(
  filename = "./results/analysis/event_study.png",
  plot     = fig_event,
  dpi      = 300,
  width    = 30 * length(plots_list) / 2,
  height   = 15,
  units    = "cm"
)
