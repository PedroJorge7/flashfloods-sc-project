# ============================================================================
# Figure B.2: Alternative Sizes of the Treatment Radius in Establishment-Level Estimates
#   Relocation is defined as reloc_tract_tminus1 (Census Tract, t-1 via code_tract)
#   reloc_tract_tminus1 is constructed before restricting the sample to 2003-2012
# ============================================================================

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
# Helper: code_tract must be numeric for varying slopes in fixest
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  
  # Fall back to factor encoding when too many values fail numeric parsing
  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) {
    x_num <- as.numeric(factor(x_chr))
  }
  x_num
}

# ---------------------------------------------------------
# 1) Load data without restricting the sample yet
# ---------------------------------------------------------
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  arrange(id_estab, year)

stopifnot(all(c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract") %in% names(data)))

# ---------------------------------------------------------
# 1.1) Construct reloc_tract_tminus1 before restricting the sample:
#      reloc_tract_tminus1(t) = 1 if the establishment moves between t and t+1
#      and the first observed year is recoded from 1 to 0
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
    
    reloc_tract_tminus1 = lead(diff_tract, 1)  # Remains NA in the last observed year for each establishment
  ) %>%
  ungroup() %>%
  select(-ct, -ct_lag, -diff_tract) %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    ),
  ) %>%
  ungroup() |> 
  mutate(reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
         reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1))

# ---------------------------------------------------------
# 2) Restrict the sample to 2003-2012
# ---------------------------------------------------------
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 3) Build the main treatment variables
# ---------------------------------------------------------
has_new_firm <- "new_firm" %in% names(data)

data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    # Main treatment definition: 0-12.5 km vs 50-80 km
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    # Preserve original variables for reference
    morte_orig              = morte,
    mover_ano_mun_orig      = mover_ano_mun,
    reloc_tract_tminus1_orig = reloc_tract_tminus1,
    new_firm_orig           = if (has_new_firm) new_firm else NA_real_
  ) %>%
  # Set outcomes to missing outside the analysis bands
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if (has_new_firm) if_else(is.na(treat_B), NA_real_, new_firm) else NA_real_
  ) %>%
  # Carry treatment status forward using the original municipal relocation measure
  mutate(
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
  # Set Census Tract relocation to missing wherever treat_B remains undefined
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    # Align relocation with closure by setting relocation to NA when closure is NA
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1),
    # Keep mover_ano_mun aligned with the final treatment definition
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# Post-shock trend indicator for tract-specific varying slopes
data <- data %>%
  mutate(
    treat_trend    = if_else(year >= 2008, 1, 0),
    code_tract_num = make_tract_num(code_tract)
  )

# ---------------------------------------------------------
# 4) Alternative treatment-radius specifications
# ---------------------------------------------------------
radius_specs <- list(
  "0-2.5 km"  = 2.5,
  "0-7.5 km"  = 7.5,
  "0-17.5 km" = 17.5,
  "0-22.5 km" = 22.5
)

# Two outcomes are estimated: closure and Census Tract relocation at t-1
outcomes <- c("morte", "reloc_tract_tminus1")

all_results <- data.frame()

# ---------------------------------------------------------
# 5) Loop over treatment radii and outcomes for the post-treatment and annual specifications
# ---------------------------------------------------------
for (rad_lab in names(radius_specs)) {
  
  radius <- radius_specs[[rad_lab]]
  
  temp_data <- data %>%
    mutate(
      # Alternative treatment definition: 0-radius vs 50-80 km
      treat_B_temp = case_when(
        dist_flood <= radius ~ 1,
        dist_flood >= 50 & dist_flood <= 80 ~ 0,
        TRUE ~ NA_real_
      ),
      # Always restart from the original variables
      morte_temp               = morte_orig,
      mover_ano_mun_temp       = mover_ano_mun_orig,         # Used only to carry treatment forward
      reloc_tract_tminus1_temp = reloc_tract_tminus1_orig
    ) %>%
    group_by(id_estab) %>%
    mutate(
      # 1) Set outcomes to missing where treat_B_temp is undefined
      morte_temp               = if_else(is.na(treat_B_temp), NA_real_, morte_temp),
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1_temp),
      
      # 2) Carry treat_B_temp forward using the original municipal relocation measure
      treat_B_temp = {
        tb  <- treat_B_temp
        mov <- mover_ano_mun_temp
        for (i in seq_along(tb)) {
          if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
            tb[i] <- tb[i - 1]
          }
        }
        tb
      },
      
      # 3) Reapply missing values to relocation wherever treat_B_temp remains undefined
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1_temp),
      
      #    and align relocation with closure when closure is NA
      reloc_tract_tminus1_temp = if_else(is.na(morte_temp), NA_real_, reloc_tract_tminus1_temp)
    ) %>%
    ungroup()
  
  # Annual dummies for 2008-2012 and the aggregated post-treatment indicator
  for (y in 2008:2012) {
    var <- paste0("treat_B_temp_", y)
    temp_data[[var]] <- dplyr::case_when(
      temp_data$year == y & !is.na(temp_data$treat_B_temp) ~ temp_data$treat_B_temp,
      !is.na(temp_data$treat_B_temp) & temp_data$year != y  ~ 0,
      TRUE                                                  ~ NA_real_
    )
  }
  
  temp_data <- temp_data %>%
    mutate(
      treat_B_agg_temp = case_when(
        year >= 2008 & treat_B_temp == 1 ~ 1,
        !is.na(treat_B_temp)             ~ 0,
        TRUE                             ~ NA_real_
      )
    )
  
  # Estimate the regressions by outcome
  for (outcome in outcomes) {
    
    yvar <- paste0(outcome, "_temp")
    
    # (1) Aggregated post-treatment model
    fml_agg <- as.formula(
      paste0(yvar, " ~ treat_B_agg_temp | id_estab + year + treat_trend[code_tract_num]")
    )
    m_agg <- feols(
      fml_agg,
      data    = temp_data,
      cluster = ~ id_estab,
      lean    = TRUE
    )
    agg_row <- broom::tidy(m_agg) %>%
      dplyr::filter(term == "treat_B_agg_temp") %>%
      transmute(
        Treatment_Radius = rad_lab,
        Outcome          = outcome,
        Period           = "Post",
        Coefficient      = estimate,
        SE               = std.error,
        CI_Low           = estimate - 1.96 * std.error,
        CI_High          = estimate + 1.96 * std.error
      )
    
    # (2) Model with year-specific dummies for 2008-2012
    treat_vars <- paste0("treat_B_temp_", 2008:2012)
    fml_evt <- as.formula(
      paste0(yvar, " ~ ", paste(treat_vars, collapse = " + "),
             " | id_estab + year + treat_trend[code_tract_num]")
    )
    m_evt <- feols(
      fml_evt,
      data    = temp_data,
      cluster = ~ id_estab + year,
      lean    = TRUE
    )
    evt_rows <- broom::tidy(m_evt) %>%
      dplyr::filter(grepl("^treat_B_temp_", term)) %>%
      mutate(Period = gsub("treat_B_temp_", "", term)) %>%
      transmute(
        Treatment_Radius = rad_lab,
        Outcome          = outcome,
        Period           = Period,
        Coefficient      = estimate,
        SE               = std.error,
        CI_Low           = estimate - 1.96 * std.error,
        CI_High          = estimate + 1.96 * std.error
      )
    
    all_results <- bind_rows(all_results, agg_row, evt_rows)
  }
}

# ---------------------------------------------------------
# 6) Prepare the plotting data, palette, and Panels A/B
# ---------------------------------------------------------
all_results <- all_results %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c("morte", "reloc_tract_tminus1"),
      labels = c("Closure", "Relocation")
    ),
    Period  = factor(
      Period,
      levels = c("Post", "2008", "2009", "2010", "2011", "2012")
    ),
    Treatment_Radius = factor(
      Treatment_Radius,
      levels = c("0-2.5 km", "0-7.5 km", "0-17.5 km", "0-22.5 km")
    )
  )

rad_colors <- c(
  "0-2.5 km"  = "#FEE0D2",
  "0-7.5 km"  = "#FCBBA1",
  "0-17.5 km" = "#FB6A4A",
  "0-22.5 km" = "#CB181D"
)

pos_dodge <- position_dodge(width = 0.4)

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Treatment_Radius)) +
    geom_point(position = pos_dodge, size = 2) +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High),
                  position = pos_dodge, width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = rad_colors, name = "Treatment Radius") +
    labs(x = "Year", y = "Coefficient", title = title_label) +
    theme_bw()
}

pA <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Closure"),
  title_label = "A - Establishment Closure (0/1)"
)

pB <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Relocation"),
  title_label = "B - Establishment Relocation (0/1)"
)

# ---------------------------------------------------------
# 7) Build and save the final figure
# ---------------------------------------------------------
fig_rad <- ggpubr::ggarrange(
  pA, pB,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)

dir.create("results/analysis", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "results/analysis/change_treatment.png",
  plot     = fig_rad,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)
