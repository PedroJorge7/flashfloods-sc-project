# ============================================================================
# Figure B.3: Establishment-Level Estimates Using Alternative Inference Procedures
# Relocation is defined as Census Tract, t-1 (reloc_tract_tminus1 via code_tract)
# - Uses lead() to align moves between t and t+1
# - Recode the first observed year from 1 to 0
# - Set relocation to NA when closure is NA
# - Uses the trend fixed effect treat_trend_f[code_tract_num]
# ============================================================================

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(broom)
library(ggpubr)

# ---------------------------------------------------------
# 0) Output dir
# ---------------------------------------------------------
dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load the dataset with coordinates
# ---------------------------------------------------------
data <- haven::read_dta(data_path("firm_coordinates.dta")) %>%
  mutate(
    lat = Lat,
    lon = Lon
  ) %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

stopifnot(all(c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract","lat","lon") %in% names(data)))

# Conley standard errors require coordinates, so drop observations with missing coordinates
data <- data %>%
  filter(!is.na(lat), !is.na(lon))

# ---------------------------------------------------------
# 2) Build the treatment variables and baseline outcomes
# ---------------------------------------------------------
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
    morte_orig         = morte,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    # Coerce to numeric to avoid haven_labelled issues
    morte         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    mover_raw     = mover_ano_mun,  # Used when carrying treat_B forward
    # Set outcomes to missing outside the analysis bands
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  # Carry treatment status forward using the original municipal relocation measure
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_raw
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # Set mover_ano_mun to missing wherever treat_B remains undefined
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.1) Construct the Census Tract relocation measure at t-1
#      reloc_tract_tminus1(t) = lead(1[ct(t) != ct(t-1)], 1)
#      Recode the first observed year from 1 to 0
#      and align relocation with closure by setting relocation to NA when closure is NA
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
    reloc_tract_tminus1 = dplyr::lead(diff_tract, 1)
  ) %>%
  ungroup() %>%
  select(-ct, -ct_lag, -diff_tract)

# Apply the treatment-based sample restriction, first-year adjustment, and closure-based alignment
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_tminus1))
  ) %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = ifelse(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0,
      reloc_tract_tminus1
    )
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1),
  )

# ---------------------------------------------------------
# 2.2) Trend fixed effect: treat_trend_f[code_tract_num]
# ---------------------------------------------------------
data <- data %>%
  mutate(
    treat_trend    = if_else(year >= 2008, 1, 0),
    treat_trend_f  = factor(treat_trend),
    code_tract_num = suppressWarnings(as.numeric(as.character(code_tract)))
  )

# ---------------------------------------------------------
# 2.3) Annual dummies for 2008-2012 plus the aggregated post-treatment indicator
# ---------------------------------------------------------
for (y in 2008:2012) {
  var <- paste0("treat_B_", y)
  data[[var]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    )
  )

# ---------------------------------------------------------
# 3) Inference specifications
# ---------------------------------------------------------
outcomes <- c("morte", "reloc_tract_tminus1")

inference_specs <- list(
  "Code tract"      = list(type = "cluster", cluster = ~ code_tract),
  "Conley: 10 km"   = list(type = "conley",  dist = 10),
  "Conley: 15 km"   = list(type = "conley",  dist = 15),
  "Conley: 20 km"   = list(type = "conley",  dist = 20)
)

# ---------------------------------------------------------
# 4) Estimate the post-treatment and annual models under each inference procedure
#    Fixed effects: id_estab + year + treat_trend_f[code_tract_num]
# ---------------------------------------------------------
all_results <- data.frame()

for (inf_name in names(inference_specs)) {
  
  spec <- inference_specs[[inf_name]]
  
  for (outcome in outcomes) {
    
    # ---------- (a) Aggregated post-treatment effect ----------
    fml_agg <- as.formula(
      paste0(outcome,
             " ~ treat_B_agg | id_estab + year + treat_trend_f[code_tract_num]")
    )
    
    if (spec$type == "cluster") {
      m_agg <- feols(
        fml_agg,
        data    = data,
        cluster = spec$cluster,
        lean    = TRUE
      )
    } else {
      m_agg <- feols(
        fml_agg,
        data = data,
        vcov = conley(spec$dist),
        lean = TRUE
      )
    }
    
    agg_row <- broom::tidy(m_agg) %>%
      dplyr::filter(term == "treat_B_agg") %>%
      transmute(
        Inference   = inf_name,
        Outcome     = outcome,
        Period      = "Post",
        Coefficient = estimate,
        SE          = std.error,
        CI_Low      = estimate - 1.96 * std.error,
        CI_High     = estimate + 1.96 * std.error
      )
    
    # ---------- (b) Year-specific dummies for 2008-2012 ----------
    treat_vars <- paste0("treat_B_", 2008:2012)
    fml_evt <- as.formula(
      paste0(outcome, " ~ ",
             paste(treat_vars, collapse = " + "),
             " | id_estab + year + treat_trend_f[code_tract_num]")
    )
    
    if (spec$type == "cluster") {
      m_evt <- feols(
        fml_evt,
        data    = data,
        cluster = spec$cluster,
        lean    = TRUE
      )
    } else {
      m_evt <- feols(
        fml_evt,
        data = data,
        vcov = conley(spec$dist),
        lean = TRUE
      )
    }
    
    evt_rows <- broom::tidy(m_evt) %>%
      dplyr::filter(grepl("^treat_B_", term)) %>%
      mutate(Period = gsub("treat_B_", "", term)) %>%
      transmute(
        Inference   = inf_name,
        Outcome     = outcome,
        Period      = Period,
        Coefficient = estimate,
        SE          = std.error,
        CI_Low      = estimate - 1.96 * std.error,
        CI_High     = estimate + 1.96 * std.error
      )
    
    all_results <- bind_rows(all_results, agg_row, evt_rows)
  }
}

# ---------------------------------------------------------
# 5) Prepare the plotting data for Panels A and B
# ---------------------------------------------------------
all_results <- all_results %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c("morte", "reloc_tract_tminus1"),
      labels = c("Closure", "Relocation (Census Tract, t-1)")
    ),
    Period = factor(
      Period,
      levels = c("Post", "2008", "2009", "2010", "2011", "2012")
    ),
    Inference = factor(
      Inference,
      levels = c("Code tract", "Conley: 10 km", "Conley: 15 km", "Conley: 20 km")
    )
  )

inf_colors <- c(
  "Code tract"      = "#FEE0D2",
  "Conley: 10 km"   = "#FCBBA1",
  "Conley: 15 km"   = "#FB6A4A",
  "Conley: 20 km"   = "#CB181D"
)

pos_dodge <- position_dodge(width = 0.4)

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Inference)) +
    geom_point(position = pos_dodge, size = 2) +
    geom_errorbar(
      aes(ymin = CI_Low, ymax = CI_High),
      position = pos_dodge,
      width = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = inf_colors, name = "Inference Procedure") +
    labs(
      x = "Year",
      y = "Coefficient",
      title = title_label
    ) +
    theme_bw()
}

pA <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Closure"),
  title_label = "A - Establishment Closure (0/1)"
)

pB <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Relocation (Census Tract, t-1)"),
  title_label = "B - Establishment Relocation"
)

# ---------------------------------------------------------
# 6) Build the final figure with a single legend at the bottom
# ---------------------------------------------------------
fig_inf <- ggpubr::ggarrange(
  pA, pB,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)

# ---------------------------------------------------------
# 7) Save the figure
# ---------------------------------------------------------
ggsave(
  filename = "./results/analysis/change_standard_deviation.png",
  plot     = fig_inf,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)
