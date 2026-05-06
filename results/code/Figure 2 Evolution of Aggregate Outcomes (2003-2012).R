# ============================================================================
# Figure 2: Evolution of Aggregate Outcomes (2003-2012) - 4 panels
#   Relocation is measured at the Census Tract, t-1 level (via code_tract)
#   Panel B labels are aligned with the plotting order
# Output: ./results/analysis/graph_descritivo.png
# ============================================================================

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(haven)

# ---------------------------------------------------------
# 0) Ensure the output directory exists
# ---------------------------------------------------------
dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1) Load the full panel before constructing t-1 measures
# ============================================================================
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%   # Keep later years needed to construct lead()
  arrange(id_estab, year)

# ============================================================================
# 2) Build treatment, closure, and municipal relocation variables
#    before aggregation
# ============================================================================
data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    # Main treatment definition
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    # Preserve original variables for reference
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # Set closure and entry outcomes to missing outside the analysis bands
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
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
  # Set mover_ano_mun to missing outside the analysis bands
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# Apply the same restriction to alternative relocation measures if they exist
if ("mover_ano_tract" %in% names(data)) {
  data <- data %>% mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}
if ("mover_ano_cep" %in% names(data)) {
  data <- data %>% mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# ============================================================================
# 3) Construct Census Tract relocation at t-1 from code_tract
# ============================================================================
if (!("code_tract" %in% names(data))) {
  stop("Variable 'code_tract' is not available in the dataset, so Census Tract relocation cannot be constructed.")
}
if (all(is.na(data$code_tract))) {
  stop("Variable 'code_tract' is entirely missing in the dataset. Check the .dta file and variable name.")
}

to_chr_id <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (is.character(x)) return(trimws(x))
  if (is.numeric(x))   return(ifelse(is.na(x), NA_character_, sprintf("%.0f", x)))
  trimws(as.character(x))
}

data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct     = to_chr_id(code_tract),
    ct_lag = lag(ct, 1),
    diff_tract = case_when(
      is.na(ct) | is.na(ct_lag) ~ NA_real_,
      ct != ct_lag              ~ 1,
      TRUE                      ~ 0
    ),
    reloc_tract_tminus1 = lead(diff_tract, 1)  # Remains NA when the panel has no t+1 observation
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1)
  ) %>%
  select(-ct, -ct_lag, -diff_tract)

# ============================================================================
# 4) Aggregate by treat_B and year and compute proportions
# ============================================================================
base_agregado <- data %>%
  mutate(
    firm                 = if_else(morte == 0, 1, NA_real_),
    massa_salarial_medio = massa_salarial
  ) %>%
  group_by(treat_B, year) %>%
  summarise(
    new_firm             = sum(new_firm,             na.rm = TRUE),
    firm                 = sum(firm,                 na.rm = TRUE),
    morte                = sum(morte,                na.rm = TRUE),
    reloc_tract_tminus1  = sum(reloc_tract_tminus1,  na.rm = TRUE),
    empregados           = sum(empregados,           na.rm = TRUE),
    massa_salarial       = sum(massa_salarial,       na.rm = TRUE),
    massa_salarial_medio = mean(massa_salarial_medio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(between(year, 2003, 2012)) %>%
  mutate(
    prop_morte = morte / firm,
    prop_new   = new_firm / firm,
    prop_mov   = reloc_tract_tminus1 / firm
  ) %>%
  mutate(
    morte               = prop_morte * 100,          # Closure rate (%)
    reloc_tract_tminus1 = prop_mov   * 100,          # Relocation rate (%)
    empregados          = empregados / firm,         # Mean employees
    massa_salarial      = massa_salarial / firm      # Mean payroll
  ) %>%
  pivot_longer(
    cols = c(morte, reloc_tract_tminus1, empregados, massa_salarial),
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    treat = case_when(
      treat_B == 1 ~ "Treat",
      treat_B == 0 ~ "Control",
      TRUE         ~ "Others"
    ),
    # Keep the relocation label aligned with vars_ordem
    variable = case_when(
      variable == "morte"               ~ "A - Closure Rate (%)",
      variable == "reloc_tract_tminus1" ~ "B - Relocation Rate (%)",
      variable == "empregados"          ~ "C - Mean number of Employees",
      variable == "massa_salarial"      ~ "D - Mean Payroll (in BRL)",
      TRUE                              ~ NA_character_
    )
  ) %>%
  filter(treat %in% c("Treat", "Control"), !is.na(variable)) %>%
  arrange(variable, treat, year)

# Use a fixed panel order to match the labels defined above
vars_ordem <- c(
  "A - Closure Rate (%)",
  "B - Relocation Rate (%)",
  "C - Mean number of Employees",
  "D - Mean Payroll (in BRL)"
)

# Stop here if any panel is missing to avoid saving an incomplete figure
panels_missing <- setdiff(vars_ordem, unique(base_agregado$variable))
if (length(panels_missing) > 0) {
  stop("Panels with no data after label matching: ", paste(panels_missing, collapse = " | "))
}

# ============================================================================
# 5) Plot function
# ============================================================================
create_plot <- function(df, var_name) {
  df %>%
    filter(variable == var_name) %>%
    ggplot() +
    geom_rect(
      aes(xmin = 2007 - 0.0001, xmax = 2007 + 0.0001,
          ymin = -Inf, ymax = Inf),
      fill = "grey60", alpha = 0.25
    ) +
    geom_vline(xintercept = 2007 - 0.0001, linetype = "dashed", color = "firebrick") +
    geom_vline(xintercept = 2007 + 0.0001, linetype = "dashed", color = "firebrick") +
    geom_line(aes(x = year, y = valor, colour = treat)) +
    geom_point(aes(x = year, y = valor, colour = treat), shape = 23, fill = "white") +
    scale_color_manual(
      values = c("Control" = "coral3", "Treat" = "cornflowerblue"),
      breaks = c("Control", "Treat"),
      name   = ""
    ) +
    scale_x_continuous(breaks = 2003:2012, limits = c(2003, 2012)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = var_name,
      x     = "Year",
      y     = "Value"
    )
}

plots <- lapply(vars_ordem, function(v) create_plot(base_agregado, v))

# ============================================================================
# 6) Combine the panels in a 2x2 layout and save
# ============================================================================
combined_plot <- ggpubr::ggarrange(
  plotlist      = plots,
  nrow          = 2,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)

ggsave(
  filename = "./results/analysis/graph_descritivo.png",
  plot     = combined_plot,
  dpi      = 300,
  width    = 14,
  height   = 9,
  units    = "in"
)
