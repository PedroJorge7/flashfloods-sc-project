############################################################
## Figure F.1 - Alternative Sizes of the Control Rings in Worker-Level Estimations
## Control rings displayed in a fixed order:
##   40-70, 50-80, 30-80, 50-100 with a fixed 0-12.5 treatment radius
## Output: ./results/analysis/results_controles_empregados.png
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(ggplot2)
library(ggpubr)
library(MatchIt)
library(fixest)

OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

source("./results/code/read_functions.R")

# ---------------------------------------------------------
# Control-ring definitions
# ---------------------------------------------------------
control_specs <- list(
  "40-70"  = c(40, 70),
  "30-80"  = c(30, 80),
  "50-100" = c(50, 100)
)

# Plot the control rings in a fixed order
radius_levels <- c("40-70 km",  "30-80 km", "50-100 km")

# Fixed palette
control_colors <- c(
  "40-70 km"  = "#FCBBA1",
  "30-80 km"  = "#FB6A4A",
  "50-100 km" = "#CB181D"
)

# ---------------------------------------------------------
# Load data and apply filters
# ---------------------------------------------------------
dados <- readRDS(data_path("workers_clean_data.rds")) %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

if (nrow(dados) == 0) stop("Workers: `dados` is empty after filtering.")

# ---------------------------------------------------------
# Helper: enforce numeric types to keep the plot stable
# ---------------------------------------------------------
clean_output_for_plot <- function(df) {
  df %>%
    mutate(
      parmseq  = trimws(as.character(parmseq)),
      estimate = as.numeric(estimate),
      min      = as.numeric(min),
      max      = as.numeric(max),
      radius   = trimws(as.character(radius))
    ) %>%
    filter(is.finite(estimate), is.finite(min), is.finite(max))
}

# ---------------------------------------------------------
# Run the worker regressions for each control ring
# ---------------------------------------------------------
outputs_list <- lapply(names(control_specs), function(k) {
  lo <- control_specs[[k]][1]
  up <- control_specs[[k]][2]
  
  output_empregados(
    dados,
    0, 12.5,
    lo, up,
    trend = TRUE
  )
})

output_raw <- bind_rows(outputs_list)
if (nrow(output_raw) == 0) stop("Workers: `output_raw` is empty.")

# ---------------------------------------------------------
# Filter, build the radius labels, and order the results
# ---------------------------------------------------------
output <- output_raw %>%
  filter(type == "type_treatment") %>%
  mutate(
    parmseq = gsub("^Flash Flood\\s+", "", as.character(parmseq)),
    
    # Build labels such as "40-70 km"
    control_chr = gsub("\\s+", "", as.character(control)),
    radius      = paste0(control_chr, " km"),
    
    # Enforce the fixed ordering in the plot
    radius      = factor(radius, levels = radius_levels)
  ) %>%
  clean_output_for_plot() %>%
  arrange(parmseq, radius)

if (nrow(output) == 0) stop("Workers: `output` is empty after filtering and type conversion.")

# ---------------------------------------------------------
# Plot with fixed ordering and colors via breaks and limits
# ---------------------------------------------------------
plot_one <- function(reg) {
  dd <- output %>% filter(Regression == reg)
  
  ggplot(dd, aes(y = parmseq, x = estimate, color = radius)) +
    geom_pointrange(
      aes(xmax = max, xmin = min),
      size = 0.5,
      position = position_dodge(width = 0.7)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_discrete(limits = c("Post","2008","2009","2010","2011","2012")) +
    scale_color_manual(
      values = control_colors,
      breaks = radius_levels,   # Keep the legend order fixed
      limits = radius_levels,   # Keep the plotting order fixed even when levels are missing
      drop   = FALSE
    ) +
    coord_flip() +
    labs(x = "Coefficient", y = "Year", title = reg, color = "Control Radius") +
    theme_bw() +
    theme(legend.position = "bottom")
}

reg_names <- unique(output$Regression)

# Focus on employment and wage when available; otherwise plot all available panels
keep <- reg_names[grepl("Employment|Wage|emp|wage", reg_names, ignore.case = TRUE)]
if (length(keep) > 0) reg_names <- keep

plots_list <- lapply(reg_names, plot_one)
if (length(plots_list) == 0) stop("Workers: no panels are available to plot.")

# ---------------------------------------------------------
# Arrange the panels in a single row
# ---------------------------------------------------------
nplots <- length(plots_list)
fig <- ggpubr::ggarrange(
  plotlist      = plots_list,
  nrow          = 1,
  ncol          = nplots,
  common.legend = TRUE,
  legend        = "bottom"
)

print(fig)

ggsave(
  filename = file.path(OUT_DIR, "results_controles_empregados.png"),
  plot     = fig,
  dpi      = 500,
  width    = 30,
  height   = 15,
  units    = "cm"
)
