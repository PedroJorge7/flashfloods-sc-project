############################################################
## Worker event study using output_empregados
## - Runs from the project-side processed data
## - Excludes migration outcomes
## - Plots with cowplot and saves a PNG
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(cowplot)
library(ggplot2)
library(MatchIt)
library(fixest)

# ---------------------------------------------------------
# Helper: use the same y-limits for Employment and Wage
# ---------------------------------------------------------
.get_ylim_gg <- function(p) {
  b <- ggplot_build(p)
  
  # Recent ggplot2 versions may return multiple panels
  if (!is.null(b$layout$panel_params) && length(b$layout$panel_params) > 0) {
    yr <- unlist(lapply(b$layout$panel_params, function(pp) pp$y.range), use.names = FALSE)
    yr <- yr[is.finite(yr)]
    if (length(yr) >= 2) return(range(yr))
  }
  
  # Fallback for older ggplot2 behavior
  if (!is.null(b$layout$panel_scales_y) && length(b$layout$panel_scales_y) > 0) {
    yr <- unlist(lapply(b$layout$panel_scales_y, function(s) s$range$range), use.names = FALSE)
    yr <- yr[is.finite(yr)]
    if (length(yr) >= 2) return(range(yr))
  }
  
  c(NA_real_, NA_real_)
}

.force_common_ylim_emp_wage <- function(plots, reg_names, pad = 0.03) {
  idx_emp  <- which(grepl("emp",  reg_names, ignore.case = TRUE))[1]
  idx_wage <- which(grepl("wage", reg_names, ignore.case = TRUE))[1]
  
  if (is.na(idx_emp) || is.na(idx_wage)) return(plots)
  if (!inherits(plots[[idx_emp]],  "ggplot")) return(plots)
  if (!inherits(plots[[idx_wage]], "ggplot")) return(plots)
  
  yl_emp  <- .get_ylim_gg(plots[[idx_emp]])
  yl_wage <- .get_ylim_gg(plots[[idx_wage]])
  
  yl <- range(c(yl_emp, yl_wage), finite = TRUE)
  if (length(yl) != 2 || any(!is.finite(yl))) return(plots)
  
  # Small margin to avoid clipping the confidence intervals
  span <- diff(yl)
  if (is.finite(span) && span > 0) {
    yl <- yl + c(-1, 1) * pad * span
  }
  
  plots[[idx_emp]]  <- plots[[idx_emp]]  + coord_cartesian(ylim = yl)
  plots[[idx_wage]] <- plots[[idx_wage]] + coord_cartesian(ylim = yl)
  
  plots
}

# ---------------------------------------------------------
# 1) Load helper functions
# ---------------------------------------------------------
source("./results/code/read_functions.R")

# ---------------------------------------------------------
# 2) Load data and apply the worker sample filters
# ---------------------------------------------------------
dados <- readRDS(data_path("workers_clean_data.rds")) %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# ---------------------------------------------------------
# 3) Run the worker regressions
# ---------------------------------------------------------
output_trend <- output_empregados(
  dados, 0, 12.5, 50, 80,
  trend = TRUE
)

# ---------------------------------------------------------
# 4) Keep only the displayed worker outcomes
# ---------------------------------------------------------
if ("Regression" %in% names(output_trend)) {
  output_trend <- output_trend %>%
    filter(!grepl("migration", Regression, ignore.case = TRUE))
} else {
  stop("output_trend does not contain the `Regression` column. Check the return value of output_empregados().")
}

# ---------------------------------------------------------
# 5) Build the plots
# ---------------------------------------------------------
reg_names <- unique(output_trend$Regression)

keep <- reg_names[grepl("emp", reg_names, ignore.case = TRUE) |
                    grepl("wage", reg_names, ignore.case = TRUE)]
if (length(keep) >= 2) reg_names <- keep

plots <- lapply(reg_names, function(rg) event_study_plot(rg, data = output_trend))

# Use the same y-limits across both panels
plots <- .force_common_ylim_emp_wage(plots, reg_names, pad = 0.03)

fig <- cowplot::plot_grid(plotlist = plots, nrow = 1)

# ---------------------------------------------------------
# 6) Save the figure
# ---------------------------------------------------------
ggsave(
  filename = "./results/analysis/event_study_empregados.png",
  plot     = fig,
  dpi      = 500,
  width    = 30,
  height   = 15,
  units    = "cm"
)
