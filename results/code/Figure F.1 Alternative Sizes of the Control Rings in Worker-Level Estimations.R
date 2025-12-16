############################################################
## Robustez 1 – Mudanças no Anel de Controle (Workers)
## Controles: 30-80 60-80 30-50 50-100 80-100 (treat fixo 0–12.5)
## Saída: ./results/analysis/results_controles_empregados.png
## AJUSTES:
##  - Cores padrão (paleta nomeada pelos níveis do radius)
##  - Painéis em apenas 1 linha (ggarrange nrow = 1)
############################################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggpubr)
library(MatchIt)

OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

source("./results/code/Aux.R")

# ---------------------------------------------------------
# Paleta padrão (NOMES = níveis do radius)
# ---------------------------------------------------------
control_colors <- c(
  "30-50 km"  = "#FEE0D2",
  "30-80 km"  = "#FCBBA1",
  "60-80 km"  = "#FB6A4A",
  "50-100 km" = "#EF3B2C",
  "80-100 km" = "#CB181D"
)

# ---------------------------------------------------------
# Dados + filtros
# ---------------------------------------------------------
dados <- readRDS("./data/workers_clean_data.rds") %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# ---------------------------------------------------------
# Helper: garante tipos numéricos (evita plot “sumir”)
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
# Rodar outputs por anel de controle (treat fixo 0–12.5)
# ---------------------------------------------------------
output_empregados_30_80  <- output_empregados(dados, 0, 12.5, 30, 80,  trend = TRUE)
output_empregados_60_80  <- output_empregados(dados, 0, 12.5, 60, 80,  trend = TRUE)
output_empregados_30_50  <- output_empregados(dados, 0, 12.5, 30, 50,  trend = TRUE)
output_empregados_50_100 <- output_empregados(dados, 0, 12.5, 50, 100, trend = TRUE)
output_empregados_80_100 <- output_empregados(dados, 0, 12.5, 80, 100, trend = TRUE)

output <- bind_rows(
  output_empregados_30_80,
  output_empregados_60_80,
  output_empregados_30_50,
  output_empregados_50_100,
  output_empregados_80_100
) %>%
  filter(type == "type_treatment") %>%
  mutate(
    parmseq = gsub("^Flash Flood\\s+", "", as.character(parmseq)),
    radius  = paste0(control, " km"),
    radius  = factor(radius, levels = c("30-50 km","30-80 km","60-80 km","50-100 km","80-100 km"))
  ) %>%
  clean_output_for_plot() %>%
  arrange(parmseq, radius)

if (nrow(output) == 0) stop("Robustez 1: `output` ficou vazio depois dos filtros/conversões.")

# ---------------------------------------------------------
# Plot (comparando controles no MESMO painel, com cores padrão)
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
    scale_color_manual(values = control_colors, drop = FALSE) +
    coord_flip() +
    labs(x = "Coefficient", y = "Year", title = reg, color = "Control Radius") +
    theme_bw() +
    theme(legend.position = "bottom")
}

reg_names <- unique(output$Regression)

# se tiver emp/wage, foca neles (senão plota tudo que existir)
keep <- reg_names[grepl("Employment|Wage|emp|wage", reg_names, ignore.case = TRUE)]
if (length(keep) > 0) reg_names <- keep

plots_list <- lapply(reg_names, plot_one)

# ---------------------------------------------------------
# AJUSTE: montar em apenas 1 linha
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

