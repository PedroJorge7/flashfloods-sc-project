############################################################
## Figure F.2 - Alternative Sizes of the Treatment Radius in Worker-Level Estimations
## Tratamentos: 2.5 7.5 17.5 22.5 30 (control fixo 50â€“80)
## SaÃ­da: ./results/analysis/change_treatment_empregados.png
## AJUSTES:
##  - Cores padrÃ£o (paleta nomeada pelos nÃ­veis do radius)
##  - PainÃ©is em apenas 1 linha (ggarrange nrow = 1)
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(ggplot2)
library(ggpubr)
library(MatchIt)

OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

source("./results/code/read_functions.R")

# ---------------------------------------------------------
# Paleta padrÃ£o (NOMES = nÃ­veis do radius)
# ---------------------------------------------------------
treat_colors <- c(
  "0-2.5 km"  = "#FEE0D2",
  "0-7.5 km"  = "#FCBBA1",
  "0-17.5 km" = "#FB6A4A",
  "0-22.5 km" = "#EF3B2C",
  "0-30 km"   = "#CB181D"
)

# ---------------------------------------------------------
# Dados + filtros
# ---------------------------------------------------------
dados <- readRDS(data_path("workers_clean_data.rds")) %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# ---------------------------------------------------------
# Helper: garante tipos numÃ©ricos (evita plot â€œsumirâ€)
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
# Rodar outputs por raio de TRATAMENTO (control fixo 50â€“80)
# ---------------------------------------------------------
output_empregados_2_5  <- output_empregados(dados, 0,  2.5, 50, 80, trend = TRUE)
output_empregados_7_5  <- output_empregados(dados, 0,  7.5, 50, 80, trend = TRUE)
output_empregados_17_5 <- output_empregados(dados, 0, 17.5, 50, 80, trend = TRUE)
output_empregados_22_5 <- output_empregados(dados, 0, 22.5, 50, 80, trend = TRUE)
output_empregados_30   <- output_empregados(dados, 0, 30.0, 50, 80, trend = TRUE)

output <- bind_rows(
  output_empregados_2_5,
  output_empregados_7_5,
  output_empregados_17_5,
  output_empregados_22_5,
  output_empregados_30
) %>%
  filter(type == "type_treatment") %>%
  mutate(
    parmseq = gsub("^Flash Flood\\s+", "", as.character(parmseq)),
    radius  = paste0(treat, " km"),
    radius  = factor(radius, levels = c("0-2.5 km","0-7.5 km","0-17.5 km","0-22.5 km","0-30 km"))
  ) %>%
  clean_output_for_plot() %>%
  arrange(parmseq, radius)

if (nrow(output) == 0) stop("Robustez 2: `output` ficou vazio depois dos filtros/conversÃµes.")

# ---------------------------------------------------------
# Plot (comparando raios no MESMO painel, com cores padrÃ£o)
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
    scale_color_manual(values = treat_colors, drop = FALSE) +
    coord_flip() +
    labs(x = "Coefficient", y = "Year", title = reg, color = "Treatment Radius") +
    theme_bw() +
    theme(legend.position = "bottom")
}

reg_names <- unique(output$Regression)

# se tiver emp/wage, foca neles (senÃ£o plota tudo que existir)
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
  filename = file.path(OUT_DIR, "change_treatment_empregados.png"),
  plot     = fig,
  dpi      = 500,
  width    = 30,
  height   = 15,
  units    = "cm"
)
