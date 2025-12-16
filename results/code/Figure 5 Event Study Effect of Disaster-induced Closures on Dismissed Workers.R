############################################################
## Event Study (Workers) – usando output_empregados + Aux.R
## - NÃO usa .dta do Stata
## - NÃO tem migration
## - Plota com cowplot e salva PNG
############################################################

rm(list = ls())

library(dplyr)
library(cowplot)
library(ggplot2)
library(MatchIt)

# ---------------------------------------------------------
# Helper: mesmo ylim para Employment e Wage (captura os DOIS)
# ---------------------------------------------------------
.get_ylim_gg <- function(p) {
  b <- ggplot_build(p)
  
  # ggplot2 novo: pode ter múltiplos painéis
  if (!is.null(b$layout$panel_params) && length(b$layout$panel_params) > 0) {
    yr <- unlist(lapply(b$layout$panel_params, function(pp) pp$y.range), use.names = FALSE)
    yr <- yr[is.finite(yr)]
    if (length(yr) >= 2) return(range(yr))
  }
  
  # fallback ggplot2 antigo
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
  
  # pequena margem pra não cortar nada
  span <- diff(yl)
  if (is.finite(span) && span > 0) {
    yl <- yl + c(-1, 1) * pad * span
  }
  
  plots[[idx_emp]]  <- plots[[idx_emp]]  + coord_cartesian(ylim = yl)
  plots[[idx_wage]] <- plots[[idx_wage]] + coord_cartesian(ylim = yl)
  
  plots
}

# ---------------------------------------------------------
# 1) Carrega Aux (output_empregados + event_study_plot)
# ---------------------------------------------------------
source("./results/code/Aux.R")

# ---------------------------------------------------------
# 2) Dados + filtros (mesma lógica que você fixou)
# ---------------------------------------------------------
dados <- readRDS("./data/workers_clean_data.rds") %>%
  filter(emprego_06_07 == 1, mesma_empresa_06_07 == TRUE) %>%
  filter(between(year, 2002, 2012))

# ---------------------------------------------------------
# 3) Roda outputs
# ---------------------------------------------------------
output_trend <- output_empregados(dados, 0, 12.5, 50, 80, trend = TRUE)

# ---------------------------------------------------------
# 4) Remover migration (se existir)
# ---------------------------------------------------------
if ("Regression" %in% names(output_trend)) {
  output_trend <- output_trend %>%
    filter(!grepl("migration", Regression, ignore.case = TRUE))
} else {
  stop("output_trend não tem a coluna `Regression`. Verifique o retorno de output_empregados().")
}

# ---------------------------------------------------------
# 5) Gerar plots
# ---------------------------------------------------------
reg_names <- unique(output_trend$Regression)

keep <- reg_names[grepl("emp", reg_names, ignore.case = TRUE) |
                    grepl("wage", reg_names, ignore.case = TRUE)]
if (length(keep) >= 2) reg_names <- keep

plots <- lapply(reg_names, function(rg) event_study_plot(rg))

# >>> AQUI: mesmo ylim para os dois, capturando ambos
plots <- .force_common_ylim_emp_wage(plots, reg_names, pad = 0.03)

fig <- cowplot::plot_grid(plotlist = plots, nrow = 1)

# ---------------------------------------------------------
# 6) Salvar
# ---------------------------------------------------------
ggsave(
  filename = "./results/analysis/event_study_empregados.png",
  plot     = fig,
  dpi      = 500,
  width    = 30,
  height   = 15,
  units    = "cm"
)
