# ============================================================================
# Appendix G, Figure G.1 — Alternative Sizes of the Control Rings in
# Worker-Level Estimations
# Treatment fixed at 0-5 km; control ring varies (40-70, 30-80, 50-100 km).
# Clustering: census tract. Employment outcome only. Uses `dados_main`
# (2002-2012) from 00b_load_worker_data.R.
# ============================================================================

log_msg("=== 01_figure_G1_alternative_control_rings_workers.R: start ===")

control_specs <- list("40-70" = c(40, 70), "30-80" = c(30, 80), "50-100" = c(50, 100))

outputs_list <- lapply(names(control_specs), function(k) {
  lo <- control_specs[[k]][1]; up <- control_specs[[k]][2]
  log_msg("Figure G.1: control ring %s", k)
  tryCatch(
    output_empregados(dados_main, WORKER_MIN_TREAT, WORKER_MAX_TREAT, lo, up,
                       trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA),
    error = function(e) { log_msg("ERROR (G.1 ring=%s): %s", k, conditionMessage(e)); NULL }
  )
})
names(outputs_list) <- names(control_specs)

all_results <- bind_rows(lapply(names(outputs_list), function(k) {
  df <- outputs_list[[k]]
  if (is.null(df)) return(NULL)
  df %>%
    filter(type == "type_treatment", Regression == "A - Employment (1/0)", grepl("^Flash Flood", parmseq)) %>%
    mutate(
      Control_Ring = k,
      Period = gsub("Flash Flood ", "", parmseq),
      Coefficient = as.numeric(estimate), SE = as.numeric(se),
      CI_Low = Coefficient - 1.96 * SE, CI_High = Coefficient + 1.96 * SE
    )
}))

# output_empregados()'s regression formula always includes treat_B_2008
# through treat_B_2016 as regressors (unchanged from the original
# read_functions.R). Since this figure uses `dados_main` (2002-2012), years
# 2013-2016 have no actual observations, so those dummy columns are
# structurally constant-zero and their coefficients are undefined/NA.
# Dropped here since only 2008-2012 have real data.
all_results <- all_results %>% filter(Period %in% c("Post", as.character(2008:2012)))

if (nrow(all_results) > 0) {
  all_results <- all_results %>%
    mutate(
      Period = factor(Period, levels = c("Post", as.character(2008:2012))),
      Control_Ring = factor(Control_Ring, levels = names(control_specs))
    )
  ring_colors <- c("40-70" = "#FEE0D2", "30-80" = "#FB6A4A", "50-100" = "#CB181D")

  make_panel <- function(df) {
    ggplot(df, aes(x = Period, y = Coefficient, color = Control_Ring)) +
      geom_point(position = position_dodge(width = 0.4), size = 2) +
      geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), position = position_dodge(width = 0.4), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_discrete(drop = FALSE) +
      scale_color_manual(values = ring_colors, name = "Control Ring") +
      labs(x = "Year", y = "Coefficient") + theme_bw() + theme(legend.position = "bottom")
  }

  fig <- make_panel(all_results)

  out_path <- file.path(figures_dir, "results_controles_empregados.png")
  ggsave(filename = out_path, plot = fig, dpi = 300, width = 8, height = 6, units = "in")
  log_msg("Saved figure: %s", out_path)
}

log_msg("=== 01_figure_G1_alternative_control_rings_workers.R: done ===")
