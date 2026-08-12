# ============================================================================
# Appendix G, Figure G.2 — Alternative Sizes of the
# Treatment Radius in Worker-Level Estimations
# Radius ladder: 2.5, 5 (reference), 7.5, 10, 12.5, 15 km; control fixed at
# 50-80 km. Clustering: census tract. Employment outcome only. Uses
# `dados_main` (2002-2012) from 00b_load_worker_data.R and
# output_empregados().
# ============================================================================

log_msg("=== 02_figure_G2_alternative_treatment_radius_workers.R: start ===")

radius_specs <- list(
  "0-2.5 km"           = 2.5,
  "0-5 km (reference)" = 5,
  "0-7.5 km"           = 7.5,
  "0-10 km"            = 10,
  "0-12.5 km"          = 12.5,
  "0-15 km"            = 15
)

outputs_list <- lapply(names(radius_specs), function(k) {
  r <- radius_specs[[k]]
  log_msg("Figure G.2: treatment radius %s", k)
  tryCatch(
    output_empregados(dados_main, 0, r, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
                       trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA),
    error = function(e) { log_msg("ERROR (G.2 radius=%s): %s", k, conditionMessage(e)); NULL }
  )
})
names(outputs_list) <- names(radius_specs)

all_results <- bind_rows(lapply(names(outputs_list), function(k) {
  df <- outputs_list[[k]]
  if (is.null(df)) return(NULL)
  df %>%
    filter(type == "type_treatment", Regression == "A - Employment (1/0)", grepl("^Flash Flood", parmseq)) %>%
    mutate(
      Treatment_Radius = k, Period = gsub("Flash Flood ", "", parmseq),
      Coefficient = as.numeric(estimate), SE = as.numeric(se),
      CI_Low = Coefficient - 1.96 * SE, CI_High = Coefficient + 1.96 * SE
    )
}))

all_results <- all_results %>% filter(Period %in% c("Post", as.character(2008:2012)))

if (nrow(all_results) > 0) {
  all_results <- all_results %>%
    mutate(
      Period = factor(Period, levels = c("Post", as.character(2008:2012))),
      Treatment_Radius = factor(Treatment_Radius, levels = names(radius_specs))
    )
  rad_colors <- c("0-2.5 km" = "#FEE0D2", "0-5 km (reference)" = "#6BAED6", "0-7.5 km" = "#FCBBA1",
                  "0-10 km" = "#FC9272", "0-12.5 km" = "#FB6A4A", "0-15 km" = "#CB181D")

  make_panel <- function(df) {
    ggplot(df, aes(x = Period, y = Coefficient, color = Treatment_Radius)) +
      geom_point(position = position_dodge(width = 0.4), size = 2) +
      geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), position = position_dodge(width = 0.4), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_discrete(drop = FALSE) +
      scale_color_manual(values = rad_colors, name = "Treatment Radius (km)") +
      labs(x = "Year", y = "Coefficient") + theme_bw() + theme(legend.position = "bottom")
  }

  fig <- make_panel(all_results)

  out_path <- file.path(figures_dir, "Fig_G02_Alternative_Treatment_Radius_Workers_5km_tract.png")
  ggsave(filename = out_path, plot = fig, dpi = 300, width = 8, height = 6, units = "in")
  log_msg("Saved figure: %s", out_path)
}

log_msg("=== 02_figure_G2_alternative_treatment_radius_workers.R: done ===")
