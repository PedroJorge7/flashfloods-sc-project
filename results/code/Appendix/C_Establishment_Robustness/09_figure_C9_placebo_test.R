# ============================================================================
# Appendix C, Figure C.9 — Falsification Test for the Control Area in
# Establishment-Level Estimates
# The "treated" group here is the ordinary 50-80 km control ring, compared
# against four far-away placebo "control" rings (200-240 km) where no true
# effect should exist. Clustering: census tract. Uses `raw_data` (each
# placebo ring needs its own panel).
# ============================================================================

log_msg("=== 09_figure_C9_placebo_test.R: start ===")

# reloc_tract_tminus1 is recomputed here on the full raw_data (not the
# 2003-2012 panel) to avoid a boundary artifact at year_max.
reloc_fixed <- raw_data %>%
  dplyr::mutate(code_tract_num = make_numeric_id(code_tract)) %>%
  dplyr::arrange(id_estab, year) %>%
  dplyr::group_by(id_estab) %>%
  dplyr::mutate(
    reloc_tract_tminus1_fixed = as.numeric(
      !is.na(dplyr::lead(code_tract_num)) & dplyr::lead(code_tract_num) != code_tract_num
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(id_estab, year, reloc_tract_tminus1_fixed)

apply_reloc_fix <- function(panel) {
  panel %>%
    dplyr::left_join(reloc_fixed, by = c("id_estab", "year")) %>%
    dplyr::mutate(
      reloc_tract_tminus1 = dplyr::if_else(is.na(morte), NA_real_, reloc_tract_tminus1_fixed)
    ) %>%
    dplyr::select(-reloc_tract_tminus1_fixed)
}

control_specs <- list(
  "200-210 km" = list(lower = 200, upper = 210),
  "200-230 km" = list(lower = 200, upper = 230),
  "230-240 km" = list(lower = 230, upper = 240),
  "210-240 km" = list(lower = 210, upper = 240)
)

placebo_treated_rule <- function(d) dplyr::between(d, 50, 80)  # real control ring, used as "treated" here
outcomes <- c(morte = "Closure", reloc_tract_tminus1 = "Relocation")
all_results <- data.frame()

for (ring_label in names(control_specs)) {
  spec <- control_specs[[ring_label]]
  control_rule <- local({ lo <- spec$lower; hi <- spec$upper; function(d) dplyr::between(d, lo, hi) })
  ring_tag <- gsub("[^A-Za-z0-9]+", "_", ring_label)
  panel <- build_or_load_panel(
    file.path(cache_dir, sprintf("B4_panel_%s.rds", ring_tag)),
    function() build_establishment_panel(raw_data, placebo_treated_rule, sprintf("C.9 placebo=%s", ring_label), control_rule = control_rule)
  )
  panel <- apply_reloc_fix(panel)

  for (outcome in names(outcomes)) {
    res <- fit_establishment_models(panel, outcome, context = sprintf("C.9 / %s / %s", ring_label, outcomes[[outcome]]))
    if (!is.null(res$agg)) {
      agg_row <- broom::tidy(res$agg) %>% filter(term == "treat_B_agg") %>%
        transmute(Placebo_Ring = ring_label, Outcome = outcomes[[outcome]], Period = "Post",
                  Coefficient = estimate, CI_Low = estimate - 1.96*std.error, CI_High = estimate + 1.96*std.error)
      all_results <- bind_rows(all_results, agg_row)
    }
    if (!is.null(res$timevar)) {
      evt_rows <- broom::tidy(res$timevar) %>% filter(grepl("^treat_B_", term)) %>%
        mutate(Period = gsub("treat_B_", "", term)) %>%
        transmute(Placebo_Ring = ring_label, Outcome = outcomes[[outcome]], Period = Period,
                  Coefficient = estimate, CI_Low = estimate - 1.96*std.error, CI_High = estimate + 1.96*std.error)
      all_results <- bind_rows(all_results, evt_rows)
    }
    rm(res)
  }
  # Free this placebo ring's panel before building the next one.
  rm(panel)
  gc(full = TRUE)
}

all_results <- all_results %>%
  mutate(
    Period = factor(Period, levels = c("Post", "2008", "2009", "2010", "2011", "2012")),
    Placebo_Ring = factor(Placebo_Ring, levels = names(control_specs))
  )

placebo_colors <- c(
  "200-210 km" = "#FEE0D2",
  "200-230 km" = "#FCBBA1",
  "230-240 km" = "#FB6A4A",
  "210-240 km" = "#CB181D"
)

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Placebo_Ring)) +
    geom_point(position = position_dodge(width = 0.4), size = 2) +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), position = position_dodge(width = 0.4), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = placebo_colors, name = "Placebo Ring") +
    labs(x = "Year", y = "Coefficient", title = title_label) + theme_bw() + theme(legend.position = "bottom")
}

pA <- make_panel(filter(all_results, Outcome == "Closure"), "A - Establishment Closure (0/1)")
pB <- make_panel(filter(all_results, Outcome == "Relocation"), "B - Establishment Relocation")
fig <- ggpubr::ggarrange(pA, pB, ncol = 2, common.legend = TRUE, legend = "bottom")

out_path <- file.path(figures_dir, "results_falso_tratamento2.png")
ggsave(filename = out_path, plot = fig, dpi = 300, width = 12, height = 6, units = "in")
log_msg("Saved figure: %s", out_path)

log_msg("=== 09_figure_C9_placebo_test.R: done ===")
