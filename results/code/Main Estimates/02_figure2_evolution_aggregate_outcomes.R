# ============================================================================
# Figure 2 — Evolution of Aggregate Outcomes (2003-2012)
# Treatment: 0-5 km vs. 50-80 km control. Aggregate proportions by group-year,
# no regression. Depends on `main_panel`, `raw_data`.
#
# ============================================================================

log_msg("=== 02_figure2_evolution_aggregate_outcomes.R: start ===")

reloc_wide <- raw_data %>%
  mutate(code_tract_num = make_numeric_id(code_tract)) %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1_wide = as.numeric(
      !is.na(dplyr::lead(code_tract_num)) & dplyr::lead(code_tract_num) != code_tract_num
    )
  ) %>%
  ungroup() %>%
  select(id_estab, year, reloc_tract_tminus1_wide)

main_panel_fig2 <- main_panel %>%
  select(-reloc_tract_tminus1) %>%
  left_join(reloc_wide, by = c("id_estab", "year")) %>%
  mutate(reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1_wide))

base_agregado <- main_panel_fig2 %>%
  mutate(firm = if_else(morte == 0, 1, NA_real_)) %>%
  group_by(treat_B, year) %>%
  summarise(
    new_firm = sum(new_firm, na.rm = TRUE), firm = sum(firm, na.rm = TRUE),
    morte = sum(morte, na.rm = TRUE), reloc_tract_tminus1 = sum(reloc_tract_tminus1, na.rm = TRUE),
    empregados = sum(empregados, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(between(year, 2003, 2012)) %>%
  mutate(
    morte = (morte / firm) * 100, reloc_tract_tminus1 = (reloc_tract_tminus1 / firm) * 100,
    empregados = empregados / firm
  ) %>%
  pivot_longer(cols = c(morte, reloc_tract_tminus1, empregados),
               names_to = "variable", values_to = "valor") %>%
  mutate(
    treat = case_when(treat_B == 1 ~ "Treat", treat_B == 0 ~ "Control", TRUE ~ "Others"),
    variable = case_when(
      variable == "morte" ~ "A - Closure Rate (%)",
      variable == "reloc_tract_tminus1" ~ "B - Relocation Rate (%)",
      variable == "empregados" ~ "C - Mean number of Employees",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(treat %in% c("Treat", "Control"), !is.na(variable)) %>%
  arrange(variable, treat, year)

vars_ordem <- c("A - Closure Rate (%)", "B - Relocation Rate (%)",
                "C - Mean number of Employees")

panels_missing <- setdiff(vars_ordem, unique(base_agregado$variable))
if (length(panels_missing) > 0) stop("Panels with no data: ", paste(panels_missing, collapse = " | "))

create_plot <- function(df, var_name) {
  df %>% filter(variable == var_name) %>%
    ggplot() +
    geom_rect(aes(xmin = 2007 - 0.0001, xmax = 2007 + 0.0001, ymin = -Inf, ymax = Inf), fill = "grey60", alpha = 0.25) +
    geom_vline(xintercept = 2007 - 0.0001, linetype = "dashed", color = "firebrick") +
    geom_vline(xintercept = 2007 + 0.0001, linetype = "dashed", color = "firebrick") +
    geom_line(aes(x = year, y = valor, colour = treat)) +
    geom_point(aes(x = year, y = valor, colour = treat), shape = 23, fill = "white") +
    scale_color_manual(values = c("Control" = "coral3", "Treat" = "cornflowerblue"), breaks = c("Control", "Treat"), name = "") +
    scale_x_continuous(breaks = 2003:2012, limits = c(2003, 2012)) +
    theme_bw() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var_name, x = "Year", y = "Value")
}

plots <- lapply(vars_ordem, function(v) create_plot(base_agregado, v))
combined_plot <- ggpubr::ggarrange(plotlist = plots, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

out_path <- file.path(figures_dir, "Fig_02_Evolution_Aggregate_Outcomes_5km.png")
ggsave(filename = out_path, plot = combined_plot, dpi = 300, width = 12, height = 9, units = "in")
log_msg("Saved figure: %s", out_path)

log_msg("=== 02_figure2_evolution_aggregate_outcomes.R: done ===")
