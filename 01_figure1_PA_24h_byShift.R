source("R/00_helpers.R")
ensure_dir("outputs")

hourly_plot2 <- load_or_stop("hourly_plot2", "data/processed/hourly_plot2.rds")

pa_df <- hourly_plot2 %>%
  mutate(
    id = toupper(trimws(as.character(id))),
    date = as.Date(date),
    hour = as.integer(hour)
  ) %>%
  with_shift_lab() %>%
  select(id, date, hour, shift_type_lab, sedentary_min, light_min, mvpa_min)

pa_id_hour <- pa_df %>%
  group_by(id, shift_type_lab, hour) %>%
  summarise(
    sedentary = mean(sedentary_min, na.rm = TRUE),
    light     = mean(light_min, na.rm = TRUE),
    mvpa      = mean(mvpa_min, na.rm = TRUE),
    .groups = "drop"
  )

pa_sum <- pa_id_hour %>%
  tidyr::pivot_longer(
    cols = c(sedentary, light, mvpa),
    names_to = "intensity",
    values_to = "value"
  ) %>%
  mutate(intensity = factor(intensity,
                            levels = c("sedentary", "light", "mvpa"),
                            labels = c("Sedentary", "Light", "MVPA"))) %>%
  group_by(shift_type_lab, hour, intensity) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    n    = sum(is.finite(value)),
    se   = sd / sqrt(pmax(n, 1)),
    lo   = mean - 1.96 * se,
    hi   = mean + 1.96 * se,
    .groups = "drop"
  )

p <- ggplot(pa_sum, aes(x = hour, y = mean, colour = intensity, fill = intensity)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.26, colour = NA) +
  geom_line(linewidth = 1.15) +
  facet_wrap(~ shift_type_lab, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 21, 3), limits = c(0, 23), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 20, 40, 60), expand = c(0, 0)) +
  scale_colour_manual(values = c("Sedentary" = COL_SED, "Light" = COL_LIGHT, "MVPA" = COL_MVPA)) +
  scale_fill_manual(values = c("Sedentary" = COL_SED, "Light" = COL_LIGHT, "MVPA" = COL_MVPA)) +
  labs(x = "Hour of day (24-h clock)", y = "Physical activity (min/h)", colour = NULL, fill = NULL) +
  thesis_theme()

print(p)
ggsave("outputs/Figure1_PA_24h_byShift_CI.pdf", p, width = 11, height = 6)
write.csv(pa_sum, "outputs/Figure1_PA_summary_CI.csv", row.names = FALSE)
