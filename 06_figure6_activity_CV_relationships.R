source("R/00_helpers.R")
ensure_dir("outputs")

# Requires dat_id with columns: id, sedentary_hr, light_hr, mvpa_hr, cv_percent

dat_id <- load_or_stop("dat_id", "data/processed/dat_id.rds")

dat_id <- dat_id %>%
  mutate(
    id = toupper(trimws(as.character(id))),
    sedentary_hr = as.numeric(sedentary_hr),
    light_hr = as.numeric(light_hr),
    mvpa_hr = as.numeric(mvpa_hr),
    cv_percent = as.numeric(cv_percent)
  ) %>%
  filter(is.finite(cv_percent))

extract_lm <- function(df, predictor) {
  fit <- lm(reformulate(predictor, response = "cv_percent"), data = df)
  s <- summary(fit)$coefficients
  beta <- s[predictor, "Estimate"]
  se   <- s[predictor, "Std. Error"]
  p    <- s[predictor, "Pr(>|t|)"]
  ci   <- beta + c(-1.96, 1.96) * se
  data.frame(
    predictor = predictor,
    beta = beta,
    CI_low = ci[1],
    CI_high = ci[2],
    p_value = p,
    label = panel_label(beta, ci[1], ci[2], p, digits_beta = 3, digits_ci = 3)
  )
}

stats_tab <- bind_rows(
  extract_lm(dat_id, "sedentary_hr"),
  extract_lm(dat_id, "light_hr"),
  extract_lm(dat_id, "mvpa_hr")
)

plot_df <- dat_id %>%
  select(id, cv_percent, sedentary_hr, light_hr, mvpa_hr) %>%
  tidyr::pivot_longer(cols = c(sedentary_hr, light_hr, mvpa_hr), names_to = "predictor", values_to = "hours") %>%
  mutate(
    predictor = factor(predictor,
                       levels = c("sedentary_hr", "light_hr", "mvpa_hr"),
                       labels = c("Sedentary", "Light", "MVPA"))
  )

stats_tab$predictor <- factor(stats_tab$predictor,
                              levels = c("sedentary_hr", "light_hr", "mvpa_hr"),
                              labels = c("Sedentary", "Light", "MVPA"))

ann <- plot_df %>%
  group_by(predictor) %>%
  summarise(x = quantile(hours, 0.05, na.rm = TRUE), y = max(cv_percent, na.rm = TRUE), .groups = "drop") %>%
  left_join(stats_tab, by = "predictor")

col_map <- c("Sedentary" = COL_SED, "Light" = COL_LIGHT, "MVPA" = COL_MVPA)

p <- ggplot(plot_df, aes(x = hours, y = cv_percent, colour = predictor, fill = predictor)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.0, alpha = 0.16) +
  geom_text(data = ann, aes(x = x, y = y, label = label), hjust = 0, vjust = 1, size = 3.2, inherit.aes = FALSE, colour = "black") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 1) +
  scale_colour_manual(values = col_map) +
  scale_fill_manual(values = col_map) +
  labs(x = "Activity (h/day)", y = "Glucose variability (CV%)", colour = NULL, fill = NULL) +
  thesis_theme() +
  theme(legend.position = "none")

print(p)
ggsave("outputs/Figure7_activity_CV_relationships.pdf", p, width = 12, height = 4.5)
write.csv(stats_tab, "outputs/Figure7_panel_stats.csv", row.names = FALSE)
