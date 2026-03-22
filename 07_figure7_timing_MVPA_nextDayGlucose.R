source("R/00_helpers.R")
ensure_dir("outputs")

# Requires dat_timing with columns: id, mvpa_pre, mvpa_post, next_day_glucose
# If next_day_glucose absent but cgm_daily + next_date exist, it is rebuilt.

dat_timing <- load_or_stop("dat_timing", "data/processed/dat_timing.rds")

if (!"next_day_glucose" %in% names(dat_timing)) {
  cgm_daily <- load_or_stop("cgm_daily", "data/processed/cgm_daily.rds")
  dat_timing <- dat_timing %>%
    left_join(
      cgm_daily %>% mutate(id = toupper(trimws(as.character(id))), date = as.Date(date)) %>%
        select(id, date, next_day_glucose = mean_glucose),
      by = c("id", "next_date" = "date")
    )
}

dat_long <- dat_timing %>%
  mutate(id = toupper(trimws(as.character(id)))) %>%
  select(id, mvpa_pre, mvpa_post, next_day_glucose) %>%
  tidyr::pivot_longer(cols = c(mvpa_pre, mvpa_post), names_to = "timing", values_to = "mvpa_min") %>%
  mutate(
    timing = recode(timing, mvpa_pre = "Pre", mvpa_post = "Post"),
    timing = factor(timing, levels = c("Pre", "Post"))
  ) %>%
  filter(is.finite(mvpa_min), is.finite(next_day_glucose))

fit <- lmer(next_day_glucose ~ mvpa_min * timing + (1|id), data = dat_long, REML = FALSE)
slopes <- as.data.frame(summary(emtrends(fit, specs = "timing", var = "mvpa_min"), infer = TRUE)) %>%
  transmute(
    timing,
    beta = mvpa_min.trend,
    CI_low = lower.CL,
    CI_high = upper.CL,
    p_value = p.value,
    label = panel_label(beta, CI_low, CI_high, p_value)
  )

fit_no_timing <- lmer(next_day_glucose ~ mvpa_min + (1|id), data = dat_long, REML = FALSE)
lrt <- anova(fit_no_timing, fit)

pred_grid <- expand.grid(
  mvpa_min = seq(min(dat_long$mvpa_min, na.rm = TRUE), max(dat_long$mvpa_min, na.rm = TRUE), length.out = 100),
  timing = levels(dat_long$timing),
  id = dat_long$id[1]
)
X <- model.matrix(~ mvpa_min * timing, data = pred_grid)
V <- as.matrix(vcov(fit))
eta <- as.numeric(X %*% fixef(fit))
se  <- sqrt(diag(X %*% V %*% t(X)))
pred_grid$fit <- eta
pred_grid$lo  <- eta - 1.96 * se
pred_grid$hi  <- eta + 1.96 * se

ann <- dat_long %>%
  group_by(timing) %>%
  summarise(x = quantile(mvpa_min, 0.05, na.rm = TRUE), y = max(next_day_glucose, na.rm = TRUE), .groups = "drop") %>%
  left_join(slopes, by = "timing")

p <- ggplot(dat_long, aes(x = mvpa_min, y = next_day_glucose)) +
  geom_point(alpha = 0.35, size = 1.6) +
  geom_ribbon(data = pred_grid, aes(ymin = lo, ymax = hi), fill = COL_GLU, alpha = 0.18, colour = NA, inherit.aes = FALSE) +
  geom_line(data = pred_grid, aes(y = fit), colour = COL_GLU, linewidth = 1.15, inherit.aes = FALSE) +
  geom_text(data = ann, aes(label = label), hjust = 0, vjust = 1, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ timing, ncol = 2) +
  labs(x = "MVPA in window (min)", y = "Next-day mean glucose (mmol/L)") +
  thesis_theme() +
  theme(legend.position = "none")

print(p)
ggsave("outputs/Figure6_timing_MVPA_nextDayGlucose.pdf", p, width = 9, height = 4.8)
write.csv(slopes, "outputs/Figure6_panel_stats.csv", row.names = FALSE)
write.csv(data.frame(
  test = "Timing + interaction (LRT)",
  chisq = lrt$Chisq[2],
  df = lrt$Df[2],
  p_value = lrt$`Pr(>Chisq)`[2]
), "outputs/Figure6_overall_stats.csv", row.names = FALSE)
