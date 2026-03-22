source("R/00_helpers.R")
ensure_dir("outputs")

# Figure 5B: Daily MVPA -> glucose variability (CV%) by shift type
# Requires dat_mvpa_cv, OR mvpa_daily + cgm_daily_var (or cgm_with_shift to rebuild cgm_daily_var)

if (exists("dat_mvpa_cv", envir = .GlobalEnv, inherits = FALSE)) {
  dat_mvpa_cv <- get("dat_mvpa_cv", envir = .GlobalEnv)
} else {
  mvpa_daily <- load_or_stop("mvpa_daily", "data/processed/mvpa_daily.rds")

  if (exists("cgm_daily_var", envir = .GlobalEnv, inherits = FALSE)) {
    cgm_daily_var <- get("cgm_daily_var", envir = .GlobalEnv)
  } else if (file.exists("data/processed/cgm_daily_var.rds")) {
    cgm_daily_var <- readRDS("data/processed/cgm_daily_var.rds")
  } else {
    cgm_with_shift <- load_or_stop("cgm_with_shift", "data/processed/cgm_with_shift.rds")
    cgm_daily_var <- cgm_with_shift %>%
      mutate(
        id = toupper(trimws(as.character(id))),
        date = as.Date(date)
      ) %>%
      with_shift_lab() %>%
      group_by(id, date, shift_type_lab) %>%
      summarise(
        mean_glucose_day = mean(mean_glucose, na.rm = TRUE),
        sd_glucose_day   = sd(mean_glucose, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(cv_percent = 100 * (sd_glucose_day / mean_glucose_day)) %>%
      filter(is.finite(cv_percent))
  }

  dat_mvpa_cv <- mvpa_daily %>%
    mutate(id = toupper(trimws(as.character(id))), date = as.Date(date)) %>%
    with_shift_lab() %>%
    select(id, date, shift_type_lab, mvpa_min_day) %>%
    inner_join(
      cgm_daily_var %>%
        mutate(id = toupper(trimws(as.character(id))), date = as.Date(date)) %>%
        with_shift_lab() %>%
        select(id, date, shift_type_lab, cv_percent),
      by = c("id", "date", "shift_type_lab")
    )
}

dat_mvpa_cv <- dat_mvpa_cv %>%
  mutate(
    id = toupper(trimws(as.character(id))),
    shift_type_lab = factor(as.character(shift_type_lab), levels = SHIFT_LEVELS)
  ) %>%
  filter(is.finite(mvpa_min_day), is.finite(cv_percent), !is.na(shift_type_lab))

fit <- lmer(cv_percent ~ mvpa_min_day * shift_type_lab + (1|id), data = dat_mvpa_cv, REML = FALSE)

slopes <- as.data.frame(summary(emtrends(fit, specs = "shift_type_lab", var = "mvpa_min_day"), infer = TRUE)) %>%
  transmute(
    shift_type_lab,
    beta = mvpa_min_day.trend,
    CI_low = lower.CL,
    CI_high = upper.CL,
    p_value = p.value,
    label = panel_label(beta, CI_low, CI_high, p_value)
  )

fit_no_int  <- update(fit, . ~ . - mvpa_min_day:shift_type_lab)
fit_no_mvpa <- update(fit, . ~ . - mvpa_min_day - mvpa_min_day:shift_type_lab)
lrt_int  <- anova(fit_no_int, fit)
lrt_mvpa <- anova(fit_no_mvpa, fit)

pred_grid <- expand.grid(
  mvpa_min_day = seq(min(dat_mvpa_cv$mvpa_min_day, na.rm = TRUE), max(dat_mvpa_cv$mvpa_min_day, na.rm = TRUE), length.out = 100),
  shift_type_lab = levels(droplevels(dat_mvpa_cv$shift_type_lab)),
  id = dat_mvpa_cv$id[1]
)
X <- model.matrix(~ mvpa_min_day * shift_type_lab, data = pred_grid)
V <- as.matrix(vcov(fit))
eta <- as.numeric(X %*% fixef(fit))
se  <- sqrt(diag(X %*% V %*% t(X)))
pred_grid$fit <- eta
pred_grid$lo  <- eta - 1.96 * se
pred_grid$hi  <- eta + 1.96 * se

ann <- dat_mvpa_cv %>%
  group_by(shift_type_lab) %>%
  summarise(
    x = quantile(mvpa_min_day, 0.05, na.rm = TRUE),
    y = max(cv_percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(slopes, by = "shift_type_lab")

p <- ggplot(dat_mvpa_cv, aes(x = mvpa_min_day, y = cv_percent)) +
  geom_point(alpha = 0.35, size = 1.6) +
  geom_ribbon(data = pred_grid, aes(ymin = lo, ymax = hi), alpha = 0.18, fill = COL_GLU, colour = NA, inherit.aes = FALSE) +
  geom_line(data = pred_grid, aes(y = fit), colour = COL_GLU, linewidth = 1.15, inherit.aes = FALSE) +
  geom_text(data = ann, aes(x = x, y = y, label = label), hjust = 0, vjust = 1, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ shift_type_lab, ncol = 3) +
  labs(x = "MVPA (min/day)", y = "Glucose variability (CV%)") +
  thesis_theme() +
  theme(legend.position = "none")

print(p)
ggsave("outputs/Figure5B_MVPA_CV_byShift.pdf", p, width = 11, height = 7)
write.csv(slopes, "outputs/Figure5B_panel_stats.csv", row.names = FALSE)
write.csv(data.frame(
  test = c("MVPA x shift interaction", "Overall MVPA"),
  chisq = c(lrt_int$Chisq[2], lrt_mvpa$Chisq[2]),
  df = c(lrt_int$Df[2], lrt_mvpa$Df[2]),
  p_value = c(lrt_int$`Pr(>Chisq)`[2], lrt_mvpa$`Pr(>Chisq)`[2])
), "outputs/Figure5B_overall_stats.csv", row.names = FALSE)
