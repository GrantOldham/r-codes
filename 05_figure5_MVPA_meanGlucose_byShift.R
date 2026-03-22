source("R/00_helpers.R")
ensure_dir("outputs")

# Requires dat_mvpa with columns: id, mvpa_min_day, mean_glucose_day, shift_type_lab
# If absent, script tries to build from mvpa_daily + cgm_daily

if (exists("dat_mvpa", envir = .GlobalEnv, inherits = FALSE)) {
  dat_mvpa <- get("dat_mvpa", envir = .GlobalEnv)
} else {
  mvpa_daily <- load_or_stop("mvpa_daily", "data/processed/mvpa_daily.rds")
  cgm_daily  <- load_or_stop("cgm_daily",  "data/processed/cgm_daily.rds")
  dat_mvpa <- mvpa_daily %>%
    mutate(id = toupper(trimws(as.character(id))), date = as.Date(date)) %>%
    with_shift_lab() %>%
    select(id, date, shift_type_lab, mvpa_min_day) %>%
    inner_join(
      cgm_daily %>%
        mutate(id = toupper(trimws(as.character(id))), date = as.Date(date)) %>%
        select(id, date, mean_glucose_day = mean_glucose),
      by = c("id", "date")
    )
}

dat_mvpa <- dat_mvpa %>%
  mutate(
    id = toupper(trimws(as.character(id))),
    shift_type_lab = factor(as.character(shift_type_lab), levels = SHIFT_LEVELS)
  ) %>%
  filter(is.finite(mvpa_min_day), is.finite(mean_glucose_day), !is.na(shift_type_lab))

fit <- lmer(mean_glucose_day ~ mvpa_min_day * shift_type_lab + (1|id), data = dat_mvpa, REML = FALSE)

# per-panel slopes
slopes <- as.data.frame(summary(emtrends(fit, specs = "shift_type_lab", var = "mvpa_min_day"), infer = TRUE)) %>%
  transmute(
    shift_type_lab,
    beta = mvpa_min_day.trend,
    CI_low = lower.CL,
    CI_high = upper.CL,
    p_value = p.value,
    label = panel_label(beta, CI_low, CI_high, p_value)
  )

# overall LRTs
fit_no_int  <- update(fit, . ~ . - mvpa_min_day:shift_type_lab)
fit_no_mvpa <- update(fit, . ~ . - mvpa_min_day - mvpa_min_day:shift_type_lab)
lrt_int  <- anova(fit_no_int, fit)
lrt_mvpa <- anova(fit_no_mvpa, fit)

# predictions
pred_grid <- expand.grid(
  mvpa_min_day = seq(min(dat_mvpa$mvpa_min_day, na.rm = TRUE), max(dat_mvpa$mvpa_min_day, na.rm = TRUE), length.out = 100),
  shift_type_lab = levels(droplevels(dat_mvpa$shift_type_lab)),
  id = dat_mvpa$id[1]
)

pred <- cbind(pred_grid,
              as.data.frame(predict(fit, newdata = pred_grid, re.form = NA, se.fit = TRUE)))
# predict.lmerMod lacks se.fit in many installs; fallback via model.matrix
X <- model.matrix(~ mvpa_min_day * shift_type_lab, data = pred_grid)
V <- as.matrix(vcov(fit))
eta <- as.numeric(X %*% fixef(fit))
se  <- sqrt(diag(X %*% V %*% t(X)))
pred$fit <- eta
pred$lo  <- eta - 1.96 * se
pred$hi  <- eta + 1.96 * se

# annotation positions
ann <- dat_mvpa %>%
  group_by(shift_type_lab) %>%
  summarise(
    x = quantile(mvpa_min_day, 0.05, na.rm = TRUE),
    y = max(mean_glucose_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(slopes, by = "shift_type_lab")

p <- ggplot(dat_mvpa, aes(x = mvpa_min_day, y = mean_glucose_day)) +
  geom_point(alpha = 0.35, size = 1.6) +
  geom_ribbon(data = pred, aes(ymin = lo, ymax = hi), alpha = 0.18, fill = COL_GLU, colour = NA, inherit.aes = FALSE) +
  geom_line(data = pred, aes(y = fit), colour = COL_GLU, linewidth = 1.15, inherit.aes = FALSE) +
  geom_text(data = ann, aes(x = x, y = y, label = label), hjust = 0, vjust = 1, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ shift_type_lab, ncol = 3) +
  labs(x = "MVPA (min/day)", y = "Mean glucose (mmol/L)") +
  thesis_theme() +
  theme(legend.position = "none")

print(p)
ggsave("outputs/Figure5_MVPA_meanGlucose_byShift.pdf", p, width = 11, height = 7)
write.csv(slopes, "outputs/Figure5_panel_stats.csv", row.names = FALSE)
write.csv(data.frame(
  test = c("MVPA x shift interaction", "Overall MVPA"),
  chisq = c(lrt_int$Chisq[2], lrt_mvpa$Chisq[2]),
  df = c(lrt_int$Df[2], lrt_mvpa$Df[2]),
  p_value = c(lrt_int$`Pr(>Chisq)`[2], lrt_mvpa$`Pr(>Chisq)`[2])
), "outputs/Figure5_overall_stats.csv", row.names = FALSE)
