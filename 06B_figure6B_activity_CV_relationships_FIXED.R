# =========================================================
# FIGURE 7B: Daily activity intensity vs glucose variability (CV%)
# Participant-level relationships
#
# Requires:
#   dat_id with columns:
#     id, sedentary_hr, light_hr, mvpa_hr, cv_percent
#
# Saves:
#   outputs/Figure7B_activity_CV_relationships.pdf
#   outputs/Figure7B_activity_CV_relationships.png
#   outputs/Figure7B_activity_CV_stats.csv
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)

if (!dir.exists("outputs")) dir.create("outputs")

stopifnot(exists("dat_id"))

# -----------------------
# Clean data
# -----------------------
df <- dat_id %>%
  mutate(
    id = as.character(id),
    sedentary_hr = as.numeric(sedentary_hr),
    light_hr     = as.numeric(light_hr),
    mvpa_hr      = as.numeric(mvpa_hr),
    cv_percent   = as.numeric(cv_percent)
  ) %>%
  filter(
    is.finite(cv_percent),
    is.finite(sedentary_hr),
    is.finite(light_hr),
    is.finite(mvpa_hr)
  )

# -----------------------
# Long format for plotting
# -----------------------
plot_df <- df %>%
  pivot_longer(
    cols = c(sedentary_hr, light_hr, mvpa_hr),
    names_to = "intensity",
    values_to = "hours"
  ) %>%
  mutate(
    intensity = factor(
      intensity,
      levels = c("sedentary_hr", "light_hr", "mvpa_hr"),
      labels = c("Sedentary", "Light", "MVPA")
    )
  )

# -----------------------
# Stats helper
# -----------------------
extract_stats_lm <- function(data, predictor){
  f_full <- as.formula(paste0("cv_percent ~ ", predictor))
  fit <- lm(f_full, data = data)
  s <- summary(fit)
  ct <- coef(s)

  beta <- ct[predictor, "Estimate"]
  se   <- ct[predictor, "Std. Error"]
  tval <- ct[predictor, "t value"]
  pval <- ct[predictor, "Pr(>|t|)"]
  dfres <- df.residual(fit)

  ci_lo <- beta - 1.96 * se
  ci_hi <- beta + 1.96 * se
  r_val <- sqrt((tval^2) / (tval^2 + dfres))
  f_val <- unname(s$fstatistic[1])

  data.frame(
    Predictor = predictor,
    Beta = beta,
    SE = se,
    CI_Lower = ci_lo,
    CI_Upper = ci_hi,
    t_value = tval,
    df = dfres,
    p_value = pval,
    r_value = r_val,
    F_value = f_val
  )
}

stats_df <- bind_rows(
  extract_stats_lm(df, "sedentary_hr"),
  extract_stats_lm(df, "light_hr"),
  extract_stats_lm(df, "mvpa_hr")
) %>%
  mutate(
    Intensity = c("Sedentary", "Light", "MVPA"),
    Beta = round(Beta, 3),
    SE = round(SE, 3),
    CI_Lower = round(CI_Lower, 3),
    CI_Upper = round(CI_Upper, 3),
    t_value = round(t_value, 3),
    df = round(df, 1),
    p_value = signif(p_value, 3),
    r_value = round(r_value, 3),
    F_value = round(F_value, 3)
  ) %>%
  select(Intensity, Beta, SE, CI_Lower, CI_Upper, t_value, df, p_value, r_value, F_value)

write.csv(stats_df, "outputs/Figure7B_activity_CV_stats.csv", row.names = FALSE)

# -----------------------
# Plot
# -----------------------
p7b <- ggplot(plot_df, aes(x = hours, y = cv_percent)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.0, alpha = 0.15) +
  facet_wrap(~ intensity, ncol = 3, scales = "free_x") +
  labs(
    x = "Hours/day",
    y = "Glucose variability (CV%)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_text(size = 11)
  )

print(p7b)

ggsave("outputs/Figure7B_activity_CV_relationships.pdf", p7b, width = 11, height = 4.5)
ggsave("outputs/Figure7B_activity_CV_relationships.png", p7b, width = 11, height = 4.5, dpi = 300)
