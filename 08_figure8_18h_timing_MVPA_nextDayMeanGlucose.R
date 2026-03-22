# =========================================================
# FIGURE 8: 18-hour pre vs post night-shift MVPA tertiles
#           vs next-day mean glucose
#
# Assumes dat_timing_18h exists with columns:
#   id, date, next_date, mvpa_pre18, mvpa_post18
#
# and cgm_daily exists with:
#   id, date, mean_glucose
#
# If your MVPA columns are named differently, edit mvpa_pre_col / mvpa_post_col
#
# Saves:
#   outputs/Figure8_18h_timing_MVPA_nextDayMeanGlucose.pdf
#   outputs/Figure8_18h_timing_MVPA_nextDayMeanGlucose_stats.csv
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)

if (!dir.exists("outputs")) dir.create("outputs")

stopifnot(exists("dat_timing_18h"))
stopifnot(exists("cgm_daily"))

mvpa_pre_col  <- "mvpa_pre18"
mvpa_post_col <- "mvpa_post18"

# -----------------------
# Build analysis dataset
# -----------------------
dat8 <- dat_timing_18h %>%
  mutate(
    id = toupper(trimws(as.character(id))),
    next_date = as.Date(next_date)
  ) %>%
  left_join(
    cgm_daily %>%
      mutate(
        id = toupper(trimws(as.character(id))),
        date = as.Date(date)
      ) %>%
      select(id, date, mean_glucose),
    by = c("id" = "id", "next_date" = "date")
  ) %>%
  rename(next_day_glucose = mean_glucose) %>%
  pivot_longer(
    cols = c(all_of(mvpa_pre_col), all_of(mvpa_post_col)),
    names_to = "timing",
    values_to = "mvpa_min"
  ) %>%
  mutate(
    timing = recode(
      timing,
      !!mvpa_pre_col := "Pre",
      !!mvpa_post_col := "Post"
    ),
    timing = factor(timing, levels = c("Pre", "Post"))
  ) %>%
  filter(is.finite(mvpa_min), is.finite(next_day_glucose))

# -----------------------
# Tertiles within timing
# -----------------------
dat8 <- dat8 %>%
  group_by(timing) %>%
  mutate(mvpa_tertile = ntile(mvpa_min, 3)) %>%
  ungroup() %>%
  mutate(
    mvpa_tertile = factor(
      mvpa_tertile,
      levels = c(1, 2, 3),
      labels = c("Low", "Medium", "High")
    )
  )

# -----------------------
# Model
# -----------------------
fit8 <- lmer(
  next_day_glucose ~ mvpa_tertile * timing + (1|id),
  data = dat8,
  REML = FALSE
)

fit8_no_tertile <- lmer(
  next_day_glucose ~ timing + (1|id),
  data = dat8,
  REML = FALSE
)

fit8_no_int <- lmer(
  next_day_glucose ~ mvpa_tertile + timing + (1|id),
  data = dat8,
  REML = FALSE
)

anova_terms <- anova(fit8)
lrt_tertile <- anova(fit8_no_tertile, fit8)
lrt_int <- anova(fit8_no_int, fit8)

emm8 <- emmeans(fit8, ~ mvpa_tertile | timing)
pairs8 <- contrast(emm8, method = "pairwise", adjust = "tukey")

# Save stats
stats_main <- data.frame(
  Effect = c("mvpa_tertile", "timing", "mvpa_tertile:timing"),
  F_value = c(anova_terms$`F value`[1], anova_terms$`F value`[2], anova_terms$`F value`[3]),
  p_value = c(anova_terms$`Pr(>F)`[1], anova_terms$`Pr(>F)`[2], anova_terms$`Pr(>F)`[3])
)

stats_lrt <- data.frame(
  Test = c("Tertile+interaction vs timing only", "Interaction vs additive model"),
  ChiSq = c(lrt_tertile$Chisq[2], lrt_int$Chisq[2]),
  df = c(lrt_tertile$Df[2], lrt_int$Df[2]),
  p_value = c(lrt_tertile$`Pr(>Chisq)`[2], lrt_int$`Pr(>Chisq)`[2])
)

write.csv(stats_main, "outputs/Figure8_18h_timing_MVPA_nextDayMeanGlucose_stats_main.csv", row.names = FALSE)
write.csv(as.data.frame(pairs8), "outputs/Figure8_18h_timing_MVPA_nextDayMeanGlucose_pairwise.csv", row.names = FALSE)
write.csv(stats_lrt, "outputs/Figure8_18h_timing_MVPA_nextDayMeanGlucose_lrt.csv", row.names = FALSE)

# -----------------------
# Plot
# -----------------------
plot_dat <- as.data.frame(summary(emm8, infer = TRUE))

p8 <- ggplot(plot_dat, aes(x = mvpa_tertile, y = emmean, group = timing)) +
  geom_point(position = position_dodge(width = 0.25), size = 2.5) +
  geom_line(aes(linetype = timing), position = position_dodge(width = 0.25), linewidth = 0.9) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.12,
    position = position_dodge(width = 0.25)
  ) +
  facet_wrap(~ timing, nrow = 1) +
  labs(
    x = "MVPA tertile (18 h window)",
    y = "Next-day mean glucose (mmol/L)",
    linetype = NULL
  ) +
  theme_classic(base_size = 12)

print(p8)
ggsave("outputs/Figure8_18h_timing_MVPA_nextDayMeanGlucose.pdf", p8, width = 9, height = 4.5)
