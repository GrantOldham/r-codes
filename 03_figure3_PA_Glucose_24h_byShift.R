source("R/00_helpers.R")
ensure_dir("outputs")

hourly_plot2   <- load_or_stop("hourly_plot2", "data/processed/hourly_plot2.rds")
cgm_with_shift <- load_or_stop("cgm_with_shift", "data/processed/cgm_with_shift.rds")

summ_ci <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  n <- sum(is.finite(x))
  se <- s / sqrt(pmax(n, 1))
  c(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se)
}

pa_df <- hourly_plot2 %>%
  mutate(id = toupper(trimws(as.character(id))), hour = as.integer(hour)) %>%
  with_shift_lab() %>%
  filter(!is.na(hour))

pa_id_hour <- pa_df %>%
  group_by(id, shift_type_lab, hour) %>%
  summarise(
    sedentary = mean(sedentary_min, na.rm = TRUE),
    light     = mean(light_min, na.rm = TRUE),
    mvpa      = mean(mvpa_min, na.rm = TRUE),
    .groups = "drop"
  )

pa_sum <- pa_id_hour %>%
  group_by(shift_type_lab, hour) %>%
  summarise(
    sed_mean  = summ_ci(sedentary)["mean"],
    sed_lo    = summ_ci(sedentary)["lo"],
    sed_hi    = summ_ci(sedentary)["hi"],
    lgt_mean  = summ_ci(light)["mean"],
    lgt_lo    = summ_ci(light)["lo"],
    lgt_hi    = summ_ci(light)["hi"],
    mvpa_mean = summ_ci(mvpa)["mean"],
    mvpa_lo   = summ_ci(mvpa)["lo"],
    mvpa_hi   = summ_ci(mvpa)["hi"],
    .groups = "drop"
  )

glu_df <- cgm_with_shift %>%
  mutate(id = toupper(trimws(as.character(id))), hour = as.integer(hour)) %>%
  with_shift_lab() %>%
  filter(!is.na(hour), is.finite(mean_glucose))

glu_id_hour <- glu_df %>%
  group_by(id, shift_type_lab, hour) %>%
  summarise(glucose = mean(mean_glucose, na.rm = TRUE), .groups = "drop")

glu_sum <- glu_id_hour %>%
  group_by(shift_type_lab, hour) %>%
  summarise(
    mean = summ_ci(glucose)["mean"],
    lo   = summ_ci(glucose)["lo"],
    hi   = summ_ci(glucose)["hi"],
    .groups = "drop"
  )

g_min <- min(glu_sum$lo, na.rm = TRUE)
g_max <- max(glu_sum$hi, na.rm = TRUE)
to60 <- function(x) (x - g_min) / (g_max - g_min) * 60
from60 <- function(y) (y / 60) * (g_max - g_min) + g_min

glu_sum <- glu_sum %>% mutate(mean_y = to60(mean), lo_y = to60(lo), hi_y = to60(hi))

p <- ggplot() +
  geom_ribbon(data = pa_sum, aes(x = hour, ymin = sed_lo, ymax = sed_hi), fill = COL_SED, alpha = 0.26, colour = NA) +
  geom_line(data = pa_sum, aes(x = hour, y = sed_mean, colour = "Sedentary"), linewidth = 1.15) +
  geom_ribbon(data = pa_sum, aes(x = hour, ymin = lgt_lo, ymax = lgt_hi), fill = COL_LIGHT, alpha = 0.26, colour = NA) +
  geom_line(data = pa_sum, aes(x = hour, y = lgt_mean, colour = "Light"), linewidth = 1.15) +
  geom_ribbon(data = pa_sum, aes(x = hour, ymin = mvpa_lo, ymax = mvpa_hi), fill = COL_MVPA, alpha = 0.26, colour = NA) +
  geom_line(data = pa_sum, aes(x = hour, y = mvpa_mean, colour = "MVPA"), linewidth = 1.15) +
  geom_ribbon(data = glu_sum, aes(x = hour, ymin = lo_y, ymax = hi_y), fill = COL_GLU, alpha = 0.18, colour = NA) +
  geom_line(data = glu_sum, aes(x = hour, y = mean_y, colour = "Glucose"), linewidth = 1.15) +
  facet_wrap(~ shift_type_lab, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 21, 3), limits = c(0, 23), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, 60), breaks = c(0, 20, 40, 60), expand = c(0, 0),
    name = "Physical activity (min/h)", sec.axis = sec_axis(~ from60(.), name = "Glucose")
  ) +
  scale_colour_manual(values = c("Sedentary" = COL_SED, "Light" = COL_LIGHT, "MVPA" = COL_MVPA, "Glucose" = COL_GLU)) +
  labs(x = "Hour of day (24-h clock)", colour = NULL) +
  thesis_theme()

print(p)
ggsave("outputs/Figure2_PA_Glucose_24h_byShift_CI.pdf", p, width = 11, height = 6.5)
