source("R/00_helpers.R")
ensure_dir("outputs")

sleep_hourly   <- load_or_stop("sleep_hourly", "data/processed/sleep_hourly.rds")
cgm_with_shift <- load_or_stop("cgm_with_shift", "data/processed/cgm_with_shift.rds")

summ_ci <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  n <- sum(is.finite(x))
  se <- s / sqrt(pmax(n, 1))
  c(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se)
}

sleep_df <- sleep_hourly %>%
  mutate(id = toupper(trimws(as.character(id))), hour = as.integer(hour)) %>%
  with_shift_lab() %>%
  filter(!is.na(hour))

sleep_id_hour <- sleep_df %>%
  group_by(id, shift_type_lab, hour) %>%
  summarise(sleep = mean(sleep_min, na.rm = TRUE), .groups = "drop")

sleep_sum <- sleep_id_hour %>%
  group_by(shift_type_lab, hour) %>%
  summarise(
    mean = summ_ci(sleep)["mean"],
    lo   = summ_ci(sleep)["lo"],
    hi   = summ_ci(sleep)["hi"],
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
  geom_ribbon(data = sleep_sum, aes(x = hour, ymin = lo, ymax = hi), fill = COL_SLEEP, alpha = 0.26, colour = NA) +
  geom_line(data = sleep_sum, aes(x = hour, y = mean, colour = "Sleep"), linewidth = 1.15) +
  geom_ribbon(data = glu_sum, aes(x = hour, ymin = lo_y, ymax = hi_y), fill = COL_GLU, alpha = 0.18, colour = NA) +
  geom_line(data = glu_sum, aes(x = hour, y = mean_y, colour = "Glucose"), linewidth = 1.15) +
  facet_wrap(~ shift_type_lab, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 21, 3), limits = c(0, 23), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, 60), breaks = c(0, 20, 40, 60), expand = c(0, 0),
    name = "Sleep (min/h)", sec.axis = sec_axis(~ from60(.), name = "Glucose")
  ) +
  scale_colour_manual(values = c("Sleep" = COL_SLEEP, "Glucose" = COL_GLU)) +
  labs(x = "Hour of day (24-h clock)", colour = NULL) +
  thesis_theme()

print(p)
ggsave("outputs/Figure2B_Sleep_Glucose_24h_byShift_CI.pdf", p, width = 11, height = 6.5)
