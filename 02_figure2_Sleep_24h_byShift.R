source("R/00_helpers.R")
ensure_dir("outputs")

sleep_hourly <- load_or_stop("sleep_hourly", "data/processed/sleep_hourly.rds")

sleep_df <- sleep_hourly %>%
  mutate(
    id = toupper(trimws(as.character(id))),
    date = as.Date(date),
    hour = as.integer(hour)
  ) %>%
  with_shift_lab() %>%
  select(id, date, hour, shift_type_lab, sleep_min)

sleep_id_hour <- sleep_df %>%
  group_by(id, shift_type_lab, hour) %>%
  summarise(sleep = mean(sleep_min, na.rm = TRUE), .groups = "drop")

sleep_sum <- sleep_id_hour %>%
  group_by(shift_type_lab, hour) %>%
  summarise(
    mean = mean(sleep, na.rm = TRUE),
    sd   = sd(sleep, na.rm = TRUE),
    n    = sum(is.finite(sleep)),
    se   = sd / sqrt(pmax(n, 1)),
    lo   = mean - 1.96 * se,
    hi   = mean + 1.96 * se,
    .groups = "drop"
  )

p <- ggplot(sleep_sum, aes(x = hour, y = mean)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = COL_SLEEP, alpha = 0.26, colour = NA) +
  geom_line(colour = COL_SLEEP, linewidth = 1.15) +
  facet_wrap(~ shift_type_lab, ncol = 3) +
  scale_x_continuous(breaks = seq(0, 21, 3), limits = c(0, 23), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 20, 40, 60), expand = c(0, 0)) +
  labs(x = "Hour of day (24-h clock)", y = "Sleep (min/h)") +
  thesis_theme() +
  theme(legend.position = "none")

print(p)
ggsave("outputs/Figure1B_Sleep_24h_byShift_CI.pdf", p, width = 11, height = 6)
write.csv(sleep_sum, "outputs/Figure1B_Sleep_summary_CI.csv", row.names = FALSE)
