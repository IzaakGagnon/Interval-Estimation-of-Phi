library(tidyverse)
library(scales)


df <- read_csv("C:\\Users\\izaak\\Downloads\\gap_bootstrap_results.csv") |>
  mutate(
    technique = factor(technique,
                       levels = c("GapBootstrap_GPD", "Robson"),
                       labels = c("Gap-Bootstrap", "Robson–Whitlock"))
  )

agg <- df |>
  group_by(technique, nominal_coverage) |>
  summarise(
    mean_cov   = mean(coverage),
    sd_cov     = sd(coverage),
    mean_width = mean(mean_interval_length),
    sd_width   = sd(mean_interval_length),
    .groups    = "drop"
  ) |>
  mutate(
    cov_lo = pmax(mean_cov   - sd_cov,   0),
    cov_hi = pmin(mean_cov   + sd_cov,   1),
    wid_lo = pmax(mean_width - sd_width, 0),
    wid_hi = mean_width + sd_width
  )

COLS  <- c("Gap-Bootstrap" = "red",     "Robson–Whitlock" = "blue")
FILLS <- c("Gap-Bootstrap" = "#FFAAAA", "Robson–Whitlock" = "#AACCFF")

theme_base <- theme_minimal(base_size = 13) +
  theme(
    legend.position      = "bottom",
    legend.title         = element_text(size = 26, face = "bold"),
    legend.text          = element_text(size = 26),
    panel.grid.minor     = element_blank(),
    plot.title           = element_text(face = "bold", size = 36),
    plot.title.position  = "plot",
    axis.title           = element_text(size = 30, face = "bold"),
    axis.text            = element_text(size = 30),
    plot.margin          = margin(t = 15, r = 30, b = 15, l = 15, unit = "pt")
  )

# ── Plot 1: Calibration ─────────────────────────────────────────
p1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "black", linewidth = 0.5) +
  geom_ribbon(
    data = agg,
    aes(x = nominal_coverage, ymin = cov_lo, ymax = cov_hi,
        fill = technique),
    alpha = 0.25
  ) +
  geom_line(
    data = agg,
    aes(x = nominal_coverage, y = mean_cov, colour = technique),
    linewidth = 0.9
  ) +
  geom_point(
    data = agg,
    aes(x = nominal_coverage, y = mean_cov, colour = technique),
    size = 2.0
  ) +
  scale_colour_manual(values = COLS,  name = "CI Method") +
  scale_fill_manual(  values = FILLS, name = "CI Method") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.6, 1, by = 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.6, 1, by = 0.1)) +
  coord_equal(xlim = c(0.59, 1.01), ylim = c(0.59, 1.01)) +
  labs(title = "Intended vs Actual Coverage",
       x     = "Intended Coverage",
       y     = "Empirical Coverage") +
  guides(colour = guide_legend(override.aes = list(fill = FILLS)),
         fill   = "none") +
  theme_base

ggsave("calibration_plot.pdf", p1, width = 10, height = 9)
ggsave("calibration_plot.png", p1, width = 10, height = 9, dpi = 200)
cat("Saved calibration_plot\n")



p2 <- ggplot() +
  geom_ribbon(
    data = agg,
    aes(x = mean_cov, ymin = wid_lo, ymax = wid_hi,
        fill = technique),
    alpha = 0.25
  ) +
  geom_line(
    data = agg,
    aes(x = mean_cov, y = mean_width, colour = technique),
    linewidth = 0.9
  ) +
  geom_point(
    data = agg,
    aes(x = mean_cov, y = mean_width, colour = technique),
    size = 2.0
  ) +
  scale_colour_manual(values = COLS,  name = "CI Method") +
  scale_fill_manual(  values = FILLS, name = "CI Method") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.8, 1, by = 0.05)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  coord_cartesian(xlim = c(0.85, 1.01)) +
  labs(title = "Empirical Coverage vs Interval Length",
       x     = "Empirical Coverage",
       y     = "Mean Interval Length") +
  guides(colour = guide_legend(override.aes = list(fill = FILLS)),
         fill   = "none") +
  theme_base

ggsave("efficiency_plot.pdf", p2, width = 11, height = 9)
ggsave("efficiency_plot.png", p2, width = 11, height = 9, dpi = 200)

