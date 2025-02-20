library(tidyverse)

source("scripts/functions.R")
full_drift <- mean_drift_dep()

varying_band <-
  expand.grid(
    band_width = seq(0.5, 3, by = 0.1),
    inter_band_width = seq(0.5, 3, by = 0.1)
  ) %>%
  mutate(
    mean_drift_loading = multi_banded_drift_dep(
      band_width = band_width,
      inter_band_width = inter_band_width
    ),
    perc_full_field = 100 * mean_drift_loading / full_drift,
    percentage_reduction=100-perc_full_field,
    perc_full_field_cat = cut(perc_full_field, breaks = seq(0, 100, by = 10))
  )

varying_band_plot <-
varying_band %>%
  ggplot(aes(x = band_width, y = inter_band_width, z = percentage_reduction)) +
  geom_contour_filled(breaks = seq(0, 100, by = 5)) +
  theme_bw() +
  scale_x_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.25)) +
  scale_y_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.25)) +
  labs(
    title = "Percentage of drift compared to a full field application for various band and inter band widths",
    subtitle = "FOCUS SW Arable crop, assuming 1m between edge of field and water body",
    x = "Band Width (Spray) [m]",
    y = "Inter Band Width (No Spray) [m]",
    fill = "Percentage Reduction from \nFull Field Application"
  )+
  coord_fixed()

ggsave("plots/band_vs_inter_band_width.png",plot = varying_band_plot, width = 10, height = 8)




varying_band_zoom <-
  expand.grid(
    band_width = seq(0.1, 1, by = 0.01),
    inter_band_width = seq(0.1, 1, by = 0.01)
  ) %>%
  mutate(
    mean_drift_loading = multi_banded_drift_dep(
      band_width = band_width,
      inter_band_width = inter_band_width
    ),
    perc_full_field = 100 * mean_drift_loading / full_drift,
    percentage_reduction=100-perc_full_field,
    perc_full_field_cat = cut(perc_full_field, breaks = seq(0, 100, by = 5))
  )

varying_band_zoom_plot <-
varying_band_zoom %>%
  ggplot(aes(x = band_width, y = inter_band_width, z = percentage_reduction)) +
  geom_contour_filled(breaks = seq(0, 100, by = 5)) +
  theme_bw() +
  scale_x_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, by = 0.1)) +
  scale_y_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, by = 0.1)) +
  labs(
    title = "Percentage of drift compared to a full field application for various band and inter band widths",
    subtitle = "FOCUS SW Arable crop, assuming 1m between edge of field and water body",
    x = "Band Width (Spray) [m]",
    y = "Inter Band Width (No Spray) [m]",
    fill = "Percentage Reductions from \nFull Field Application"
  )+
  coord_fixed()

ggsave("plots/band_vs_inter_band_width_zoom.png",plot = varying_band_zoom_plot, width = 10, height = 8)
