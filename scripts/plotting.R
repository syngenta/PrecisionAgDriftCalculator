# Packages, data, and functions ----
library(tidyverse)

source("scripts/functions.R")
source("scripts/load_data.R")


# A first contour plot ----
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
    percentage_reduction = 100 - perc_full_field,
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
  ) +
  coord_fixed()

ggsave(
  "plots/band_vs_inter_band_width.png",
  plot = varying_band_plot,
  width = 10,
  height = 8
)


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
    percentage_reduction = 100 - perc_full_field,
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
  ) +
  coord_fixed()

ggsave(
  "plots/band_vs_inter_band_width_zoom.png",
  plot = varying_band_zoom_plot,
  width = 10,
  height = 8
)


# Contour plot for all drift curves for fixed water body ----

for (crop_group in unique(default_drift_reference_df$`Crop grouping`)) {
  full_drift <- mean_drift_dep(
    params = get_params(crop_grouping = crop_group, num_apps = 1)
  )

  varying_band <-
    expand.grid(
      band_width = seq(0.5, 3, by = 0.1),
      inter_band_width = seq(0.5, 3, by = 0.1)
    ) %>%
    mutate(
      mean_drift_loading = multi_banded_drift_dep(
        band_width = band_width,
        inter_band_width = inter_band_width,
        params = get_params(crop_grouping = crop_group, num_apps = 1)
      ),
      perc_full_field = 100 * mean_drift_loading / full_drift,
      percentage_reduction = 100 - perc_full_field,
      perc_full_field_cat = cut(perc_full_field, breaks = seq(0, 100, by = 10))
    )

  varying_band_plot <-
    varying_band %>%
    ggplot(aes(
      x = band_width,
      y = inter_band_width,
      z = percentage_reduction
    )) +
    geom_contour_filled(breaks = seq(0, 100, by = 5)) +
    theme_bw() +
    scale_x_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.25)) +
    scale_y_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.25)) +
    labs(
      title = "Percentage of drift compared to a full field application for various band and inter band widths",
      subtitle = paste0(
        crop_group,
        " assuming 1m between edge of field and water body"
      ),
      x = "Band Width (Spray) [m]",
      y = "Inter Band Width (No Spray) [m]",
      fill = "Percentage Reduction from \nFull Field Application"
    ) +
    coord_fixed()

  ggsave(
    paste0("plots/crop_group/", crop_group, "band_vs_inter_band_width.png"),
    plot = varying_band_plot,
    width = 10,
    height = 8
  )

  varying_band_zoom <-
    expand.grid(
      band_width = seq(0.1, 1, by = 0.05),
      inter_band_width = seq(0.1, 1, by = 0.05)
    ) %>%
    mutate(
      mean_drift_loading = multi_banded_drift_dep(
        band_width = band_width,
        inter_band_width = inter_band_width,
        params = get_params(crop_grouping = crop_group, num_apps = 1)
      ),
      perc_full_field = 100 * mean_drift_loading / full_drift,
      percentage_reduction = 100 - perc_full_field,
      perc_full_field_cat = cut(perc_full_field, breaks = seq(0, 100, by = 5))
    )

  varying_band_zoom_plot <-
    varying_band_zoom %>%
    ggplot(aes(
      x = band_width,
      y = inter_band_width,
      z = percentage_reduction
    )) +
    geom_contour_filled(breaks = seq(0, 100, by = 5)) +
    theme_bw() +
    scale_x_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, by = 0.1)) +
    labs(
      title = "Percentage of drift compared to a full field application for various band and inter band widths",
      subtitle = paste0(
        crop_group,
        " assuming 1m between edge of field and water body"
      ),
      x = "Band Width (Spray) [m]",
      y = "Inter Band Width (No Spray) [m]",
      fill = "Percentage Reductions from \nFull Field Application"
    ) +
    coord_fixed()

  ggsave(
    paste0(
      "plots/crop_group/",
      crop_group,
      "band_vs_inter_band_width_zoom.png"
    ),
    plot = varying_band_zoom_plot,
    width = 10,
    height = 8
  )
}


# Countour plots for FOCUS crops ----

#focus crops have a corresponding distance from the crop to the bank
#focus water bodies have a corresponding distance from the bank to the water

#We need to come up with all the combinations of crop and water body, sum those
#distances and use them to generate contour plots for each combination

focus_crop_combinations <-
  expand.grid(
    Crop = focus_sw_crop_distance$Crop,
    `water body` = focus_sw_water_body_dim$`water body`
  ) %>%
  as_tibble() %>%
  left_join(focus_sw_water_body_dim) %>%
  left_join(focus_sw_crop_distance) %>%
  mutate(
    z_1 = `distance from crop to top of bank (m)` +
      `distance from top of bank to water (m)`,
    z_2 = z_1 + `water width (m)`
  )

for (i in seq_len(nrow(focus_crop_combinations))) {
  this_param <- get_params(
    crop_grouping = focus_crop_combinations$`Crop grouping`[i],
    num_apps = 1
  )
  this_crop <- focus_crop_combinations$Crop[i]
  this_water_body <- focus_crop_combinations$`water body`[i]

  full_drift <- mean_drift_dep(
    z_1 = focus_crop_combinations$z_1[i],
    z_2 = focus_crop_combinations$z_2[i],
    params = this_param
  )

  varying_band <-
    expand.grid(
      band_width = seq(0.5, 3, by = 0.1),
      inter_band_width = seq(0.5, 3, by = 0.1)
    ) %>%
    mutate(
      mean_drift_loading = multi_banded_drift_dep(
        band_width = band_width,
        inter_band_width = inter_band_width,
        z_1 = focus_crop_combinations$z_1[i],
        z_2 = focus_crop_combinations$z_2[i],
        params = this_param
      ),
      perc_full_field = 100 * mean_drift_loading / full_drift,
      percentage_reduction = 100 - perc_full_field,
      perc_full_field_cat = cut(perc_full_field, breaks = seq(0, 100, by = 10))
    )

  varying_band_plot <-
    varying_band %>%
    ggplot(aes(
      x = band_width,
      y = inter_band_width,
      z = percentage_reduction
    )) +
    geom_contour_filled(breaks = seq(0, 100, by = 5), show.legend = TRUE) +
    scale_fill_discrete(drop = FALSE, type = scale_fill_viridis_d) +
    metR::geom_text_contour(aes(z = percentage_reduction), skip = 0) +
    theme_bw() +
    scale_x_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.25)) +
    scale_y_continuous(limits = c(0.5, 3), breaks = seq(0.5, 3, by = 0.25)) +
    labs(
      title = "Percentage of drift compared to a full field application for various band and inter band widths",
      subtitle = paste0(
        "Crop: ",
        this_crop,
        "\nWater body: FOCUS ",
        this_water_body
      ),
      x = "Band Width (Spray) [m]",
      y = "Inter Band Width (No Spray) [m]",
      fill = "Percentage Reduction from \nFull Field Application",
    ) +
    coord_fixed()

  varying_band_plot
  ggsave(
    paste0("plots/focus_crop_group/", this_crop, "_", this_water_body, ".png"),
    plot = varying_band_plot,
    width = 10,
    height = 8
  )

  varying_band_zoom <-
    expand.grid(
      band_width = seq(0.1, 1, by = 0.05),
      inter_band_width = seq(0.1, 1, by = 0.05)
    ) %>%
    mutate(
      mean_drift_loading = multi_banded_drift_dep(
        band_width = band_width,
        inter_band_width = inter_band_width,
        z_1 = focus_crop_combinations$z_1[i],
        z_2 = focus_crop_combinations$z_2[i],
        params = this_param
      ),
      perc_full_field = 100 * mean_drift_loading / full_drift,
      percentage_reduction = 100 - perc_full_field,
      perc_full_field_cat = cut(perc_full_field, breaks = seq(0, 100, by = 10))
    )

  varying_band_zoom_plot <-
    varying_band_zoom %>%
    ggplot(aes(
      x = band_width,
      y = inter_band_width,
      z = percentage_reduction
    )) +
    geom_contour_filled(breaks = seq(0, 100, by = 5), show.legend = TRUE) +
    scale_fill_discrete(drop = FALSE, type = scale_fill_viridis_d) +
    metR::geom_text_contour(aes(z = percentage_reduction), skip = 0) +
    theme_bw() +
    scale_x_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, by = 0.1)) +
    scale_y_continuous(limits = c(0.1, 1), breaks = seq(0.1, 1, by = 0.1)) +
    labs(
      title = "Percentage of drift compared to a full field application for various band and inter band widths",
      subtitle = paste0(
        "Crop: ",
        this_crop,
        "\nWater body: FOCUS ",
        this_water_body
      ),
      x = "Band Width (Spray) [m]",
      y = "Inter Band Width (No Spray) [m]",
      fill = "Percentage Reduction from \nFull Field Application"
    ) +
    coord_fixed()

  ggsave(
    paste0(
      "plots/focus_crop_group/",
      this_crop,
      "_",
      this_water_body,
      "_zoom.png"
    ),
    plot = varying_band_zoom_plot,
    width = 10,
    height = 8
  )
}
