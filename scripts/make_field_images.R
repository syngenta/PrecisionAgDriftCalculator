save_field <- function(x, name = "fullfield") {
  ggsave(
    x,
    filename = fs::path("field_images", name, ext = "png"),
    height = 10,
    width = 10
  )
}

source("EU_Precision_Ag_drift/plot_field.R")


# FOCUS SW Appendix B drift parameters
default_drift_reference_df <- read_csv(
  "EU_Precision_Ag_drift/data/focus_sw_drift_values.csv",
  show_col_types = FALSE
)

focus_sw_water_body_dim <- read_csv(
  "EU_Precision_Ag_drift/data/focus_sw_water_body_dimensions.csv",
  show_col_types = FALSE
)

focus_sw_crop_distance <- read_csv(
  "EU_Precision_Ag_drift/data/focus_sw_crop_distance_to_bank.csv",
  show_col_types = FALSE
)


plot_full_field(field_width = 10) %>% save_field("full_field")
plot_full_field(field_width = 10 - 1, z_1 = 2, z_2 = 3) %>%
  save_field("full_field_offset1m")
plot_single_band_field(field_width = 10) %>% save_field("single_band")

plot_multi_band_field() %>% save_field("multi_band")
plot_regular_spot_field() %>% save_field("regular_spot")
