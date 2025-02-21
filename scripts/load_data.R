library(readr)

# FOCUS SW Appendix B drift parameters
default_drift_reference_df <- read_csv("data/focus_sw_drift_values.csv", show_col_types = FALSE)

focus_sw_water_body_dim <- read_csv("data/focus_sw_water_body_dimensions.csv", show_col_types = FALSE)

focus_sw_crop_distance <- read_csv("data/focus_sw_crop_distance_to_bank.csv", show_col_types = FALSE)
