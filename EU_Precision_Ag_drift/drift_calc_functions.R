#' full_field_drift
#'
#' Calculates the SW loading using the Rautmann 2001 equations acording to FOCUS
#' SW. Full explanation can be found in section 5.4 of the FOCUS SW final report
#' and the parameters can be found in Appendix B. Both can be obtained from the
#' FOCUS website \href{https://esdac.jrc.ec.europa.eu/projects/surface-water}{here}
#'
#' @param z_1 distance from crop to near edge of waterbody
#' @param z_2 distance from crop to far edge of waterbody
#' @param A vertical scaling in regression curve (pre hinge)
#' @param B horizontal scaling in regresion curve (pre hinge)
#' @param C vertical scaling in regression curve (post hinge, if hinge used)
#' @param D horizontal scaling in regression curve (post hinge, if hinge used)
#' @param H hinge point (if used)
#'
#' @returns numeric representing percentage of application rate as drift entry to
#' SW body
#' @examples
#' # First run the app.R script to obtain the variable
#' #`focus_crop_combinations`. You can check that this example creates the same
#' # drift percentage as the SWASH Drift Calculator
#' focus_crop_combinations %>%
#'   filter(
#'     Crop == "cereals (not maize)",
#'     `water body` == "ditch",
#'     NumApps == 1
#'   ) %>%
#'   with(., {
#'     full_field_drift(
#'       z_1,
#'       z_2,
#'       A,
#'       B,
#'       C,
#'       D,
#'       H
#'     )
#'   })
#' #1.927392
full_field_drift <- function(
  z_1,
  z_2,
  A,
  B,
  C,
  D,
  H
) {
  if (dplyr::between(H, z_1, z_2)) {
    (((H^(B + 1) - z_1^(B + 1)) * A / (B + 1)) +
      ((z_2^(D + 1) - H^(D + 1)) * C / (D + 1))) /
      (z_2 - z_1)
  } else if (H < z_1) {
    (C / ((z_2 - z_1) * (D + 1))) * (z_2^(D + 1) - z_1^(D + 1))
  } else {
    (A / ((z_2 - z_1) * (B + 1))) * (z_2^(B + 1) - z_1^(B + 1))
  }
}

#' single_band_drift
#'
#' Calculates the drift from a single band of application by taking away the
#' full field drift from the far side (from the SW body) of the band from the
#' full field drift from the near side of the band
#'
#' @param band_width width of band
#' @param z_1 distance from crop to near edge of waterbody
#' @param z_2 distance from crop to far edge of waterbody
#' @param A vertical scaling in regression curve (pre hinge)
#' @param B horizontal scaling in regresion curve (pre hinge)
#' @param C vertical scaling in regression curve (post hinge, if hinge used)
#' @param D horizontal scaling in regression curve (post hinge, if hinge used)
#' @param H hinge point (if used)
#'
#' @returns numeric, percentage drift for single band
#' @examples
#' # First run the app.R script to obtain the variable
#' #`focus_crop_combinations`. You can check that this example creates the same
#' # drift percentage as the SWASH Drift Calculator
#' focus_crop_combinations %>%
#'   filter(
#'     Crop == "cereals (not maize)",
#'     `water body` == "ditch",
#'     NumApps == 1
#'   ) %>%
#'   with(., {
#'     single_band_drift(
#'       band_width = 1, #1m band
#'       z_1,
#'       z_2,
#'       A,
#'       B,
#'       C,
#'       D,
#'       H
#'     )
#'   })
#' #0.7861145
single_band_drift <- function(
  band_width,
  z_1,
  z_2,
  A,
  B,
  C,
  D,
  H
) {
  full_field_drift(
    z_1 = z_1,
    z_2 = z_2,
    A = A,
    B = B,
    C = C,
    D = D,
    H = H
  ) -
    full_field_drift(
      z_1 = z_1 + band_width,
      z_2 = z_2 + band_width,
      A = A,
      B = B,
      C = C,
      D = D,
      H = H
    )
}
#this converts the above function to a vectorised version that can be called for
#multiple z_1 and z_2. This is used in `multi_banded_drift` to avoid a for loop
single_band_drift <- Vectorize(
  single_band_drift,
  vectorize.args = c("z_1", "z_2")
)

#' multi_banded_drift
#'
#' Calculates the SW deposition from a field with many bands (ie, a full banded
#' application). It does this by calculating drift for each band then summing
#' them.
#'
#' @param band_width width of Spray band (m)
#' @param inter_band_width width on "No Spray" band (m)
#' @param upper_limit_field_size practical variable for upper limit of field
#' depth (m). This is used to calculate how many bands are in the field. By
#' default it's 1km
#' @param z_1 distance from crop to near edge of waterbody
#' @param z_2 distance from crop to far edge of waterbody
#' @param A vertical scaling in regression curve (pre hinge)
#' @param B horizontal scaling in regresion curve (pre hinge)
#' @param C vertical scaling in regression curve (post hinge, if hinge used)
#' @param D horizontal scaling in regression curve (post hinge, if hinge used)
#' @param H hinge point (if used)
#'
#' @returns percent of application rate loaded as spray drift
#' @examples
#' # First run the app.R script to obtain the variable
#' #`focus_crop_combinations`. You can check that this example creates the same
#' # drift percentage as the SWASH Drift Calculator
#' focus_crop_combinations %>%
#'   filter(
#'     Crop == "cereals (not maize)",
#'     `water body` == "ditch",
#'     NumApps == 1
#'   ) %>%
#'   with(., {
#'     multi_banded_drift(
#'       band_width = 1, #1m spray band,
#'       inter_band_width = 1, #1m no spray band,
#'       upper_limit_field_size = 1000,
#'       z_1,
#'       z_2,
#'       A,
#'       B,
#'       C,
#'       D,
#'       H
#'     )
#'   })
#' #1.248173
multi_banded_drift <- function(
  band_width = 1,
  inter_band_width = 1,
  upper_limit_field_size = 1000,
  z_1 = 1,
  z_2 = 2,
  A,
  B,
  C,
  D,
  H
) {
  # Because R is vectorised, we can do this in a single call to
  # `single_band_drift()`
  # the magic here is that we pass z_1 and z_2 vectors. The values in this vectors
  # represent the valid z_1 and z_2 bands for each individual band striping the
  # field.
  #  - the first values of z_1 and z_2 will be for the band right by the field edge
  #  - the second values will be the second band into the field (which will be
  #    band_width + inter_band_width further into the field)
  #  - so on until the last band which borders the other side of the field
  #    which is set to `upper_limit_field_size`
  num_bands <- upper_limit_field_size / (band_width + inter_band_width)
  single_band_drift(
    band_width = band_width,
    z_1 = z_1 + (seq_len(num_bands) - 1) * (band_width + inter_band_width),
    z_2 = z_2 + (seq_len(num_bands) - 1) * (band_width + inter_band_width),
    A = A,
    B = B,
    C = C,
    D = D,
    H = H
  ) |>
    sum()
}

# Vectorise
multi_banded_drift <- Vectorize(
  multi_banded_drift,
  vectorize.args = c("band_width", "inter_band_width")
)


#' regular_spot_drift
#'
#' Basically the same as banded drift only the drift from each band is scaled
#' to what percentage of the band is sprayed. This function assumes the regular
#' spot application is square. It would be very easy to adjust the function
#' for regular spot with rectangular spots
#'
#' @param band_width width of Spray band (m)
#' @param inter_band_width width on "No Spray" band (m)
#' @param upper_limit_field_size practical variable for upper limit of field
#' depth (m). This is used to calculate how many bands are in the field. By
#' default it's 1km
#' @param z_1 distance from crop to near edge of waterbody
#' @param z_2 distance from crop to far edge of waterbody
#' @param A vertical scaling in regression curve (pre hinge)
#' @param B horizontal scaling in regresion curve (pre hinge)
#' @param C vertical scaling in regression curve (post hinge, if hinge used)
#' @param D horizontal scaling in regression curve (post hinge, if hinge used)
#' @param H hinge point (if used)
#'
#' @returns percent of application rate loaded as spray drift
#' @examples
#' # First run the app.R script to obtain the variable
#' #`focus_crop_combinations`. You can check that this example creates the same
#' # drift percentage as the SWASH Drift Calculator
#' focus_crop_combinations %>%
#'   filter(
#'     Crop == "cereals (not maize)",
#'     `water body` == "ditch",
#'     NumApps == 1
#'   ) %>%
#'   with(., {
#'     regular_spot_drift(
#'       band_width = 1, #1m spray band,
#'       inter_band_width = 1, #1m no spray band,
#'       upper_limit_field_size = 1000,
#'       z_1,
#'       z_2,
#'       A,
#'       B,
#'       C,
#'       D,
#'       H
#'     )
#'   })
#' #0.6240865
regular_spot_drift <- function(
  band_width = 1,
  inter_band_width = 1,
  upper_limit_field_size = 1000,
  z_1 = 1,
  z_2 = 2,
  A,
  B,
  C,
  D,
  H
) {
  num_bands <- upper_limit_field_size / (band_width + inter_band_width)
  single_band_drift(
    band_width = band_width,
    z_1 = z_1 + (seq_len(num_bands) - 1) * (band_width + inter_band_width),
    z_2 = z_2 + (seq_len(num_bands) - 1) * (band_width + inter_band_width),
    A = A,
    B = B,
    C = C,
    D = D,
    H = H
  ) |>
    sum() |>
    #regular spot modification.
    magrittr::multiply_by(band_width / (band_width + inter_band_width))
}

regular_spot_drift <- Vectorize(
  regular_spot_drift,
  vectorize.args = c("band_width", "inter_band_width")
)
