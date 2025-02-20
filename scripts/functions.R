perc_drift <- function(z, A = 2.7593, B = -0.9778) {
  A * z^B
}

#' mean_drift_dep
#'
#' Calculate mean drift deposition over a water body. This is Equation 5 of
#' focus sw (page 128, section 5.4.5). Values for A, B can be found in Appendix
#' B for focus sw. Default values are arable crop 1 app
#'
#' @param z_1 distance (m) from crop edge to near side of water body.
#' @param z_2 distance (m) from crop edge to far side of water body.
#' @param A Constant regression factor
#' @param B Exponential regression factor
#'
#' @returns mean deposition (% of application)
#'
#' @examples
mean_drift_dep <- function(z_1 = 1, z_2 = 2, A = 2.7593, B = -0.9778) {
  (A / (
    (z_2 - z_1) * (B + 1)
  )) * (z_2^(B + 1) - z_1^(B + 1))
}



#' single_banded_drift_dep
#'
#' This function calculates the mean drift deposition over a water body from a
#' single band of application. It does so by calculating the deposition from
#' a field edge broadcast applicaiton and minusing the deposition from a broadcast
#' application with a buffer the same width as the intended band width
#'
#' @param band_width Width(m) of single band where PPP is applied (parallel to ditch)
#' @param z_1 Distance (m) from edge of field to near side of water body
#' @param z_2 Distance (m) from edge of field to far side of water body
#' @param A Constant regression factor
#' @param B Exponential regression factor
#'
#' @returns average drift deposition (% of application) over water body
#' @examples
#' # Base example has a 1m wide band sprayed on edge of field with a 1m wide water body
#' # that starts 1m away from the crop
#' single_banded_drift_dep()
#'
#' # Setting band_width very high leads to same results as `mean_drift_dep`
#' single_banded_drift_dep(band_width = 1e3) # 1000m wide band
#' mean_drift_dep()
single_banded_drift_dep <- function(
    band_width = 1,
    z_1 = 1,
    z_2 = 2,
    A = 2.7593,
    B = -0.9778) {
  mean_drift_dep(z_1 = z_1, z_2 = z_2, A = A, B = B) -
    mean_drift_dep(z_1 = z_1 + band_width, z_2 = z_2 + band_width, A = A, B = B)
}


#' multi_banded_drift_dep
#'
#' Banded applications "stripe" the field with bands of 'spray' then 'no spray'
#' This function calculated average drift deposition into a water body for
#' varying width of bands ('spray') and inter bands ('no spray')
#'
#' @param band_width Width (m) of 'spray' bands
#' @param inter_band_width Width (m) of 'no spray' inter-bands
#' @param upper_limit_field_size Depth (m) of field, keep high (1000m)
#' @param z_1 Distance (m) from edge of field to near side of water body
#' @param z_2 Distance (m) from edge of field to far side of water body
#' @param A Constant regression factor
#' @param B Exponential regression factor
#'
#' @returns average drift deposition (% of application) over water body
#' @export
#'
#' @examples
#' #broadcast app, whole field
#' mean_drift_dep()
#' # 1m spray, 1m no spray in 1000m deep field, ~64% of drift from broadcast app
#' multi_banded_drift_dep()
#' #reduce size of spray band (decrease drift) ~45% of drift from broadcast app
#' multi_banded_drift_dep(band_width = 0.5)
#' #reduce sizr of 'no spray' band (increase drift), ~76%
#' multi_banded_drift_dep(inter_band_width = 0.5)
#' #add in a 5m no spray buffer around crops before starting banded app ~53%
#' mean_drift_dep(z_1 = 5+1,z_2 = 5+2)
#' multi_banded_drift_dep(z_1 = 5+1,z_2 = 5+2)
multi_banded_drift_dep <- function(band_width = 1,
                                   inter_band_width = 1,
                                   upper_limit_field_size = 1000,
                                   z_1 = 1,
                                   z_2 = 2,
                                   A = 2.7593,
                                   B = -0.9778) {

  # Because R is vectorised, we can do this in a single call to
  # `single_banded_drift_dep()`
  # the magic here is that we pass z_1 and z_2 vectors. The values in this vectors
  # represent the valid z_1 and z_2 bands for each individual band striping the
  # field.
  #  - the first values of z_1 and z_2 will be for the band right by the field edge
  #  - the second values will be the second band into the field (which will be
  #    band_width + inter_band_width further into the field)
  #  - so on until the last band which borders the other side of the field
  #    which is set to `upper_limit_field_size`
  num_bands <- upper_limit_field_size / (band_width + inter_band_width)
  single_banded_drift_dep(
    band_width = band_width,
    z_1 = z_1 + (seq_len(num_bands) - 1) * (band_width + inter_band_width),
    z_2 = z_2 + (seq_len(num_bands) - 1) * (band_width + inter_band_width),
    A = A,
    B = B
  ) |> sum()
}

multi_banded_drift_dep <- Vectorize(multi_banded_drift_dep,vectorize.args = c("band_width","inter_band_width"))




#' plot_field
#'
#' Visualise banded applications using the same variables as `multi_banded_drift_dep()`
#'
#' @param band_width Width (m) of 'spray' bands
#' @param inter_band_width Width (m) of 'no spray' inter-bands
#' @param z_1 Distance (m) from edge of field to near side of water body
#' @param z_2 Distance (m) from edge of field to far side of water body
#' @param field_width How many meters in from edge of field to plot, default 20m, smaller "zooms in"
#'
#' @returns ggplot
#' @export
#'
#' @examples
plot_field <- function(band_width = 1, inter_band_width = 1, z_1 = 1, z_2 = 2, field_width = 20) {


  ditch_position <-
    tibble::tibble(xmin = z_1, xmax = z_2, ymin = 0, ymax = 1)

  field_position <-
    tibble::tibble(xmin = -field_width, xmax = z_2, ymin = 0, ymax = 1)

  band_positions <-
    tibble::tibble(
      xmin = seq(0, -field_width, by = -(band_width + inter_band_width)) - band_width,
      xmax = seq(0, -field_width, by = -(band_width + inter_band_width)),
      ymin = 0,
      ymax = 1
    )

  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_rect(data = field_position, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "sienna", col = "sienna") +
    ggplot2::geom_rect(data = ditch_position, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "turquoise4", col = "turquoise3") +
    ggplot2::geom_rect(data = band_positions, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "seagreen", col = "darkgreen")
}

