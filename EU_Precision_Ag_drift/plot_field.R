# The functions in this script create ggplot objects representing the four spray
# options of this app. In general they work by taking a `theme_void()` ggplot
# then overlaying coloured rectangles with `geom_rect()`. Plot limits are
# carefully controlled for perspective purposes.
#
# x=0 is the field edge, so x=z_2 will be the right side of the plot for far edge
# of waterbody and x=-field width will be the left side of the plot.
# y is to have a range of 0 to field_width+z_2 to keep it scaled to x
#
# For a full description of variables see `drift_calc_functions.R`

plot_full_field <- function(
  z_1 = 1,
  z_2 = 2,
  field_width = 10
) {
  #setting ymax as field width+z_2 forces y axis to be same length as x
  #which means that coord_fixed() always produces a square field
  ditch_position <-
    tibble::tibble(xmin = z_1, xmax = z_2, ymin = 0, ymax = field_width + z_2)

  #Brown background square
  field_position <-
    tibble::tibble(
      xmin = -field_width,
      xmax = z_2,
      ymin = 0,
      ymax = field_width + z_2
    )

  #green square overlayed. Note: I've called this variable `crop_position` however
  #this method can be adapted for inter_row spray
  crop_position <-
    tibble::tibble(
      xmin = -field_width,
      xmax = 0, #this one is different from field_position
      ymin = 0,
      ymax = field_width + z_2
    )

  ggplot2::ggplot() +
    ggplot2::theme_void() +
    # Brown field
    ggplot2::geom_rect(
      data = field_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "sienna",
      col = "sienna"
    ) +
    #blue ditch
    ggplot2::geom_rect(
      data = ditch_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "turquoise4",
      col = "turquoise3"
    ) +
    #green crops/spray zone
    ggplot2::geom_rect(
      data = crop_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "seagreen",
      col = "darkgreen"
    ) +
    #force graph limits to correct scale
    ggplot2::scale_x_continuous(limits = c(-field_width, z_2)) +
    ggplot2::scale_y_continuous(limits = c(0, field_width + z_2)) +
    ggplot2::coord_fixed()
}


plot_single_band_field <- function(
  band_width = 1,
  z_1 = 1,
  z_2 = 2,
  field_width = 10
) {
  #setting ymax as field width+z_2 forces y axis to be same length as x
  #which means that coord_fixed() always produces a square field
  ditch_position <-
    tibble::tibble(xmin = z_1, xmax = z_2, ymin = 0, ymax = field_width + z_2)

  field_position <-
    tibble::tibble(
      xmin = -field_width,
      xmax = z_2,
      ymin = 0,
      ymax = field_width + z_2
    )

  #single green band, starting at -band_width and ends at 0 (edge of field)
  crop_position <-
    tibble::tibble(
      xmin = -band_width,
      xmax = 0,
      ymin = 0,
      ymax = field_width + z_2
    )

  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_rect(
      data = field_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "sienna",
      col = "sienna"
    ) +
    ggplot2::geom_rect(
      data = ditch_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "turquoise4",
      col = "turquoise3"
    ) +
    ggplot2::geom_rect(
      data = crop_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "seagreen",
      col = "darkgreen"
    ) +
    ggplot2::scale_x_continuous(limits = c(-field_width, z_2)) +
    ggplot2::scale_y_continuous(limits = c(0, field_width + z_2)) +
    ggplot2::coord_fixed()
}


plot_multi_band_field <- function(
  band_width = 1,
  inter_band_width = 1,
  z_1 = 1,
  z_2 = 2,
  field_width = 10
) {
  #setting ymax as field width+z_2 forces y axis to be same length as x
  #which means that coord_fixed() always produces a square field
  ditch_position <-
    tibble::tibble(xmin = z_1, xmax = z_2, ymin = 0, ymax = field_width + z_2)

  field_position <-
    tibble::tibble(
      xmin = -field_width,
      xmax = z_2,
      ymin = 0,
      ymax = field_width + z_2
    )

  #Complicated! xmin and xmax represent the left and right edge of the bands
  #respectively. We do this by starting at the field edge (x=0) and making a
  #sequence to the field edge with interval band_width+inter_band_width.
  #We then offset the left edge of bands by the band width
  band_positions <-
    tibble::tibble(
      xmin = seq(0, -field_width, by = -(band_width + inter_band_width)) -
        band_width,
      xmax = seq(0, -field_width, by = -(band_width + inter_band_width)),
      ymin = 0,
      ymax = field_width + z_2
    )

  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_rect(
      data = field_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "sienna",
      col = "sienna"
    ) +
    ggplot2::geom_rect(
      data = ditch_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "turquoise4",
      col = "turquoise3"
    ) +
    ggplot2::geom_rect(
      data = band_positions,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "seagreen",
      col = "darkgreen"
    ) +
    ggplot2::scale_x_continuous(limits = c(-field_width, z_2)) +
    ggplot2::scale_y_continuous(limits = c(0, field_width + z_2)) +
    ggplot2::coord_fixed()
}


plot_regular_spot_field <- function(
  band_width = 1,
  inter_band_width = 1,
  z_1 = 1,
  z_2 = 2,
  field_width = 10
) {
  #setting ymax as field width+z_2 forces y axis to be same length as x
  #which means that coord_fixed() always produces a square field
  ditch_position <-
    tibble::tibble(xmin = z_1, xmax = z_2, ymin = 0, ymax = field_width + z_2)

  field_position <-
    tibble::tibble(
      xmin = -field_width,
      xmax = z_2,
      ymin = 0,
      ymax = field_width + z_2
    )

  # Complicated! similar to banded only now we have to also do y axis
  spot_position <-
    expand.grid(
      xmin = seq(0, -field_width, by = -(band_width + inter_band_width)) -
        band_width,
      ymin = seq(0, field_width + z_2, by = (band_width + inter_band_width))
    ) |>
    dplyr::mutate(
      xmax = xmin + band_width,

      ymax = ymin - band_width
    )

  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::geom_rect(
      data = field_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "sienna",
      col = "sienna"
    ) +
    ggplot2::geom_rect(
      data = ditch_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "turquoise4",
      col = "turquoise3"
    ) +
    ggplot2::geom_rect(
      data = spot_position,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = "seagreen",
      col = "darkgreen"
    ) +
    ggplot2::scale_x_continuous(limits = c(-field_width, z_2)) +
    ggplot2::scale_y_continuous(limits = c(0, field_width + z_2)) +
    ggplot2::coord_fixed()
}
