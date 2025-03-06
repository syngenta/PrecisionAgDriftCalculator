plot_full_field <- function(
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

  crop_position <-
    tibble::tibble(
      xmin = -field_width,
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
