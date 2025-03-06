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
single_band_drift <- Vectorize(
  single_band_drift,
  vectorize.args = c("z_1", "z_2")
)

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

multi_banded_drift <- Vectorize(
  multi_banded_drift,
  vectorize.args = c("band_width", "inter_band_width")
)


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
    magrittr::multiply_by(band_width / (band_width + inter_band_width))
}

regular_spot_drift <- Vectorize(
  regular_spot_drift,
  vectorize.args = c("band_width", "inter_band_width")
)
