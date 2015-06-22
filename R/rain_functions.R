#' Surface area of the funnel, given its internal diameter
#'
#' The larger the funnel diameter, the more rain enters the pluviometer
#' and the more precise the readings are
#'
#' It will silently return *NA_real_* for non-numeric and <= 0 funnel diameters
#'
#' @param funnel_diameter internal diameter of the funnel (required)
#'
#' @return surface area (numeric). The unit of measure is *funnel_diameter* squared
#' @export
#'
#' @import assertthat
#' @examples
#' # Surface area of a 20 cm diameter funnel (314.1593 cm2)
#' get_funnel_area(20)
get_funnel_area <- function(funnel_diameter) {
  assert_that(is.numeric(funnel_diameter))
  if (sum(funnel_diameter > 0) != length(funnel_diameter)) {
    warning("Zero or negative funnel_diameter")
  }
  (funnel_diameter/2)^2 * pi
}

#' Get pluviometer factor
#'
#' Pluviometer factor is a property of the funnel you chose and depends on
#' its surface area. It measure the number of chosen funnels needed to
#' catch rain in 1 m2 surface area.
#'
#' @param funnel_area The internal surface area of the funnel (in cm2) (required)
#'
#' @return pluviometer factor (numeric). This is a property of the chosen funnel, its units of measure is 1/m2
#' @export
#'
#' @import assertthat
#'
#' @examples
#' # Get pluviometer factor of a funnel of 314 cm2
#' get_pluviometer_factor(314)
#' # To catch rain in 1 m2 one would need 31.84713 funnels of 314 cm2
get_pluviometer_factor <- function(funnel_area) {
  assert_that(is.numeric(funnel_area))
  if (sum(funnel_diameter > 0) != length(funnel_diameter)) {
    warning("Zero or negative funnel_area")
  }
  1E4 / funnel_area
}

#' Convert the amount of water found in the graduated cylinder into l/m2
#' 
#' The amount of water in ml (or g), is converted to l/m2 (or mm) taking into account
#' the pluviometer factor
#'
#' @param water_volume the volume (in ml) or weight (in g) of the water in the graduated cylinder (required)
#' @param pluviometer_factor the factor unique to the funnel chosen (required)
#'
#' @return standard rain measure in l/m2 or mm (numeric)
#' @export
#'
#' @examples
#' # Found 10 ml of water in a pluviometer whose factor is 31.84713
#' get_precipitation_measure(10, 31.84713)
get_precipitation_measure <- function(water_volume, pluviometer_factor) {
  water_volume_l <- water_volume / 1E3
  water_volume_l * pluviometer_factor
}
