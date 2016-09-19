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
#' @importFrom assertthat assert_that
#' @examples
#' # Surface area of a 20 cm diameter funnel (314.1593 cm2)
#' get_funnel_area(20)
get_funnel_area <- function(funnel_diameter) {
  assert_that(is.numeric(funnel_diameter))
  if (sum(funnel_diameter > 0) != length(funnel_diameter)) {
    warning("Zero or negative funnel_diameter")
  }
  (funnel_diameter / 2) ^ 2 * pi
}

#' Get pluviometer factor
#'
#' Pluviometer factor is a property of the funnel you chose and depends on
#' its surface area. It measure the number of chosen funnels needed to
#' catch rain in 1 m2 surface area.
#'
#' @param funnel_area The internal surface area of the funnel (in cm2) (required)
#'
#' @return pluviometer factor (numeric). This is a property of the funnel, its units of measure is 1/m2
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' # Get pluviometer factor of a funnel of 314 cm2
#' get_pluviometer_factor(314)
#' # To catch rain in 1 m2 one would need 31.84713 funnels of 314 cm2
get_pluviometer_factor <- function(funnel_area) {
  assert_that(is.numeric(funnel_area))
  if (sum(funnel_area > 0) != length(funnel_area)) {
    warning("Zero or negative funnel_area")
  }
  1E4 / funnel_area
}

#' Convert the amount of water found in the graduated cylinder into l/m2
#' 
#' The amount of water in ml (or g), is converted to l/m2 (or mm) taking into account
#' the pluviometer factor.
#' The function may be used to get the amount of water (in ml) one should find in the cylinder, 
#' given the amount of precipitation. This may be useful to mark an ungraduated container with 
#' precipitation (mm or l/m2) levels.
#'
#' @param water_volume the volume (in ml) or weight (in g) of the water in the graduated cylinder
#' @param funnel_area  the internal surface area of the funnel (in cm2) (required)
#' @param rain_volume  the volume (in mm or l/m2) of precipitation, default to \code{NULL}
#'
#' @return standard rain measure in l/m2 or mm (numeric) when \code{rain_volume} is \code{NULL}, 
#' water volume in ml (numeric) when \code{water_volume} is not numeric
#' 
#' @export
#' @importFrom assertthat assert_that
#' @examples
#' # Found 10 ml of water in a pluviometer whose surface area is 20 cm^2
#' get_precipitation_measure(10, 20)
#' \dontrun{
#' # Weight of empty container: 856g
#' # Weight of container with rainwater: 925g
#' # Diameter of funnel: 19.6 cm
#' 19.6  %>% 
#'  get_funnel_area %>% 
#'  get_precipitation_measure(925-856, .)
#' # Estimate sprinkler flow on a 5 m radius area, switched
#' # on for 10 minutes, result in l/m
#' 19.6  %>% 
#'  get_funnel_area %>% 
#'  get_precipitation_measure(925-856, .) %>% 
#'  multiply_by(pi * 5^2) %>% 
#'  divide_by(10)
#'  # Inverse calculation
#'  # How to mark an ungraduated 1 litre cylinder for every 2 mm of rain?
#'  get_precipitation_measure(
#'    water_volume = NULL, 
#'    get_funnel_area(19.6 %>% get_funnel_area), 
#'    rain_volume = seq(2, 34, 2))
#'  }
get_precipitation_measure <- function(water_volume, funnel_area, rain_volume = NULL) {
  assert_that(is.numeric(water_volume) | is.numeric(rain_volume))

  pluviometer_factor <- get_pluviometer_factor(funnel_area)

  if (is.numeric(water_volume)) {
    if (sum(water_volume >= 0) != length(water_volume)) {
      warning("Negative water_volume")
    }
    message("Rain in mm")
    water_volume_l <- water_volume / 1E3
    water_volume_l * pluviometer_factor
  } else {
    if (is.numeric(rain_volume)) {
      if (sum(rain_volume >= 0) != length(rain_volume)) {
        warning("Negative rain_volume")
      }
      message("Container volume in ml")
      water_volume_l <- rain_volume / pluviometer_factor
      water_volume_l * 1E3
    } else {
      warning("Either ask for precipitation measure (given water_volume) or water_volume (given rain_volume)")
    }
  } 
}
