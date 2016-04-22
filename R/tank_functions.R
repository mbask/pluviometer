#' Estimate the amount of rain flown into a tank collected on an arbitrary surface
#' 
#' Rain tanks typically collect water from house roofs. 
#'
#' @param precipitation the amount of rain fallen in mm
#' @param precipitation_area the surface area of the roof
#' @param discharge_coef The discharge coefficient determines the proportion of rainfall to the total rainfall, which is actually discharged from the roof (optional, defaults to 0.8, for an inclined hard roof, with shingles)
#'
#' @return volume of water collected (numeric, in l)
#' @export
#'
#' @examples
#' # Amount of tank charge following a 10 mm rain on a 150 m2 roof
#' get_tank_charge(10, 150)
#' # Amount of tank charge following a 10 mm rain on a 150 m2 green roof,
#' get_tank_charge(10, 150, 0.3)
get_tank_charge <- function(precipitation, 
                            precipitation_area, 
                            discharge_coef = 0.8) {
  assert_that(is.numeric(precipitation))
  assert_that(is.numeric(precipitation_area))
  assert_that(is.numeric(discharge_coef))
  assert_that(discharge_coef > 0)
  assert_that(discharge_coef <= 1)
  
  if (sum(precipitation >= 0) != length(precipitation)) {
    warning("Negative precipitation")
  }
  if (sum(precipitation_area > 0) != length(precipitation_area)) {
    warning("Zero or negative precipitation_area")
  }

  precipitation * discharge_coef * precipitation_area
}

#' Estimate the amount of rain flown into a tank collected for a given surface and discharge coefficient
#'
#' Simple wrapper around \code{get_tank_charge}
#'
#' @param precipitation_area the surface area of the roof
#' @param discharge_coef the discharge coefficient determines the proportion of rainfall to the total rainfall, which is actually discharged from the roof (optional, defaults to 0.8, for an inclined hard roof, with shingles)
#'
#' @return \code{get_tank_charge} function with arguments \code{precipitation_area} and \code{discharge_coeff} filled
#' @export
#'
#' @examples
#' rainfall_in_tank <- get_tank_charge_on_same_surface(121, 0.8)
#' rainfall_in_tank(1)
#' rainfall_in_tank(100)
get_tank_charge_on_same_surface <- function(precipitation_area, discharge_coef) {
  
  function(precipitation) {
    get_tank_charge(
      precipitation,
      precipitation_area, 
      discharge_coef)
  }
}
