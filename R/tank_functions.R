#' Estimate the amount of rain flown into a tank collected on an arbitrary surface
#' 
#' Rain tanks typically collect water from house roofs. The amount of roof runoff is very simple:
#' $(precipitation * precipitation_area * discharge_coef) - first_flush_diverted$
#' The discharge coefficient determines the proportion of rainfall to the total rainfall, 
#' that is actually discharged from the roof. An inclined hard roof, with shingles, assume 
#' a discharge coefficient of 80%, i.e. 0.8, for a green roof assume a a coefficient 
#' of about 30%.
#' First flush devices are designed to siphon off the first rainwater that falls on the roof. 
#' If suc a device is fitted to the rain pipes, the volume of water diverted will have 
#' to be subtracted from the roof runoff volume.
#' 
#' @param precipitation the amount of rain fallen in mm (that is l/m2)
#' @param precipitation_area the surface area of the roof (in m2)
#' @param discharge_coef the discharge coefficient (optional, defaults to 0.8)
#' @param first_flush_diverted the amount of water diverted by "first flush devices" (in l, optional, default to 0, i.e. no first flush device installed).
#'
#' @return volume of water runoff (numeric, in l)
#' @export
#'
#' @examples
#' # Amount of roof runoff following a 10 mm rain on a 150 m2 roof
#' get_roof_runoff(10, 150)
#' # Amount of roof runoff following a 10 mm rain on a 150 m2 green roof,
#' get_roof_runoff(10, 150, 0.3)
#' # Amount of roof runoff following a 10 mm rain on a 150 m2 green roof,
#' # where a first flush device that diverts 50 litres is fitted,
#' get_roof_runoff(10, 150, 0.3, 50)
get_roof_runoff <- function(precipitation, 
                            precipitation_area, 
                            discharge_coef = 0.8,
                            first_flush_diverted = 0) {
  assert_that(is.numeric(precipitation))
  assert_that(is.numeric(precipitation_area))
  assert_that(is.numeric(discharge_coef))
  assert_that(is.numeric(first_flush_diverted))
  assert_that(discharge_coef > 0)
  assert_that(discharge_coef <= 1)
  assert_that(first_flush_diverted >= 0)
  
  if (sum(precipitation >= 0) != length(precipitation)) {
    warning("Negative precipitation")
  }
  if (sum(precipitation_area > 0) != length(precipitation_area)) {
    warning("Zero or negative precipitation_area")
  }

  runoff <- (precipitation * discharge_coef * precipitation_area) - first_flush_diverted
  
  if (runoff < 0) {
    runoff = 0
  }
  
  runoff
}

#' Estimate the amount of rain flown into a tank collected for a given surface, discharge coefficient, and first flush volume diverted
#'
#' Simple wrapper around \code{get_roof_runoff}
#'
#' @param precipitation_area the surface area of the roof
#' @param discharge_coef the discharge coefficient
#' @param first_flush_diverted the amount of water diverted by "first flush devices" (in l)
#'
#' @return \code{get_roof_runoff} function with arguments \code{precipitation_area} and \code{discharge_coeff} filled
#' @export
#'
#' @examples
#' rainfall_in_tank <- get_roof_runoff_on_same_surface(121, 0.8, 45)
#' rainfall_in_tank(1)
#' rainfall_in_tank(100)
get_roof_runoff_on_same_surface <- function(precipitation_area, discharge_coef, first_flush_diverted) {
  
  function(precipitation) {
    get_roof_runoff(
      precipitation,
      precipitation_area, 
      discharge_coef,
      first_flush_diverted)
  }
}

#' Estimate the level of water in a tank after a in and out flows
#' 
#' Water level in a tank raises due to an inflow and lowers due to an outflow.
#' An exessive inflow volume may result in water draining out the tank (\code{lost_water} variable in returned \code{data.frame}).
#' The water tank needs a recharge whenever it does not guarantee the required outflow. The tank is recharged to its entire volume and \code{is_recharged} variable is set to \code{TRUE}.
#'
#' @param tank_level level of water in the tank before in/out flows
#' @param water_in  the amount of water flowing in the tank (usually from rainfall), may be \code{NA}
#' @param water_out the amount of water flowing out the tank (usually for lawn irrigation), may be \code{NA}
#' @param tank_volume the total volume of the tank
#'
#' @return a 1 row \code{data.frame} with the following variables: tank_level, water_in, is_recharged, lost_water
#' @export
#'
#' @examples
#' tank_level_l <- 2000
#' tank_vol_l <- 5000
#' # inflow = outflow, water level is unchanged
#' get_tank_water_level(tank_level_l,  500,  500, tank_vol_l)
#' # water level decreases
#' get_tank_water_level(tank_level_l,   NA, 1000, tank_vol_l)
#' # water level decreases, tank is recharged
#' get_tank_water_level(tank_level_l,    0, 2500, tank_vol_l)
#' # inflow fills up the tank, water is lost
#' get_tank_water_level(tank_level_l, 4000,  500, tank_vol_l)
get_tank_water_level <- function(tank_level,
                            water_in, 
                            water_out,
                            tank_volume) {

  assert_that(is.numeric(c(tank_level, water_in, water_out, tank_volume)))
  assert_that(tank_level  >= 0)
  assert_that(water_in    >= 0 | is.na(water_in))
  assert_that(water_out   >= 0 | is.na(water_out))
  assert_that(tank_volume  > 0)
  
  new_tank_level <- sum(c(
    tank_level, 
    water_in, 
    water_out * -1), 
    na.rm = TRUE)
  
  if (new_tank_level > tank_volume) {
    water_lost <- new_tank_level - tank_volume
    water_in   <- water_in - water_lost
  } else {
    water_lost <- 0
  }

  is_recharged <- new_tank_level < 0

  if (is_recharged | new_tank_level > tank_volume)  {
    new_tank_level <- tank_volume
  }
  
  data.frame(
    tank_level = new_tank_level, 
    water_in, 
    is_recharged, 
    water_lost)
}