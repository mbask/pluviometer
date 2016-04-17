#' Estimate the amount of rain flown into a tank collected on an arbitrary surface
#' 
#' Rain tanks typically collect water from house roofs. 
#'
#' @param precipitation the amount of rain fallen in mm
#' @param precipitation_area the surface area of the roof
#' @param evaporation_loss mm of rain lost to evaporation on the roof (optional, defaults to 0)
#'
#' @return volume of water collected (numeric, in l)
#' @export
#'
#' @examples
#' # Amount of tank charge following a 10 mm rain on a 150 m2 roof, no evaporation
#' get_tank_charge(10, 150)
#' # Amount of tank charge following a 10 mm rain on a 150 m2 roof,
#' # assuming 2 mm of rain evaporated on hot roof
#' get_tank_charge(10, 150, 2)
get_tank_charge <- function(precipitation, 
                            precipitation_area, 
                            evaporation_loss = 0) {
  assert_that(is.numeric(precipitation))
  assert_that(is.numeric(precipitation_area))
  assert_that(is.numeric(evaporation_loss))

  if (sum(precipitation >= 0) != length(precipitation)) {
    warning("Negative precipitation")
  }
  if (sum(precipitation_area > 0) != length(precipitation_area)) {
    warning("Zero or negative precipitation_area")
  }
  if (sum(evaporation_loss >= 0) != length(evaporation_loss)) {
    warning("Negative evaporation_loss")
  }

  charge <- (precipitation - evaporation_loss) * precipitation_area
  ifelse(charge > 0, charge, 0)
}
