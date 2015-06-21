get_funnel_area <- function(funnel_diameter) {
  (funnel_diameter/2)^2 * pi
}

get_pluviometer_factor <- function(funnel_area) {
  1E4 / funnel_area
}

get_precipitation_measure <- function(water_volume, pluviometer_factor) {
  water_volume_l <- water_volume / 1E3
  water_volume_l * pluviometer_factor
}

get_tank_charge <- function(precipitation, evaporation_loss, precipitation_area) {
  (precipitation - evaporation_loss) * precipitation_area
}
