get_tank_charge <- function(precipitation, evaporation_loss, precipitation_area) {
  (precipitation - evaporation_loss) * precipitation_area
}
