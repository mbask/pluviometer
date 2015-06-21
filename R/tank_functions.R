get_tank_charge <- function(precipitation, evaporation_loss, precipitation_area) {
  charge <- (precipitation - evaporation_loss) * precipitation_area
  ifelse(charge > 0, charge, 0)
}
