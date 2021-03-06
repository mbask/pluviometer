% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tank_functions.R
\name{get_tank_water_level}
\alias{get_tank_water_level}
\title{Estimate the level of water in a tank after a in and out flows}
\usage{
get_tank_water_level(tank_level, water_in, water_out, tank_volume)
}
\arguments{
\item{tank_level}{level of water in the tank before in/out flows}

\item{water_in}{the amount of water flowing in the tank (usually from rainfall), may be \code{NA}}

\item{water_out}{the amount of water flowing out the tank (usually for lawn irrigation), may be \code{NA}}

\item{tank_volume}{the total volume of the tank}
}
\value{
a 1 row \code{data.frame} with the following variables: tank_level, water_in, is_recharged, lost_water
}
\description{
Water level in a tank raises due to an inflow and lowers due to an outflow.
An exessive inflow volume may result in water draining out the tank (\code{lost_water} variable in returned \code{data.frame}).
The water tank needs a recharge whenever it does not guarantee the required outflow. The tank is recharged to its entire volume and \code{is_recharged} variable is set to \code{TRUE}.
}
\examples{
tank_level_l <- 2000
tank_vol_l <- 5000
# inflow = outflow, water level is unchanged
get_tank_water_level(tank_level_l,  500,  500, tank_vol_l)
# water level decreases
get_tank_water_level(tank_level_l,   NA, 1000, tank_vol_l)
# water level decreases, tank is recharged
get_tank_water_level(tank_level_l,    0, 2500, tank_vol_l)
# inflow fills up the tank, water is lost
get_tank_water_level(tank_level_l, 4000,  500, tank_vol_l)
}

