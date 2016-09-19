[![Build Status](https://travis-ci.org/mbask/pluviometer.png?branch=master)](https://travis-ci.org/mbask/pluviometer)

# pluviometer
R package to aid in the estimation of precipitation amounts from a home-made pluviometer. 
Building a pluviometer is super-easy, just place a funnel ontop of a cointainer, take note of funnel diameter and, periodically (or after each rain event) note down the volume of water in the container.

The package is not yet on CRAN, to install it use `devtools` [package](https://cran.r-project.org/web/packages/devtools/index.html):

`devtools::install_github("mbask/pluviometer")`

## Usage example


```r
library(dplyr)
library(lubridate)
library(pluviometer)
library(ggplot2)
library(tidyr)
```

Let's follow a typical workflow to convert readings from home-made rain gauges (pluviometers) into precipitation figures and to estimate the recharge of a water tank connected to a house roof thanks to precipitation on the roof itself.

Having built a couple of rain gauges with different funnel diameters, let's import a couple of readings we gathered during precipitation events:


```r
precipitation_events <- read.table(
  text = "
funnel_diameter_cm water_volume_ml date_dmy
11                 13              20/06/2015
25                 329             24/06/2015
25                 100             02/07/2015
25                 200             03/07/2015
25                 700             18/07/2015",
  header = TRUE)
```


`funnel_diameter_cm` is the diameter of the circular opening where rain is captured, `water_volume_ml` is the amount of water collected in the rain gauge (either as `ml` or as `g`).

The surface area of the house roof and its discharge coefficient:

```r
rainfall_in_tank <- get_roof_runoff_on_same_surface(
  precipitation_area = 121, 
  discharge_coef     = 0.8,
  first_flush_diverted = 0)
```

Now for the core part: let's compute the amount of precipitation from the rain gauge readings for each precipitation event:


```r
precipitation_events <- precipitation_events %>%
  mutate(
    date = lubridate::dmy(date_dmy),
    rain_mm = get_precipitation_measure(
      water_volume_ml,
      funnel_diameter_cm %>% get_funnel_area), # ml o l/m2
    tank_rain_charge_l = rain_mm %>% rainfall_in_tank # l
  )
precipitation_events %>% 
  select(date, rain_mm, tank_rain_charge_l) %>% 
  print
```

```
##         date   rain_mm tank_rain_charge_l
## 1 2015-06-20  1.367943           132.4169
## 2 2015-06-24  6.702333           648.7858
## 3 2015-07-02  2.037183           197.1993
## 4 2015-07-03  4.074367           394.3987
## 5 2015-07-18 14.260283          1380.3954
```


```r
ggplot(precipitation_events, aes(x = date, y = rain_mm)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Rainfall (mm)") +
  scale_x_date("Date") +
  theme_light()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


Finally a monthly summary:


```r
monthly_precipitation <- precipitation_events %>%
  mutate(year = year(date), month = month(date, label = TRUE)) %>%
  group_by(month, year) %>%
  select(rain_mm, tank_rain_charge_l) %>%
  summarise_each(funs(sum))

print(monthly_precipitation)
```

```
## Source: local data frame [2 x 4]
## Groups: month [?]
## 
##    month  year   rain_mm tank_rain_charge_l
##   (fctr) (dbl)     (dbl)              (dbl)
## 1    Jun  2015  8.070276           781.2027
## 2    Jul  2015 20.371833          1971.9934
```

## Water balance in the water recycling tank



```r
sprinkler_flow <- 10 # l/min
sprinkler_time_min <- c(rep(10, 3), rep(5, 6)) # minutes/day
sprinkler_daily_vol_l <- Reduce(f = sum, sprinkler_time_min * sprinkler_flow) # l/day
tank_volume_l <- 5000 # l
```

A 5000 litre water tank serves an irrigation system that includes 9 sprinklers that need 10 litre/min of water each on a daily basis.

Let's now simulate 2 months of lawn irrigation (June and July 2015), eg 600 litres of water daily:


```r
water_use_tbl <- data.frame(
  date                = as.Date("2015/06/01") + 0:60,
  daily_water_usage_l = sprinkler_daily_vol_l)
```

We need to know how much rain water went into the tank in the period June-July 2015. Let's join the lawn irrigation table to the precipitation table:


```r
water_use_tbl <- water_use_tbl %>% 
  left_join(
    precipitation_events %>% 
      select(date, tank_rain_charge_l),
    by = "date")
```

When the irrigation season starts (June, 1st) the tank is full of rain water (5000) litres:


```r
water_use_tbl <- within(water_use_tbl, {
  water_balance_l    <- NA_real_
  water_balance_l[1] <- tank_volume_l
  is_recharged       <- FALSE
  rain_lost_l        <- 0
})
```

Every time the tank empties it needs to be recharged with water from the well. Let's estimate how much water we have pumped from the well, as opposed to the water we recycled from rain fallen on the house roof, on a daily level.

We will simulate a tank whose water level varies according to sprinkler water needs, water pumped from the well, and rain collected from the roof. Let's walk the `water_use_tbl` data frame to estimate the water balance on a dayly basis:


```r
for (i in seq(1, nrow(water_use_tbl), by = 1)) {
 
  if (i == 1) {
      prior_water_level_l <- water_use_tbl$water_balance_l[i]
  } else {
      prior_water_level_l <- water_use_tbl$water_balance_l[i - 1]
  } 

  tmp_df <- get_tank_water_level(
      tank_level = prior_water_level_l, 
      water_in  = water_use_tbl$tank_rain_charge_l[i], 
      water_out = water_use_tbl$daily_water_usage_l[i], 
      tank_volume = tank_volume_l)

  water_use_tbl[i,] <- within(
    water_use_tbl[i,], {
      water_balance_l = tmp_df$tank_level
      is_recharged    = tmp_df$is_recharged
      rain_lost_l     = tmp_df$water_lost
      tank_rain_charge_l = tmp_df$water_in
  })
}
```

The jaw-teeth plot of the daily water balance in the tank reveals the grim role of rainfall in the summer season as far as water budget for irrigation is concerned:

```r
water_use_tbl %>% 
  select(date, water_balance_l, rain_lost_l) %>% 
  gather(key = "variable", value = "value", -date) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  ggplot(., aes(x = date, y = value, color = variable)) + 
    geom_line() + 
    scale_y_continuous("Volume (l)") +
    scale_x_date("Date") +
    ggtitle("Balance in a water recycling tank") +
    theme_light()
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

It looks like a lot of water is pumped from the well, and occasionally rain water is lost due to the tank having been filled just days before. We also need a much bigger water tank to store rain water... 

Finally we can estimate how much water we recycled from rainfall and how much water we pumped from the well:

```r
total_water_budget <- water_use_tbl %>%
  mutate(
    tank_well_charge_l = is_recharged * tank_volume_l) %>% 
  select(
    tank_rain_charge_l, 
    tank_well_charge_l, 
    rain_lost_l) %>% 
  summarise_each(
    funs(sum(., na.rm = TRUE)))
total_water_budget %>% print
```

```
##   tank_rain_charge_l tank_well_charge_l rain_lost_l
## 1           1972.801              30000    780.3954
```

Hopefully next year won't be as dry as 2015...
