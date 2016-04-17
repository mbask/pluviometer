[![Build Status](https://travis-ci.org/mbask/pluviometer.png?branch=master)](https://travis-ci.org/mbask/pluviometer)

# pluviometer
R package to aid in the estimation of precipitation amounts from a home-made pluviometer. 
Building a pluviometer is super-easy, just place a funnel ontop of a cointainer, take note of funnel diameter and, periodically (or after each rain event) note down the volume of water in the container.

The package is not yet on CRAN, to install it use `devtools` [package](https://cran.r-project.org/web/packages/devtools/index.html):

`devtools::install_github("mbask/pluviometer")`

# Usage example

```r
library(dplyr)
library(lubridate)
library(pluviometer)
```

Let's follow a typical workflow to convert readings from home-made rain gauges (pluviometers) into precipitation figures and to estimate the recharge of a water tank connected to a house roof thanks to precipitation on the roof itself.

Having built a couple of rain gauges with different funnel diameters, let's import a couple of readings we gathered during precipitation events:


```r
precipitation_events <- read.table(
  text = "
funnel_diameter_cm water_volume_ml evaporation_loss_mm date_dmy
11                 13              1                   20/06/2015
25                 329             0.5                 24/06/2015
25                 100             0.5                 02/09/2015
25                 200             0.0                 03/09/2015
25                 500             0.3                 16/09/2015",
  header = TRUE)
```


`Funnel_diameter` is the diameter of the circular opening where rain is captured, `water_volume_ml` is the amount of water collected in the rain gauge (either as ml or as g), `evaporation_loss_mm` is an estimate of the water lost from the roof at the beginning of the precipitation event due to evaporation (this is water that will not flow in the tank).

The surface area of the house roof:

```r
roof_area <- 12*12 # m2
```

Now for the core parte: let's compute the amount of precipitation from the rain gauge readings for each precipitation event:


```r
precipitation_events <- precipitation_events %>%
  mutate(
    date = lubridate::dmy(date_dmy),
    funnel_area_cm2 = get_funnel_area(funnel_diameter_cm), # cm2
    pluv_factor     = get_pluviometer_factor(funnel_area_cm2), # 1/m2
    rain_mm         = get_precipitation_measure(water_volume_ml, pluv_factor), # ml o l/m2
    tank_charge_l   = get_tank_charge(rain_mm, roof_area, evaporation_loss_mm) # l
  )
print(precipitation_events %>% select(date, rain_mm, tank_charge_l))
```

```
##         date   rain_mm tank_charge_l
## 1 2015-06-20  1.367943      52.98384
## 2 2015-06-24  6.702333     893.13595
## 3 2015-09-02  2.037183     221.35439
## 4 2015-09-03  4.074367     586.70878
## 5 2015-09-16 10.185916    1423.57196
```

Finally a monthly summary:


```r
monthly_precipitation <- precipitation_events %>%
  mutate(year = year(date), month = month(date, label = TRUE)) %>%
  group_by(month, year) %>%
  select(rain_mm, tank_charge_l) %>%
  summarise_each(funs(sum))

print(monthly_precipitation)
```

```
## Source: local data frame [2 x 4]
## Groups: month [?]
## 
##    month  year   rain_mm tank_charge_l
##   (fctr) (dbl)     (dbl)         (dbl)
## 1    Jun  2015  8.070276      946.1198
## 2    Sep  2015 16.297466     2231.6351
```
