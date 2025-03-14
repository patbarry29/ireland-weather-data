Notes
================

## Dataset Description

Hourly Elements:

| ID    | ELEMENT                                     | Unit      |
|-------|---------------------------------------------|-----------|
| rain  | Precipitation Amount                        | mm        |
| temp  | Air Temperature                             | °C        |
| wetb  | Wet Bulb Air Temperature                    | °C        |
| dewpt | Dew Point Air Temperature                   | °C        |
| vappr | Vapour Pressure                             | hpa       |
| rhum  | Relative Humidity                           | %         |
| msl   | Mean Sea Level Pressure                     | hPa       |
| wdsp  | Mean Hourly Wind Speed                      | kt        |
| wddir | Predominant Hourly Wind Direction           | kt        |
| ww    | Present Weather - decode below              |           |
| w     | Past Weather - decode below                 |           |
| sun   | Sunshine duration                           | hours     |
| vis   | Visibility                                  | m         |
| clht  | Cloud Ceiling Height - if none value is 999 | 100s feet |
| clamt | Cloud Amount                                | okta      |

Variable Descriptions

- Wet Bulb Air Temperature
  - The lowest temperature air can reach by evaporating water, indicates
    humidity and heat stress.
  - How much moisture the air can hold at its current temperature.
  - The drier the air, the more evaporation can occur, and the lower the
    wet-bulb temperature will be.
- Dew Point Air Temperature
  - The temperature at which air becomes saturated with moisture,
    leading to condensation.
  - A direct measure of the amount of moisture in the air.
  - The closer this is to the actual temperature, the more humid it
    feels.
- Vapour Pressure
  - The pressure exerted by water vapor in the air, showing moisture
    content.
- Relative Humidity
  - The percentage of moisture in the air compared to its maximum
    capacity at a given temperature.
- Mean Sea Level Pressure
  - The atmospheric pressure adjusted to sea level, useful for weather
    forecasting.
- Present Weather
  - The current observed weather conditions, like rain, fog, or
    sunshine.
- Past Weather
  - The observed weather conditions over the past hour.
- Sunshine Duration
  - The number of hours of sunshine during the hour of observation.
- Visibility
  - The maximum distance at which objects can be clearly seen.
- Cloud Ceiling Height
  - The altitude of the lowest cloud layer covering more than half the
    sky.
- Cloud Amount
  - The fraction of the sky covered by clouds, reported in eighths
    (oktas).

## Variable Analysis

After 2012, several sensors seem to have broken/been turned off.

- `present_weather`, `past_weather`, `sunshine_duration`, `visibility`,
  `cloud_height`, `cloud_amount` have no values after the first of April
  2012.
- I found the reason to be this:
  - “The manual weather station at Valentia was officially closed on 1
    April 2012 and was replaced by an Automatic Weather Station at the
    same location from 2 April 2012. On the same date, the number of
    daily radiosonde ascents reduced from 4 to 2.”
  - From this source:
    <https://www.met.ie/about-us/our-history/valentia-observatory>.
- To handle this, I will create two datasets, one with all data, and one
  with only data up to 2012.

## Progress

Undocumented indicator values

- several values of `0`,`1`,`5` for both `iwdsd` and `iwddir`
  - these are not specified in documentation
  - so I created factors corresponding to each value, and set these to
    `unknown_0`, `unknown_1` and `unknown_5`.
- 42 `-1` values for `irain`
  - these likely indicate missing values, so replace with NA
- 7 values of `6` in `iwb`
  - impute values using `present_weather` entry.
    - if `present_weather` code indicates rain, then I set `iwb` to
      positive and estimated
    - if `present_weather` code indicates snow, then I set `iwb` to
      negative and estimated
    - if `present_weather` code indicates fod, then I set `iwb` to Not
      Available
    - if `present_weather` code does not show a majority
      - use `wet_bulb_temp` mean

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](notes_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
