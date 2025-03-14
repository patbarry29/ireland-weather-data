library("lubridate")
library(dplyr)

pathname <- "/Users/pb/Documents/university/ujm/sem2/data-mining/project/ireland-weather-data/data/valentia_observatory.csv"
weather_data <- read.csv(pathname, stringsAsFactors = FALSE)

str(weather_data)

# CONVERT date VARIABLE TO CORRECT FORMAT
weather_data$date <- ymd_hms(weather_data$date)

str(weather_data)

# ADD MORE INTERPRETABLE COLUMN NAMES
column_names <- c(
  X = "id",                    # Station ID
  date = "date",               # Original date string
  ind = "irain",               # Rainfall indicator
  rain = "precipitation",      # Precipitation Amount (mm)
  ind.1 = "itemp",             # Temperature indicator
  temp = "temperature",        # Air Temperature (°C)
  ind.2 = "iwb",               # Wet Bulb indicator
  wetb = "wet_bulb_temp",      # Wet Bulb Air Temperature (°C)
  dewpt = "dew_point",         # Dew Point Air Temperature (°C)
  vappr = "vapor_pressure",    # Vapour Pressure (hPa)
  rhum = "relative_humidity",  # Relative Humidity (%)
  msl = "sea_level_pressure",  # Mean Sea Level Pressure (hPa)
  ind.3 = "iwdsp",             # Wind Speed indicator
  wdsp = "wind_speed",         # Mean Hourly Wind Speed (kt)
  ind.4 = "iwddir",            # Wind Direction indicator
  wddir = "wind_direction",    # Predominant Hourly wind Direction
  ww = "present_weather",      # Present Weather SYNOP code
  w = "past_weather",          # Past Weather SYNOP code
  sun = "sunshine_duration",   # Sunshine duration (hours)
  vis = "visibility",          # Visibility (m)
  clht = "cloud_height",       # Cloud Ceiling Height (100s feet)
  clamt = "cloud_amount"       # Cloud Amount (okta)
)
names(weather_data) <- column_names

# INSPECT THE OVERALL DATA
str(weather_data)
head(weather_data)
summary(weather_data)

# CHECK FOR MISSING VALUES
missing_values <- sapply(weather_data, function(x) sum(is.na(x)))
print("Columns with missing values:")
print(missing_values[missing_values > 0])

# SHOW DISTRIBUTION OF MISSING VALUES BY YEAR
columns_with_missing <- names(missing_values[missing_values > 0])
for (col in columns_with_missing) {
  cat("\nMissing values in", col, ":\n")
  missing_by_year <- aggregate(is.na(weather_data[[col]]),
                 by=list(year=year(weather_data$date)),
                 FUN=sum)
  print(missing_by_year)
}

# SHOW LAST VALID OBSERVATION OF present_weather
last_valid_date <- max(weather_data$date[!is.na(weather_data$present_weather)])
cat("Last valid present_weather observation:", as.character(last_valid_date), "\n")


# --- DECODE INDICATORS ---
# irain
print("Distribution of irain (ind)")
table(weather_data$irain)

# convert -1 values to na
weather_data$irain[weather_data$irain == -1] <- NA
print("Updated distribution of irain:")
table(weather_data$irain, useNA = "ifany")


# itemp
print("Distribution of itemp (ind.1)")
table(weather_data$itemp)


# iwb
print("Distribution of iwb (ind.2)")
table(weather_data$iwb)

weather_data %>% filter(iwb == 6) %>% select(present_weather) %>% table()

weather_data <- weather_data %>%
  mutate(iwb = case_when(
    iwb == 6 & present_weather %in% c(21, 25, 60:69, 80:82, 91, 92) ~ 2, # rain so iwb likely positive and estimated
    iwb == 6 & present_weather %in% c(22, 23, 26, 27, 70:79, 83:87, 93, 94) ~ 3, # snow so iwb likely negative and estimated
    iwb == 6 & present_weather %in% c(28, 40:49) ~ 4, # fog so we put not available
    iwb == 6 & mean(wet_bulb_temp[iwb == 6], na.rm = TRUE) > 0 ~ 2, # mean wet bulb temperature > 0
    iwb == 6 & mean(wet_bulb_temp[iwb == 6], na.rm = TRUE) <= 0 ~ 3, # mean wet bulb temperature <= 0
    TRUE ~ iwb
  ))
table(weather_data$iwb)

# iwdsp
print("Distribution of iwdsp (ind.3)")
table(weather_data$iwdsp)

# iwddir
print("Distribution of iwddir (ind.4)")
table(weather_data$iwddir)

# CONVERT ALL TO FACTORS
weather_data <- weather_data %>%
  mutate(
    # irain (Rainfall Indicator)
    irain_factor = factor(irain, levels = c(0, 1, 2, 3, 4, 5, 6, 7, NA),
               labels = c("Satisfactory", "Deposition", "Trace/Sum_Precip",
                    "Trace/Sum_Deposition", "Estimate_Precip", "Estimate_Deposition",
                    "Estimate_Trace_Precip", "Unknown_7", "Missing"),
               exclude = NULL),

    # itemp (Temperature Indicator)
    itemp_factor = factor(itemp, levels = c(0, 1, 2, 3, 4),
                         labels = c("Positive", "Negative", "Positive_Estimated",
                                    "Negative_Estimated", "Not_Available")),

    # iwb (Wet Bulb Indicator)
    iwb_factor = factor(iwb, levels = c(0, 1, 2, 3, 4, 5),
                       labels = c("Positive", "Negative", "Positive_Estimated",
                                  "Negative_Estimated", "Not_Available", "Frozen_Negative")),


    # iwdsp (Wind Speed Indicator)
    iwdsp_factor = factor(iwdsp, levels = c(0, 1, 2, 4, 5, 6, 7),
                          labels = c("Unknown_0", "Unknown_1", "Over_60", "Over_60_Defective",
                                     "Unknown_5", "Over_60_Partial", "N/A")),

    # iwddir (Wind Direction Indicator)
    iwddir_factor = factor(iwddir, levels = c(0, 1, 2, 4, 5, 6, 7),
                           labels = c("Unknown_0", "Unknown_1", "Over_60", "Over_60_Defective",
                                      "Unknown_5", "Over_60_Partial", "N/A"))
  )

weather_data <- weather_data %>%
   select(-irain, -itemp, -iwb, -iwdsp, -iwddir)

str(weather_data)

table(weather_data$irain_factor, useNA = "ifany")

# -- EXTRACT NEW DATE FEATURES ---
weather_data <- weather_data %>%
  mutate(
    hour = hour(date),
    day_of_week = wday(date, week_start = 1),
    day_of_year = yday(date),
    month = month(date),
    year = year(date),
    week_of_year = week(date)
  )

select(weather_data, hour:week_of_year)

head(weather_data)

# --- CREATE TWO DATASETS ---
# 1. weather_data_pre_2012: Contains all data before April 2nd 2012
weather_data_pre_2012 <- weather_data %>%
  filter(date < ymd("2012-04-02"))

# 2. weather_data_post_2012: Contains all data after April 2nd 2012
weather_data_post_2012 <- weather_data %>%
  filter(date >= ymd("2012-04-02"))

weather_data_post_2012 <- weather_data_post_2012 %>%
  select(-present_weather, -past_weather, -sunshine_duration, -visibility, -cloud_height, -cloud_amount)

head(weather_data_post_2012)

# --- DECODE WEATHER CODES ---
print("Distribution of present_weather (ww)")
table(weather_data_pre_2012$present_weather, useNA = "ifany")
print("Distribution of past_weather (w)")
table(weather_data_pre_2012$past_weather, useNA = "ifany")

weather_data_pre_2012 <- weather_data_pre_2012 %>%
  mutate(
    present_weather_category = case_when(
      present_weather >= 0  & present_weather <= 19 ~ "No Precipitation",
      present_weather == 20 ~ "Drizzle",
      present_weather == 21 ~ "Rain",
      present_weather == 22 ~ "Snow",
      present_weather == 23 ~ "Rain/Snow",
      present_weather == 24 ~ "Freezing Rain/Drizzle",
      present_weather == 25 ~ "Rain Showers",
      present_weather == 26 ~ "Snow Showers",
      present_weather == 27 ~ "Hail Showers",
      present_weather == 28 ~ "Fog/Ice Fog",
      present_weather == 29 ~ "Thunderstorm",
      between(present_weather, 30, 39) ~ "Dust/Sand/Snowstorm",
      between(present_weather, 40, 49) ~ "Fog/Ice Fog",
      between(present_weather, 50, 59) ~ "Drizzle",
      between(present_weather, 60, 69) ~ "Rain",
      between(present_weather, 70, 79) ~ "Snow",
      between(present_weather, 80, 89) ~ "Showers",
      between(present_weather, 90, 99) ~ "Thunderstorm",
      TRUE ~ "Other"
    ),
    present_weather_intensity = case_when(
      present_weather %in% c(0:19) ~ "None",
      present_weather %in% c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                             30, 36, 38, 50, 51, 56, 58, 60, 61, 66, 68,
                             70, 71, 76, 77, 78, 79, 80, 83, 85, 87, 89,
                             91, 93, 95) ~ "Light",
      present_weather %in% c(31, 37, 39, 52, 53, 57, 59, 62, 63, 67, 69,
                             72, 73, 81, 84, 86, 88, 90, 92, 94, 96) ~ "Moderate",
      present_weather %in% c(32, 34, 35, 54, 55, 64, 65, 74, 75, 82, 97) ~ "Heavy",
      present_weather %in% c(98, 99) ~ "Extreme",
      TRUE ~ NA_character_
    ),
    past_weather_category = case_when(
      past_weather >= 0  & past_weather <= 19 ~ "No Precipitation",
      past_weather == 20 ~ "Drizzle",
      past_weather == 21 ~ "Rain",
      past_weather == 22 ~ "Snow",
      past_weather == 23 ~ "Rain/Snow",
      past_weather == 24 ~ "Freezing Rain/Drizzle",
      past_weather == 25 ~ "Rain Showers",
      past_weather == 26 ~ "Snow Showers",
      past_weather == 27 ~ "Hail Showers",
      past_weather == 28 ~ "Fog/Ice Fog",
      past_weather == 29 ~ "Thunderstorm",
      between(past_weather, 30, 39) ~ "Dust/Sand/Snowstorm",
      between(past_weather, 40, 49) ~ "Fog/Ice Fog",
      between(past_weather, 50, 59) ~ "Drizzle",
      between(past_weather, 60, 69) ~ "Rain",
      between(past_weather, 70, 79) ~ "Snow",
      between(past_weather, 80, 89) ~ "Showers",
      between(past_weather, 90, 99) ~ "Thunderstorm",
      TRUE ~ "Other"
    ),

    present_weather_category = factor(present_weather_category),
    present_weather_intensity = factor(present_weather_intensity),
    past_weather_category = factor(past_weather_category)
  )


table(weather_data_pre_2012$present_weather_category)
table(weather_data_pre_2012$present_weather_intensity)
table(weather_data_pre_2012$past_weather_category)

# --- DECODE CLOUD HEIGHT ---
table(weather_data_pre_2012$cloud_height)

weather_data_pre_2012 <- weather_data_pre_2012 %>%
  mutate(
    has_cloud_ceiling = ifelse(cloud_height != 999, 1, 0),
    cloud_height = ifelse(cloud_height != 999, cloud_height, NA_real_)
  )

table(weather_data_pre_2012$cloud_height, useNA = "ifany")


# --- SAVE DATASETS TO NEW FILES ---
output_dir <- "/Users/pb/Documents/university/ujm/sem2/data-mining/project/ireland-weather-data/processed_data"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write.csv(weather_data,
          file.path(output_dir, "weather_data_complete.csv"),
          row.names = FALSE)

write.csv(weather_data_pre_2012,
          file.path(output_dir, "weather_data_pre_2012.csv"),
          row.names = FALSE)

write.csv(weather_data_post_2012,
          file.path(output_dir, "weather_data_post_2012.csv"),
          row.names = FALSE)
