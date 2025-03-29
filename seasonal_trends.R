library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

file_path <- "/Users/pb/Documents/university/ujm/sem2/data-mining/project/ireland-weather-data/data/processed_data/weather_data_complete.csv"
weather_data <- read.csv(file_path)

# Define variables to analyze
vars_to_analyze <- c("precipitation", "temperature", "relative_humidity", "wind_speed")

# Convert 'date' column
weather_data$date <- ymd_hms(weather_data$date, truncated = 3, quiet = TRUE)

# Ensure 'year' and 'month' columns are numeric
weather_data$year <- as.numeric(weather_data$year)
weather_data$month <- as.numeric(weather_data$month)

# --- ADD SEASON COLUMN ---
weather_data <- weather_data %>%
  mutate(
    season = case_when(
      month %in% 3:5   ~ "Spring",
      month %in% 6:8   ~ "Summer",
      month %in% 9:11  ~ "Autumn",
      month %in% c(12, 1, 2) ~ "Winter",
      TRUE             ~ NA_character_
    ),
    season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))
  )

# --- REMOVE DECEMBER 2021 FROM DATA ---
weather_data_modded <- weather_data %>%
                      filter(!(year == (2021) & season == "Winter"))

print(tail(weather_data_modded))

# --- SEASONAL YEARLY AVERAGES ---
seasonal_yearly_summary <- weather_data_modded %>%
  filter(!is.na(season)) %>%
  group_by(year, season) %>%
  summarise(across(all_of(vars_to_analyze), ~mean(.x, na.rm = TRUE)),
            .groups = 'drop')

print(tail(seasonal_yearly_summary))


# --- SEASONAL PLOTS ---

faceted_seasonal_plot_list <- list()

for (var in vars_to_analyze) {

  subtitle_msg <- "Yearly trend shown within each season's panel."
  plot_subtitle <- paste0(subtitle_msg, "\nData covers full seasons up to Autumn 2021.")

  p <- ggplot(seasonal_yearly_summary, aes(x = year, y = .data[[var]])) +
    geom_line(color = "steelblue", alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "tomato", linetype = "dashed", linewidth = 0.5) +
    facet_wrap(~ season, ncol = 2) +
    labs(
      title = paste("Yearly Trend by Season:", gsub("_", " ", tools::toTitleCase(var))),
      subtitle = plot_subtitle,
      x = "Year",
      y = paste("Average", gsub("_", " ", var))
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1.5, "lines")
    )

  faceted_seasonal_plot_list[[var]] <- p
}

print(faceted_seasonal_plot_list[["precipitation"]])

print(faceted_seasonal_plot_list[["temperature"]])

print(faceted_seasonal_plot_list[["relative_humidity"]])

print(faceted_seasonal_plot_list[["wind_speed"]])


#### ----------------------


# --- CREATE HEATMAP FOR YEAR/MONTH TEMPERATURE PATTERNS ---
yearly_monthly_temp <- weather_data %>%
  group_by(year, month) %>%
  summarise(avg_temp = mean(temperature, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = month, values_from = avg_temp, names_prefix = "M")

temp_matrix <- as.matrix(yearly_monthly_temp[, -1])
rownames(temp_matrix) <- yearly_monthly_temp$year

heatmap(temp_matrix,
        Colv = NA,
        Rowv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(100),
        xlab = "Month",
        ylab = "Year",
        main = "Temperature Patterns by Month and Year")

# --- CREATE BOXPLOT TO SHOW SEASONAL VARIATIONS/OUTLIERS
ggplot(weather_data, aes(x = as.factor(month), y = temperature)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Monthly Temperature Distribution",
       x = "Month",
       y = "Temperature (°C)") +
  theme_minimal()

# --- BAR PLOT OF SEASONAL TREND SLOPES
ggplot(trend_stats_long, aes(x = season, y = slope, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Annual Rate of Change in Weather Metrics by Season",
       x = "Season",
       y = "Slope (Change per Year)",
       fill = "Weather Metric") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
