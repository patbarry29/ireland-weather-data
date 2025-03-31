library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)
library(GGally)

file_path <- "/Users/pb/Documents/university/ujm/sem2/data-mining/project/ireland-weather-data/data/processed_data/weather_data_complete.csv"
weather_data <- read.csv(file_path)

core_numerical_vars <- c("precipitation", "temperature", "wet_bulb_temp",
                         "vapor_pressure", "relative_humidity", "sea_level_pressure",
                         "wind_speed")

# Variables affected by the 2012 sensor change
affected_numerical_vars <- c("sunshine_duration", "visibility", "cloud_height", "cloud_amount")

# All numerical variables
all_numerical_vars <- c(core_numerical_vars, affected_numerical_vars)

# Convert 'date' column
weather_data$date <- ymd_hms(weather_data$date, truncated = 3, quiet = TRUE)

# --- CORRELATION MATRIX/HEATMAP ---

core_data <- weather_data %>%
  select(all_of(core_numerical_vars))

# Calculate the correlation matrix
cor_matrix_core <- cor(core_data, use = "pairwise.complete.obs")

print("Correlation Matrix (Core Variables):")
print(round(cor_matrix_core, 2))

print("Visualizing Correlation Matrix (Core Variables)...")
pdf("correlation_matrix.pdf", width = 7, height = 5)
corrplot(cor_matrix_core,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         diag = FALSE,
        #  title = "Correlation Matrix of Core Weather Variables",
         mar = c(0, 0, 1, 0))
dev.off()

# Create a Pair Plot for a subset of variables
subset_vars_for_ggpairs <- c("temperature", "relative_humidity", "precipitation",
                             "wind_speed")
core_data_subset <- core_data %>% select(all_of(subset_vars_for_ggpairs))

set.seed(42)
core_data_subset <- core_data_subset %>% sample_n(min(10000, nrow(core_data_subset)))

print("Generating Pair Plot (Subset of Core Variables, Sampled Data)...")
ggpairs_plot_core <- ggpairs(core_data_subset,
                             title = "Pair Plot of Selected Core Variables (Sampled Data)",
                             upper = list(continuous = wrap("cor", size = 3)),
                             lower = list(continuous = wrap("points", alpha = 0.2, size = 0.5)),
                             diag = list(continuous = wrap("densityDiag", alpha = 0.5))
                            ) +
                       theme_minimal(base_size = 8)

print(ggpairs_plot_core)

# --- PRE 2012 DATA ---

file_path_pre2012 <- "/Users/pb/Documents/university/ujm/sem2/data-mining/project/ireland-weather-data/data/processed_data/weather_data_pre_2012.csv"
weather_data_pre2012 <- read.csv(file_path_pre2012)

# Convert date column
weather_data_pre2012$date <- ymd_hms(weather_data_pre2012$date, truncated = 3, quiet = TRUE)

numerical_vars_pre2012 <- c("precipitation", "temperature", "wet_bulb_temp", "dew_point",
                            "vapor_pressure", "relative_humidity", "sea_level_pressure",
                            "wind_speed", "sunshine_duration", "visibility",
                            "cloud_height", "cloud_amount")
categorical_vars_new <- c("present_weather_category", "present_weather_intensity",
                          "past_weather_category", "has_cloud_ceiling")

# --- 2a. Correlation Analysis (Numerical Variables) ---

pre2012_data_numeric <- weather_data_pre2012 %>%
  select(all_of(available_num_vars)) # Use only available numeric vars

 # Calculate the correlation matrix
cor_matrix_pre2012 <- cor(pre2012_data_numeric, use = "pairwise.complete.obs")

print(round(cor_matrix_pre2012, 2))

# VISUALISE
corrplot(cor_matrix_pre2012,
          method = "color", type = "upper", order = "hclust",
          tl.col = "black", tl.srt = 45, addCoef.col = "black",
          number.cex = 0.6, diag = FALSE, # Adjusted text size
          title = "Correlation Matrix (Numerical Variables, Pre-Apr 2012)",
          mar = c(0, 0, 1, 0))

# ---PRESENT VS PAST ANALYSIS ---
present_vs_past <- c("present_weather_category", "past_weather_category")

pres <- present_vs_past[1]
past <- present_vs_past[2]

# Calculate & Print Frequency Table
print(paste("\nFrequency Table:", pres, "vs", past))
freq_table <- table(weather_data_pre2012[[pres]], weather_data_pre2012[[past]], dnn = c(pres, past)) # Add names
print(freq_table)

# Create Stacked Bar Chart (showing proportions)
plot_data_cat <- weather_data_pre2012 %>% filter(!is.na(.data[[pres]]) & !is.na(.data[[past]]))

p_bar <- ggplot(plot_data_cat, aes(x = .data[[pres]], fill = .data[[past]])) +
  geom_bar(position = "fill") + # "fill" shows proportions adding up to 1
  scale_y_continuous(labels = scales::percent) + # Format y-axis as percentage
  labs(title = paste("Proportion of", tools::toTitleCase(gsub("_", " ", past))),
    subtitle = paste("within each", tools::toTitleCase(gsub("_", " ", pres))),
    x = tools::toTitleCase(gsub("_", " ", pres)),
    y = "Proportion",
    fill = tools::toTitleCase(gsub("_", " ", past))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

print(p_bar)

# --- PRESENT CATEGORY VS INTENSITY ---
cat_vs_intensity <- c("present_weather_category", "present_weather_intensity")

cat <- cat_vs_intensity[1]
int <- cat_vs_intensity[2]

# Calculate & Print Frequency Table
print(paste("\nFrequency Table:", cat, "vs", int))
freq_table <- table(weather_data_pre2012[[cat]], weather_data_pre2012[[int]], dnn = c(cat, int)) # Add names
print(freq_table)

# Create Stacked Bar Chart (showing proportions)
plot_data_cat <- weather_data_pre2012 %>% filter(!is.na(.data[[cat]]) & !is.na(.data[[int]]))

p_bar <- ggplot(plot_data_cat, aes(x = .data[[cat]], fill = .data[[int]])) +
  geom_bar(position = "fill") + # "fill" shows proportions adding up to 1
  scale_y_continuous(labels = scales::percent) + # Format y-axis as percentage
  labs(title = paste("Proportion of", tools::toTitleCase(gsub("_", " ", int))),
    subtitle = paste("within each", tools::toTitleCase(gsub("_", " ", cat))),
    x = tools::toTitleCase(gsub("_", " ", cat)),
    y = "Proportion",
    fill = tools::toTitleCase(gsub("_", " ", int))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))

print(p_bar)

# --- SCATTERPLOTS ---

# Precipitation vs Relative Humidity
p1 <- ggplot(core_data_subset, aes(x = precipitation, y = relative_humidity)) +
  geom_point(alpha = 0.8, size = 0.5, color = "blue") +
  labs(title = "Precipitation vs Relative Humidity",
       x = "Precipitation (mm)",
       y = "Relative Humidity (%)") +
  theme_minimal()

print(p1)

# --- SUNSHINE DURATION VS TEMPERATURE ---
p4 <- ggplot(weather_data_pre2012, aes(x = sunshine_duration, y = temperature)) +
  geom_point(alpha = 0.2, size = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Sunshine Duration vs Temperature (Pre-Apr 2012)",
        x = "Sunshine Duration (hours)",
        y = "Temperature (°C)") +
  theme_minimal()

print(p4)
