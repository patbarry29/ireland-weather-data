library(dplyr)
library(lubridate)
library(clustMixType)
library(arules)
library(arulesViz)
library(ggplot2)
library(patchwork)
library(forcats)
library(Rtsne)

# --- Configuration ---
file_path_pre2012 <- "/Users/pb/Documents/university/ujm/sem2/data-mining/project/ireland-weather-data/data/processed_data/weather_data_pre_2012.csv"
weather_data_pre2012 <- read.csv(file_path_pre2012)

# -- Basic Type Conversion --
weather_data_pre2012$date <- ymd_hms(weather_data_pre2012$date, truncated = 3, quiet = TRUE)
weather_data_pre2012$hour <- as.factor(weather_data_pre2012$hour) # Treat hour as categorical for some analyses
weather_data_pre2012$month <- as.factor(weather_data_pre2012$month)
weather_data_pre2012$day_of_week <- as.factor(weather_data_pre2012$day_of_week)

# Define numerical and categorical variables for analysis
numerical_vars <- c("precipitation", "temperature", "wet_bulb_temp", "dew_point",
                    "vapor_pressure", "relative_humidity", "sea_level_pressure",
                    "wind_speed", "sunshine_duration", "visibility",
                    "cloud_height", "cloud_amount")
# Adding hour, month might be interesting for rules/clusters
categorical_vars <- c("present_weather_category", "present_weather_intensity",
                      "past_weather_category", "has_cloud_ceiling",
                      "irain_factor", "itemp_factor", "iwb_factor", # Your original factors
                      "iwdsp_factor", "iwddir_factor",
                      "hour", "month") # Add time factors

# Ensure columns exist and convert types
available_num_vars <- intersect(numerical_vars, names(weather_data_pre2012))
available_cat_vars <- intersect(categorical_vars, names(weather_data_pre2012))

weather_data_pre2012 <- weather_data_pre2012 %>%
    mutate(across(all_of(available_num_vars), as.numeric)) %>%
    mutate(across(all_of(available_cat_vars), as.factor))

str(weather_data_pre2012 %>% select(all_of(available_num_vars), all_of(available_cat_vars)), list.len=6)


# ============================================================
# 1. Clustering with k-Prototypes (Handles Mixed Data)
# ============================================================

# -- VARIABLES FOR CLUSTERING --
cluster_num_vars <- intersect(c("temperature", "precipitation", "relative_humidity", "wind_speed",
                                "sea_level_pressure", "sunshine_duration"),
                              available_num_vars)
cluster_cat_vars <- intersect(c("present_weather_category", "present_weather_intensity",
                                "has_cloud_ceiling", "month"),
                              available_cat_vars)
cluster_vars <- c(cluster_num_vars, cluster_cat_vars)


cluster_data <- weather_data_pre2012 %>%
  select(all_of(cluster_vars)) %>%
  na.omit()

# -- FIND OPTIMAL K --
print("Calculating WCSS for elbow plot (may take time)...")
lambda_est <- tryCatch({
        lambdaest(cluster_data)
    },
    error = function(e) {
        print(paste("Lambda estimation failed:", e)); return(0.5)
    })

wcss <- numeric(8)
set.seed(42)

for (k in 2:8) {
    print(paste("Trying k =", k))
    kpres <- tryCatch({
        kproto(cluster_data, k = k, lambda = lambda_est, iter.max = 10, nstart = 1, verbose = FALSE)
    }, error = function(e) {
        print(paste("kproto failed for k =", k, ":", e$message)); NULL
    })
    if (!is.null(kpres)) {
        wcss[k] <- kpres$tot.withinss
    } else {
        wcss[k] <- NA
    }
}

# Plot the elbow curve
elbow_df <- data.frame(k = 1:8, WCSS = wcss)
pdf("plots/elbow_plot.pdf", width = 7, height = 5)
elbow_plot <- ggplot(elbow_df %>% filter(k >= 2, !is.na(WCSS)), aes(x = k, y = WCSS)) +
        geom_line(color = "blue") +
        geom_point(color = "blue") +
        labs(
            # title = "Elbow Method for Optimal k (k-Prototypes)",
                x = "Number of Clusters (k)",
                y = "Total Within-Cluster Sum of Squares (WCSS)"
            ) +
        theme_minimal()
print(elbow_plot)
dev.off()

# k=5 based on elbow plot
chosen_k <- 5

print(paste("Running k-Prototypes with k =", chosen_k, "... (may take time)"))
set.seed(42)
kpres_final <- tryCatch({
    kproto(cluster_data, k = chosen_k, lambda = lambda_est, iter.max = 50, nstart = 5, verbose=FALSE) # Use more iterations + starts now
}, error = function(e) {
    print(paste("Final kproto failed:", e$message)); NULL
})

print("k-Prototypes finished. Cluster sizes:")
print(kpres_final$size)

cluster_data$cluster <- as.factor(kpres_final$cluster)

# -- CLUSTER STATS --

# mean for numerical
print(aggregate(cluster_data[, cluster_num_vars, drop = FALSE], list(cluster = cluster_data$cluster), mean))

# mode for categorical
calculate_mode <- function(x) {
    uniqx <- unique(na.omit(x))
    uniqx[which.max(tabulate(match(x, uniqx)))]
}
print(aggregate(cluster_data[, cluster_cat_vars, drop = FALSE], list(cluster = cluster_data$cluster), calculate_mode))

# -- VISUALISE WITH t-SNE --
print("Performing t-SNE dimensionality reduction for cluster visualization...")

# Prepare numerical data for t-SNE
tsne_data <- cluster_data %>%
  select(all_of(cluster_num_vars)) %>%
  scale()

set.seed(42)

tsne_result <- Rtsne(tsne_data,
                     dims = 2,
                     perplexity = 30,
                     theta = 0.5,
                     check_duplicates = FALSE,
                     pca = TRUE,
                     max_iter = 1000)

tsne_plot_data <- data.frame(
  tsne1 = tsne_result$Y[,1],
  tsne2 = tsne_result$Y[,2],
  cluster = cluster_data$cluster
)

# Plot t-SNE results
pdf("plots/cluster_tsne.pdf", width = 7, height = 5)
cluster_tsne_plot <- ggplot(tsne_plot_data, aes(x = tsne1, y = tsne2, color = cluster)) +
  geom_point(alpha = 0.5, size = 1.0) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    # title = paste("t-SNE Visualization of Clusters (k =", chosen_k, ")"),
    #    subtitle = "High-dimensional data projected to 2D using t-SNE",
       x = "t-SNE Dimension 1",
       y = "t-SNE Dimension 2") +
  theme_minimal() +
  theme(legend.position = "right")
print(cluster_tsne_plot)
dev.off()

cluster_vis_plot <- ggplot(cluster_data, aes(x = temperature, y = relative_humidity, color = cluster)) +
  geom_point(alpha = 0.3, size = 0.8) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = paste("Clusters (k =", chosen_k, ") - Temp vs Humidity"),
       subtitle = "Color indicates cluster assignment",
       x = "Temperature", y = "Relative Humidity") +
  theme_minimal()

# Display the original plot and t-SNE plot side by side using patchwork
print(cluster_vis_plot + cluster_tsne_plot + plot_layout(ncol = 2))


# ============================================================
# 2. Association Rule Mining (Apriori)
# ============================================================

# -- SELECT VARIABLES --
rule_cat_vars <- intersect(c("has_cloud_ceiling", "month", "hour"),
                            available_cat_vars)
rule_num_vars <- intersect(c("temperature", "relative_humidity", "wind_speed", "sea_level_pressure", "precipitation"),
                           available_num_vars)

rules_data_base <- weather_data_pre2012 %>%
                   select(all_of(rule_cat_vars), all_of(rule_num_vars))

# -- DISCRETIZE NUM VARS --
rules_data_discretized <- rules_data_base

for(num_var in rule_num_vars) {
    discretized_col <- tryCatch({
        discretize(rules_data_base[[num_var]], method = "frequency", breaks = 3,
                    labels = c(paste0(num_var, "_Low"), paste0(num_var, "_Medium"), paste0(num_var, "_High")))
    }, error = function(e) {
        print(paste("Could not discretize", num_var, ":", e$message, "- Skipping variable."))
        NULL
    })

    if (!is.null(discretized_col)) {
        rules_data_discretized[[paste0(num_var, "_disc")]] <- discretized_col
    }
}

# Remove num cols after discretization
rules_data_final <- rules_data_discretized %>% select(-all_of(rule_num_vars))

# -- APRIORI REQUIRES TRANSACTION FORMAT --
weather_trans <- tryCatch({
    as(rules_data_final, "transactions")
}, error = function(e) {
    print(paste("Error converting to transactions:", e$message)); NULL
})

print(summary(weather_trans))

# -- RUN APRIORI --
min_support <- 0.05     # Rule must apply to at least 5% of transactions
min_confidence <- 0.6   # If LHS occurs, RHS must occur in at least 60% of those cases

print(paste("Running Apriori with min support =", min_support, "and min confidence =", min_confidence,"..."))
weather_rules <- apriori(weather_trans,
                            parameter = list(supp = min_support, conf = min_confidence, minlen = 2)) # minlen=2 avoids trivial rules

print(paste("Apriori finished. Found", length(weather_rules), "rules."))

# Inspect the top rules sorted by lift (how much more likely RHS is given LHS)
print("\nTop 10 Rules sorted by Lift:")
inspect(sort(weather_rules, by = "lift")[seq_len(min(10, length(weather_rules)))])

print("\nTop 10 Rules sorted by Confidence:")
inspect(sort(weather_rules, by = "confidence")[seq_len(min(10, length(weather_rules)))])

# --- VISUALISE RULES ---
# 1. Scatter plot of rules
print("Generating interactive rule scatter plot")
plot(weather_rules, measure=c("support", "confidence"), shading="lift", engine='plotly')

# Graph-based visualization
print("Generating rule network graph (Top 10 by Lift)...")
sub_rules_graph <- head(sort(weather_rules, by = "lift"), 10)
# graph_plot <- plot(sub_rules_graph, method = "graph", engine = "htmlwidget") # Interactive
graph_plot <- plot(sub_rules_graph, method = "graph", engine = "igraph")
print(graph_plot)

# 3. Grouped matrix plot (shows LHS vs RHS)
sub_rules_matrix <- head(sort(weather_rules, by = "lift"), 10)
tryCatch({
    plot(sub_rules_matrix, method = "grouped matrix", engine = "ggplot")
}, error = function(e){ print(paste("Grouped matrix plot failed:", e$message))})
