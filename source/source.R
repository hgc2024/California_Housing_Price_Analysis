#######
##EDA##
#######

library(ggplot2)
library(dplyr)
library(stats)
library(reshape2)

# Reading dataset
data <- read.csv("California_housing.csv")

# Summary statistics
summary(data)

# Identifying quantitative variables
quant_vars <- c("longitude", "latitude", "house_median_age",
                "total_rooms", "total_bedrooms", "population",
                "households", "median_income", "median_house_value")


# Finding correlations between each pair of quantitative variables
correlation_df <- round(cor(data[, quant_vars], use = "complete.obs"), 2)

# Reshape the dataframe for the heatmap
melted_correlation_matrix <- melt(correlation_df)

# Create heatmap of correlation matrix
ggplot(data= melted_correlation_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red", limit = c(-1,1), name = "Correlation") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank())

# Get count of each category in ocean_proximity
table(data$ocean_proximity)

# Pairwise plots of all quantitative variables
plot(data[quant_vars])

# Isolating median house value pairplots
generate_pairwise_plots <- function(data, response_variable) {
  plots <- list()
  num_cols <- ncol(data)
  
  for (i in 1:num_cols) {
    col_name <- colnames(data)[i]
    if (is.numeric(data[[col_name]]) && col_name != response_variable) {
      p <- ggplot(data, aes_string(x=col_name, y = response_variable)) + 
        geom_point(alpha=0.5) +
        theme_minimal() +
        ggtitle(paste("Median House Value vs", col_name))
      plots[[i]] <- p
    }
  }
  return(plots)
}

# Make pairwise plots with median_house_value as response variable
pairwise_plots_median_house_value <- generate_pairwise_plots(na.omit(data), "median_house_value")
for (p in pairwise_plots_median_house_value) {
  print(p)
}

# Make pairwise plots with median_income as response variable
pairwise_plots_median_income <- generate_pairwise_plots(na.omit(data), "median_income")
for (p in pairwise_plots_median_income) {
  print(p)
}

# Function creates histograms for quantitative variables and bar charts for
# qualitative variables
generate_hist_bar_plots <- function(data) {
  plots <- list()
  num_cols <- ncol(data)
  
  for (i in 1:num_cols) {
    col_name <- colnames(data)[i]
    if (is.numeric(data[[col_name]])) {
      # Histogram for numeric columns
      p <- ggplot(data, aes_string(x=col_name)) +
        geom_histogram(bins=30, fill="blue", color="black") +
        theme_minimal() +
        ggtitle(paste("Frequency Plot of", col_name))
    } else {
      # Bar plot for categorical columns
      p <- ggplot(data, aes_string(x=col_name)) +
        geom_bar(fill="blue", color="black") +
        theme_minimal() +
        ggtitle(paste("Frequency Plot of ", col_name))
    }
    plots[[i]] <- p
  }
  return(plots)
}

# Create and display the plots
hist_plots <- generate_hist_bar_plots(data)
for (p in hist_plots) {
  print(p)
}

# Create boxplots of median_house_value by ocean_proximity categories
ggplot(data, aes(x = ocean_proximity, y = median_house_value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Median House Value by Ocean Proximity",
       x = "Ocean Proximity", y = "Median House Value")

# Creating boxplots of median_income by ocean_proximity categories
ggplot(data, aes(x = ocean_proximity, y = median_income)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Median Income Value by Ocean Proximity",
       x = "Ocean Proximity", y = "Median Income")

# Creating boxplots of latitude by ocean_proximity categories
ggplot(data, aes(x = ocean_proximity, y = latitude)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Latitude by Ocean Proximity",
       x = "Ocean Proximity", y = "Latitude")

# Creating boxplots of longitude by ocean_proximity categories
ggplot(data, aes(x = ocean_proximity, y = longitude)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Longitude by Ocean Proximity",
       x = "Ocean Proximity", y = "Longitude")

# Running ANOVA on median_income vs ocean_proxmity
for (var in quant_vars) {
  anova_test <- aov(as.formula(paste(var, "~ ocean_proximity")), data = data)
  print(paste("ANOVA for ", var, " and ocean_proximity:"))
  print(summary(anova_test))
}

# Show how ocean proximity is distributed geographically
ggplot(data, aes(x = longitude, y = latitude, color = ocean_proximity)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Ocean Proximity by Geographic Coordinates",
       x = "Longitude",
       y = "Latitude",
       color = "Ocean Proximity")


###########
##Cluster##
###########

library(ggplot2)
library(dplyr)
library(stats)
library(lmtest)
library(caret)
library(cluster)
library(car)
library(rlang)
library(e1071)

# Read dataset from csv
data <- read.csv("California_housing.csv")

# Drop rows with NA values
data <- na.omit(data)

# 0 rows are dropped when standard deviation for bounds set to 3
num_sd <- 3.0

mean_median_house_value <- mean(data$median_house_value)
sd_median_house_value <- sd(data$median_house_value)

lower_bound_median_house_value <- mean_median_house_value - num_sd * sd_median_house_value
upper_bound_median_house_value <- mean_median_house_value + num_sd * sd_median_house_value

# Rows with extreme values for house_median_age and median_house_value dropped
trimmed_data <- data[data$median_house_value >= lower_bound_median_house_value &
                       data$median_house_value <= upper_bound_median_house_value &
                       data$house_median_age < 52 &
                       data$median_house_value < 500000,]

data <- trimmed_data
print(nrow(data))

# Initialising the best configuration for cluster models
best_config <- list(weighted_adj_r_squared = -Inf, min_adj_r_squared = -Inf, num_clusters = NULL)

# Trys 2-5 clusters and picks number of clusters with best weighted R2 adjusted
# score, with results saved in best_config
for (num_clusters in 2:5) {
  data$cluster <- as.factor(kmeans(data[, c("longitude", "latitude")], centers = num_clusters, nstart = 25)$cluster)
  
  model_stats <- data.frame(cluster = integer(), adj_r_squared = numeric(), n_obs = integer())
  
  for (i in 1:length(unique(data$cluster))) {
    data_cluster <- filter(data, cluster == as.character(i))
    lm_model <- lm(median_house_value ~ median_income + ocean_proximity, dat = data_cluster)
    adj_r_squared <- summary(lm_model)$adj.r.squared
    n_obs <- nrow(data_cluster)
    model_stats <<- rbind(model_stats, data.frame(cluster = i, adj_r_squared = adj_r_squared, n_obs = n_obs))
  }
  
  min_adj_r_squared <- min(model_stats$adj_r_squared)
  weighted_adj_r_squared <- with(model_stats, sum(adj_r_squared * n_obs) / sum(n_obs))
  
  if (weighted_adj_r_squared > best_config$weighted_adj_r_squared) {
    best_config$weighted_adj_r_squared <- weighted_adj_r_squared
    best_config$min_adj_r_squared <- min_adj_r_squared
    best_config$num_clusters <- num_clusters
  }
}

# Makes sure clusters are set as a readable variable
data$cluster <- as.factor(kmeans(data[, c("longitude", "latitude")], centers = best_config$num_clusters, nstart = 25)$cluster)

# Prints out summary and diagnostic statistics and visuals of the multilinear 
# models for each of 2 clusters
for (i in 1:best_config$num_clusters) {
  data_cluster <-filter(data, cluster == as.character(i))
  lm_model <- lm(median_house_value ~ median_income + ocean_proximity, data = data_cluster)
  print(summary(lm_model))
  
  fitted_values <- fitted(lm_model)
  model_residuals <- residuals(lm_model)
  sqrt_residuals <- sqrt(abs(model_residuals)) * sign(model_residuals)
  
  n_obs <- nrow(data_cluster)
  
  variance_of_errors <- var(model_residuals)
  std_deviation_of_errors <- sd(model_residuals)
  skewness_stat <- skewness(model_residuals)
  kurtosis_stat <- kurtosis(model_residuals)
  
  print(paste("Summary statistics for Cluster ", i))
  print(paste("Number of Observations:", n_obs))
  print(paste("Variance of Errors:", variance_of_errors))
  print(paste("Standard Deviation of Errors:", std_deviation_of_errors))
  print(paste("Skewness:", skewness_stat))
  print(paste("Kurtosis:", kurtosis_stat))
  
  
  dw_test <- dwtest(lm_model)
  print(dw_test)
  
  print(ggplot(data_cluster, aes(x = fitted(lm_model), y = residuals(lm_model))) +
          geom_point() +
          geom_hline(yintercept = 0, color = "red") +
          labs(title = paste("Residual Plot for Cluster", i),
               x = "Fitted Values", y = "Residuals"))
  
  qqPlot(lm_model, main = paste("QQ Plot of Residuals for Cluster", i))
}

# Creating clusters using the same k-Mean method
kmeans_result <- kmeans(data[, c("longitude", "latitude")], centers = 2)

# Add the cluster assignments to the data
data$cluster <- as.factor(kmeans_result$cluster)

# Plot for Geographic Distribution
ggplot(data, aes(x = longitude, y = latitude, color = cluster)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Geographic Distribution of Data Points",
       x = "Longitude", y = "Latitude", color = "Cluster ID")

# Histograms of median_house_value for Each Cluster
ggplot(data, aes(x = median_house_value, fill = cluster)) +
  geom_histogram(bins = 30, alpha = 0.6) +
  facet_wrap(~ cluster, scales = "free_x") +
  theme_minimal() +
  labs(title ="Histograms of Median House Value by Cluster",
       x = "Median House Value", y = "Count", fill = "Cluster")

# Print final results
print(best_config)


#######
##GBM##
#######

library(ggplot2)
library(dplyr)
library(stats)
library(lmtest)
library(randomForest)
library(randomForestExplainer)
library(rpart)
library(rpart.plot)
library(caret)
library(doParallel)
library(car)
library(gbm)
library(pdp)
library(rlang)
library(moments)

# Allows hardware to detect and use all available cores for parallel processing
registerDoParallel(cores = detectCores())

# Reading the dataset from csv
data <- read.csv("California_housing.csv")

# Same data reshaping as the clusters and multilinear method
data <- na.omit(data)

num_sd <- 3.0

mean_median_house_value <- mean(data$median_house_value)
sd_median_house_value <- sd(data$median_house_value)

lower_bound_median_house_value <- mean_median_house_value - num_sd * sd_median_house_value
upper_bound_median_house_value <- mean_median_house_value + num_sd * sd_median_house_value

trimmed_data <- data[data$median_house_value >= lower_bound_median_house_value &
                       data$median_house_value <= upper_bound_median_house_value &
                       data$house_median_age < 52 &
                       data$median_house_value < 500000,]

data <- trimmed_data

# Setting the random seed
set.seed(100)

# Splitting the data into training and testing datasets
train_Index <- createDataPartition(data$median_house_value, p = 0.8, list = FALSE)
train_Data <- data[train_Index, ]
test_Data <- data[-train_Index, ]

# Allows for cross-validation at 5 fold level
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Set seed and train the GBM model on the training dataset
# Max tree depth = 5
# Number of trees = 1000
# Shrinkage = 0.1, 0.01
# Minimum number of observations to split node = 200
set.seed(123)
gbm_model <- train(median_house_value ~ median_income + latitude + longitude,
                   data = train_Data,
                   method = "gbm",
                   trControl = fitControl,
                   verbose = FALSE,
                   tuneGrid = expand.grid(interaction.depth = c(5),
                                          n.trees = c(1000),
                                          shrinkage = c(0.1, 0.01),
                                          n.minobsinnode = 200))

# Print and plot the final GBM model
print(gbm_model)
plot(gbm_model)

# Stores and plots the importance of each predictor in the GBM model
importance <- varImp(gbm_model, scale = FALSE)

importance_df <- as.data.frame(importance$importance)
importance_df$Variable <- rownames(importance_df)

ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance in GBM Model",
       x = "Features", y = "Importance")

partial_dependencies <- function(variables, model) {
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(ceiling(length(variables)/2), 2))
  for (variable in variables) {
    pdp_data <- partial(model, pred.var = variable, grid.resolution = 20, train = test_Data)
    plot(pdp_data, main = paste("Partial Dependence Plot for ", variable),
         xlab = variable,
         ylab = "Predicted Response")
  }
  par(old_par)
}

predictor_variables <- c("median_income", "latitude", "longitude")

partial_dependencies(predictor_variables, gbm_model)

test_Data$predicted <- predict(gbm_model, test_Data)

avg_predictions_by_category <- test_Data %>%
  group_by(ocean_proximity) %>%
  summarize(Average_Prediction = mean(predicted))

# ggplot(avg_predictions_by_category, aes(x = ocean_proximity, y = Average_Prediction)) +
#   geom_bar(stat = "identity", fill = "blue") +
#   theme_minimal() +
#   labs(title = "Effect of Ocean Proximity on Median House Value",
#        x = "Ocean Proximity",
#        y = "Average Predicted House Value")

predictions <- predict(gbm_model, newdata = test_Data)
actual_values <- test_Data$median_house_value
residuals <- actual_values - predictions
sqrt_residuals <- sqrt(abs(residuals)) * sign(residuals)
r2_score <- R2(predictions, test_Data$median_house_value)
r1_score <- cor(predictions, test_Data$median_house_value)
mse <- mean((test_Data$median_house_value - predictions)^2)
skewness_stat <- skewness(residuals)
kurtosis_stat <- kurtosis(residuals)

p_number <- length(coef(gbm_model$finalModel)) - 1
adjusted_r2_score <- 1 - (1 - r2_score) * ((n_rows - 1) / (n_rows - p_number - 1))

variance_of_errors <- var(residuals)
std_deviation_of_errors <- sd(residuals)

dw_test <- dwtest(residuals ~ 1)

print(paste("R1 Score:", r1_score))
print(paste("R2 Score:", r2_score))
print(paste("Adjusted R2 Score:", adjusted_r2_score))
print(paste("Mean Squared Error:", mse))
print(paste("Variance of Errors:", variance_of_errors))
print(paste("Standard Deviation of Errors:", std_deviation_of_errors))
print(paste("Skewness:", skewness_stat))
print(paste("Kurtosis:", kurtosis_stat))
print(dw_test)

hist(residuals, main = "Histogram of Residuals of GBM")

plot(predictions, residuals, main = "Regular Residual Plot of GBM")
abline(h = 0, col = "red")
plot(predictions, sqrt_residuals, main = "Square Root Residual Plot of GBM")
abline(h = 0, col = "red")

qqPlot(residuals, main = "QQ Plot of Residuals for GBM")

