library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(car)
laptop_prices_modified <- read_csv("laptop_prices_modified.csv")

subset_columns <- c("inches", "ram", "weight", "storageamount", "CPU_freq") # "storagetype", "company"

# Summarize data
for (col in subset_columns) {
  print(toupper(col))
  formula <- as.formula(paste("price_euros ~", col))
  model <- lm(formula, data = laptop_prices_modified)
  print(summary(model))
}

# Generate Diagnostics Plots -- No remedial measures
for (col in subset_columns) {
  print(toupper(col)) 
  formula <- as.formula(paste("price_euros ~", col))
  model <- lm(formula, data = laptop_prices_modified)
  par(mfrow = c(2, 2))
  plot(model, main = paste("Diagnostics for", toupper(col)))
}

# multiple regression model
laptop_prices_modified$company_factor <- as.factor(laptop_prices_modified$company)
laptop_prices_modified$storage_type_factor <- as.factor(laptop_prices_modified$storagetype)
full_model = lm(
  price_euros ~ inches*weight + ram + storageamount + 
    storage_type_factor + company_factor + CPU_freq,
  data = laptop_prices_modified
)
summary(full_model)

quantative_model = lm(
  price_euros ~ inches + ram + weight + storageamount + CPU_freq,
  data = laptop_prices_modified
)
summary(quantative_model)

interactive_model = lm(
  price_euros ~ inches*weight + ram + storageamount + CPU_freq,
  data = laptop_prices_modified
)
summary(interactive_model)

# Apply logarithmic transformation to the relevant columns and then fit the models
for (col in subset_columns) {
  print(toupper(col)) 
  transformed_data <- laptop_prices_modified
  transformed_data$log_price <- log(transformed_data$price_euros)
  if (is.numeric(transformed_data[[col]])) {
    transformed_data[[paste0("log_", col)]] <- log(transformed_data[[col]])
  }
  formula <- as.formula(paste("log_price ~", paste0("log_", col)))
  fitted_model <- lm(formula, data = transformed_data)
  fitted_values <- fitted(fitted_model)
  residuals_values <- residuals(fitted_model)
  
  # Generate diagnostics plots
  par(mfrow = c(2, 2))
  plot(fitted_model, main = paste("Log Diagnostics for", toupper(col)))
}

# Predicted vs. Actual
predicted <- predict(full_model)
actual <- laptop_prices_modified$price_euros

# Scatter plot
par(mfrow = c(1, 1))
plot(actual, predicted, 
     xlab = "Actual Prices", 
     ylab = "Predicted Prices", 
     main = "Predicted vs. Actual Prices")
abline(0, 1, col = "red")  # Add a y = x line

# Partial dependence plots
for (col in subset_columns) {
  p <- ggplot(data = laptop_prices_modified, aes_string(x = col, y = "price_euros")) +
    geom_point(alpha = 0.5) +  # Add data points
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +  # Add regression line
    labs(title = paste("Effect of", col, "on Price"), 
         x = col, 
         y = "Price (Euros)")
  
  print(p)
}

# Partial dependence on Company
laptop_prices_modified$company_factor_ordered <- reorder(
  laptop_prices_modified$company_factor,
  laptop_prices_modified$price_euros,
  FUN = median
)
# Plot with reordered factor levels
ggplot(data = laptop_prices_modified, aes(x = company_factor_ordered, y = price_euros)) +
  geom_point(alpha = 0.5) +  # Add data points
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +  # Add regression line
  labs(title = "Effect of Company on Price", x = "Company (Ordered by Average Price)", y = "Price (Euros)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Generate the diagnostic plots for the full model
par(mfrow = c(2, 2))  # Arrange the plots in a 2x2 grid
  # Residuals vs. Fitted plot
  plot(full_model, main = "Residuals vs. Fitted")

# Apply log transformation to the full model
laptop_prices_modified$log_price <- log(laptop_prices_modified$price_euros)
laptop_prices_modified$log_inches <- log(laptop_prices_modified$inches)
laptop_prices_modified$log_ram <- log(laptop_prices_modified$ram)
laptop_prices_modified$log_weight <- log(laptop_prices_modified$weight)
laptop_prices_modified$log_storageamount <- log(laptop_prices_modified$storageamount)

# Fit the model with log-transformed variables
log_formula <- log_price ~ log_inches + log_ram + log_weight + log_storageamount + 
  storage_type_factor + company_factor

log_model <- lm(log_formula, data = laptop_prices_modified)

# Print the summary of the transformed model
summary(log_model)

# Generate the diagnostic plots for the log-transformed model
par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
plot(log_model, main = "Diagnostics for Log-Transformed Model")

# Predicted vs. Actual (log)
predicted <- predict(log_model)
actual <- laptop_prices_modified$price_euros

# Scatter plot
par(mfrow = c(1, 1))
plot(actual, predicted, 
     xlab = "Actual Prices", 
     ylab = "Predicted Prices", 
     main = "Predicted vs. Actual Prices")
abline(0, 1, col = "red")  # Add a y = x line

########################################
# Singular & Multiple Comparison Models
########################################
{
  weight_inches = lm(
    price_euros ~ inches + weight,
    data = laptop_prices_modified
  )
  summary(weight_inches)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(weight_inches, main = "Diagnostics: Weight + Inches")
  
  storage_amount_type = lm(
    price_euros ~ storageamount + storage_type_factor,
    data = laptop_prices_modified
  )
  summary(storage_amount_type)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(storage_amount_type, main = "Diagnostics: Storage Amount + Type")
  
  freq_ram = lm(
    price_euros ~ CPU_freq + ram,
    data = laptop_prices_modified
  )
  summary(freq_ram)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(freq_ram, main = "Diagnostics: CPU Freq + RAM")
  
  company_freq = lm(
    price_euros ~ CPU_freq + company_factor,
    data = laptop_prices_modified
  )
  summary(company_freq)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(company_freq, main = "Diagnostics: CPU Freq + Company")
  
  company_ram = lm(
    price_euros ~ ram + company_factor,
    data = laptop_prices_modified
  )
  summary(company_ram)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(company_ram, main = "Diagnostics: Company + RAM")
  
  company_storage_amount = lm(
    price_euros ~ storageamount + company_factor,
    data = laptop_prices_modified
  )
  summary(company_storage_amount)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(company_storage_amount, main = "Diagnostics: Company + Storage Amount")
  
  company_inches = lm(
    price_euros ~ inches + company_factor,
    data = laptop_prices_modified
  )
  summary(company_inches)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(company_inches, main = "Diagnostics: Company + Inches")
  
  company_weight = lm(
    price_euros ~ weight + company_factor,
    data = laptop_prices_modified
  )
  summary(company_weight)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(company_weight, main = "Diagnostics: Company + Weight")
  
  company_weight_inches = lm(
    price_euros ~ inches + weight + company_factor,
    data = laptop_prices_modified
  )
  summary(company_weight_inches)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(company_weight_inches, main = "Diagnostics: Company + Weight + Inches")
}

# Log transformed models that looked interesting
{ # Not currently log transformed as of 2024-12-10T12:11PM EST
  weight_inches = lm(
    price_euros ~ inches + weight,
    data = laptop_prices_modified
  )
  summary(weight_inches)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(weight_inches, main = "Diagnostics: Weight + Inches")
  
  freq_ram = lm(
    price_euros ~ CPU_freq + ram,
    data = laptop_prices_modified
  )
  summary(freq_ram)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(freq_ram, main = "Diagnostics: CPU Freq + RAM")
  
  company_freq = lm(
    price_euros ~ CPU_freq + company_factor,
    data = laptop_prices_modified
  )
  summary(company_freq)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(company_freq, main = "Diagnostics: CPU Freq + Company")
}

# Ranges
subset_000_100 <- subset(laptop_prices_modified, storageamount >= 0 & storageamount < 100)
subset_100_200 <- subset(laptop_prices_modified, storageamount >= 100 & storageamount < 200)
subset_200_400 <- subset(laptop_prices_modified, storageamount >= 200 & storageamount < 400)
subset_400_600 <- subset(laptop_prices_modified, storageamount >= 400 & storageamount < 600)
subset_600_1200 <- subset(laptop_prices_modified, storageamount >= 600 & storageamount < 1200)
subset_1200_2200 <- subset(laptop_prices_modified, storageamount >= 1200 & storageamount < 2200)
lm_000_200 <- lm(price_euros ~ inches + weight, data = subset_000_100)
lm_000_200 <- lm(price_euros ~ inches + weight, data = subset_100_200)
lm_200_300 <- lm(price_euros ~ inches + weight, data = subset_200_400)
lm_400_600 <- lm(price_euros ~ inches + weight, data = subset_400_600)
lm_600_1200 <- lm(price_euros ~ inches + weight, data = subset_600_1200)
lm_1200_2200 <- lm(price_euros ~ inches + weight, data = subset_1200_2200)

laptop_prices_modified$storage_range <- cut(
  laptop_prices_modified$storageamount,
  breaks = c(0, 100, 200, 400, 600, 1200, 2200),  # Define range endpoints
  labels = c("0-100GB", "100-200GB", "200-400GB", "400-600GB", "600-1200GB", "1200-2200GB"),
  right = FALSE  # Use left-closed intervals [a, b)
)

lm_with_ranges <- lm(price_euros ~ inches + weight + storage_range, data = laptop_prices_modified)
summary(lm_with_ranges)

table(laptop_prices_modified$storage_range)

# Jitter Plot
ggplot(laptop_prices_modified, aes(x = storage_range, y = price_euros)) +
  #geom_boxplot(outlier.color = "red", alpha = 0.7) +  # Boxplot with slightly transparent boxes
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue") +  # Jittered points for raw data
  labs(title = "Price by Storage Range", x = "Storage Range", y = "Price (Euros)") +
  theme_minimal()


# Min and Max storage within ranges
storage_summary <- aggregate(storageamount ~ storage_range, data = laptop_prices_modified, 
                             FUN = function(x) c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE)))
# Split into separate columns for min and max
storage_summary <- do.call(data.frame, storage_summary)
names(storage_summary)[2:3] <- c("min_storage", "max_storage")
print(storage_summary)


# Select numeric columns including price_euros
numeric_data <- laptop_prices_modified[, c("price_euros", "inches", "ram", "weight", "storageamount", "CPU_freq")]

# Scatter plot matrix
pairs(numeric_data, 
      main = "Scatter Plot Matrix with Price (Y)", 
      pch = 19, col = "blue",
      labels = c("Price (€)", "Inches", "RAM", "Weight", "Storage", "CPU_freq"))


# Add cors
quantitative_data <- laptop_prices_modified[, c("price_euros", "inches", "ram", "weight", "storageamount", "CPU_freq")]
# Pearson correlation matrix
cor_matrix <- cor(quantitative_data, method = "pearson")
print(cor_matrix)


# Select numeric columns
numeric_data <- laptop_prices_modified[, c("price_euros", "inches", "ram", "weight", "CPU_freq", "storageamount")]
# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
# Melt the matrix for ggplot2
cor_melt <- melt(cor_matrix)
# Plot the heatmap
ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  ggtitle("Correlation Heatmap")


agg_data <- aggregate(price_euros ~ company_factor + storage_type_factor, data = laptop_prices_modified, FUN = mean)
# Plot the heatmap
ggplot(agg_data, aes(x = company_factor, y = storage_type_factor, fill = price_euros)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal() +
  ggtitle("Average Price by Company and Storage Type")


# Filter out rows where storageamount > 600
filtered_data <- laptop_prices_modified %>%
  filter(storageamount <= 1100)

# Refit the model
filtered_model <- lm(price_euros ~ storageamount, data = filtered_data)

# Scatter plot with regression line
plot(filtered_data$storageamount, filtered_data$price_euros, 
     xlab = "Storage Amount (GB)", 
     ylab = "Price (Euros)", 
     main = "Price vs Storage Amount (Filtered)",
     pch = 19, col = "black")  # Scatter plot points
abline(filtered_model, col = "red", lwd = 2)  # Regression line


# Laptop Size by Weight Heatmap
laptop_prices_modified$inches_bin <- cut(laptop_prices_modified$inches, breaks = 5)
laptop_prices_modified$weight_bin <- cut(laptop_prices_modified$weight, breaks = 5)
# Aggregate price by bins
price_bins <- aggregate(price_euros ~ inches_bin + weight_bin, data = laptop_prices_modified, FUN = mean, na.rm = TRUE)
# Plot the heatmap
ggplot(price_bins, aes(x = inches_bin, y = weight_bin, fill = price_euros)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Price Distribution by Laptop Size and Weight", x = "Screen Size (Inches)", y = "Weight (kg)", fill = "Avg Price (€)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Storage Histogram by Company
ggplot(laptop_prices_modified, aes(x = reorder(company_factor, price_euros, FUN = median), y = price_euros, fill = company_factor)) +
  geom_boxplot(outlier.size = 1, outlier.color = "red", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Price Distribution by Company (Ordered by Median Price)",
    x = "Company",
    y = "Price (€)",
    fill = "Company"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Remove the color guide (legend) on the right
  )


# Price per gigabyte storage per company histogram
# Step 1: Calculate price per gigabyte
laptop_prices_modified <- laptop_prices_modified %>%
  mutate(price_per_gb = price_euros / storageamount)
# Step 2: Create the plot
ggplot(laptop_prices_modified, aes(x = reorder(company_factor, price_per_gb, FUN = median), y = price_per_gb, fill = company_factor)) +
  geom_boxplot(outlier.size = 1, outlier.color = "red", alpha = 0.7) +  # Boxplot of price per GB
  theme_minimal() +
  labs(
    title = "Price per Gigabyte of Storage by Company (Ordered by Median)",
    x = "Company",
    y = "Price per GB (€)",
    fill = "Company"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  # Remove the color guide
  )


# Price per gigabyte RAM per company histogram
# Step 1: Calculate price per gigabyte
laptop_prices_modified <- laptop_prices_modified %>%
  mutate(price_per_gb = price_euros / ram)
# Step 2: Create the plot
ggplot(laptop_prices_modified, aes(x = reorder(company_factor, price_per_gb, FUN = median), y = price_per_gb, fill = company_factor)) +
  geom_boxplot(outlier.size = 1, outlier.color = "red", alpha = 0.7) +  # Boxplot of price per GB
  theme_minimal() +
  labs(
    title = "Price per Gigabyte of RAM by Company (Ordered by Median)",
    x = "Company",
    y = "Price per GB Ram (€)",
    fill = "Company"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  # Remove the color guide
  )

# Function to plot Price vs Storage Amount for Storage <= 530GB
plot_price_vs_storage <- function(data, max_storage = 530) {
  filtered_data <- data %>% filter(storageamount <= max_storage)
  
  ggplot(filtered_data, aes(x = storageamount, y = price_euros)) +
    geom_point(color = "black", alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    labs(
      title = paste("Relationship Between Price and Storage (Storage <= ", max_storage, "GB)", sep = ""),
      x = "Storage Amount (GB)",
      y = "Price (Euros)"
    ) +
    theme_minimal()
}

# Call the function with your data
plot_price_vs_storage(laptop_prices_modified)


# Calculate partial R2 - Removes inches
# Full Model
full_model <- lm(price_euros ~ inches + ram + weight + storageamount + CPU_freq,
                 data = laptop_prices_modified)
r2_full <- summary(full_model)$r.squared
# Define a list of reduced models
reduced_models <- list(
  "No storageamount" = lm(price_euros ~ inches + ram + weight + CPU_freq, data = laptop_prices_modified),
  "No CPU_freq" = lm(price_euros ~ inches + ram + weight + storageamount, data = laptop_prices_modified),
  "No weight" = lm(price_euros ~ inches + ram + storageamount + CPU_freq, data = laptop_prices_modified),
  "No ram" = lm(price_euros ~ inches + weight + storageamount + CPU_freq, data = laptop_prices_modified),
  "No inches" = lm(price_euros ~ ram + weight + storageamount + CPU_freq, data = laptop_prices_modified)
)
# Calculate Partial R^2 for each model
partial_r2 <- sapply(reduced_models, function(model) {
  r2_reduced <- summary(model)$r.squared
  (r2_full - r2_reduced) / (1 - r2_reduced)
})
# Display Partial R^2 values with rounding to three decimal places
partial_r2_table <- data.frame(
  Partial_R2 = round(partial_r2, 4)
)
print(partial_r2_table)

vif(full_model)

simplified_model <- lm(price_euros ~ ram + inches*weight + storageamount + CPU_freq,
             data = laptop_prices_modified)
vif(simplified_model, type = 'predictor')
summary(simplified_model)




