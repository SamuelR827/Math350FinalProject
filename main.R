library(readr)
library(ggplot2)
library(dplyr)
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
  price_euros ~ inches + ram + weight + storageamount + 
    storage_type_factor + company_factor + CPU_freq,
  data = laptop_prices_modified
)
summary(full_model)

quantative_model = lm(
  price_euros ~ inches + ram + weight + storageamount + CPU_freq,
  data = laptop_prices_modified
)
summary(quantative_model)

# Apply logarithmic transformation to the relevant columns and then fit the models
for (col in subset_columns) {
  print(toupper(col)) 
  transformed_data <- laptop_prices_modified
  transformed_data$log_price <- log(transformed_data$price_euros)
  if (is.numeric(transformed_data[[col]])) {
    transformed_data[[paste0("log_", col)]] <- log(transformed_data[[col]])
  }
  formula <- as.formula(paste("log_price ~", paste0("log_", col)))
  model <- lm(formula, data = transformed_data)
  fitted_values <- fitted(model)
  residuals_values <- residuals(model)
  
  # Generate diagnostics plots
  par(mfrow = c(2, 2))
  plot(model, main = paste("Diagnostics for", toupper(col)))
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



# Singular Comparison Models
weight_inches = lm(
  price_euros ~ inches + weight,
  data = laptop_prices_modified
)
summary(weight_inches)

storage_amount_type = lm(
  price_euros ~ storageamount + storage_type_factor,
  data = laptop_prices_modified
)
summary(storage_amount_type)

freq_ram = lm(
  price_euros ~ CPU_freq + ram,
  data = laptop_prices_modified
)
summary(freq_ram)

company_freq = lm(
  price_euros ~ CPU_freq + company_factor,
  data = laptop_prices_modified
)
summary(company_freq)

company_ram = lm(
  price_euros ~ ram + company_factor,
  data = laptop_prices_modified
)
summary(company_ram)


company_storage_amount = lm(
  price_euros ~ storageamount + company_factor,
  data = laptop_prices_modified
)
summary(company_storage_amount)

company_inches = lm(
  price_euros ~ inches + company_factor,
  data = laptop_prices_modified
)
summary(company_inches)

company_weight = lm(
  price_euros ~ weight + company_factor,
  data = laptop_prices_modified
)
summary(company_weight)

company_weight_inches = lm(
  price_euros ~ inches + weight + company_factor,
  data = laptop_prices_modified
)
summary(company_weight_inches)


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



