library(readr)
laptop_prices_modified <- read_csv("laptop_prices_modified.csv")

subset_columns <- c("inches", "ram", "weight", "storageamount", "storagetype") # "company"

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
  price_euros ~ inches + ram + weight + storageamount + storage_type_factor + company_factor,
  data = laptop_prices_modified
)
summary(full_model)

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



