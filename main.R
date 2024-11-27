library(readr)
laptop_prices_modified <- read_csv("laptop_prices_modified.csv")
View(laptop_prices_modified)

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

# Apply square root transformation to the relevant columns and then fit the models
for (col in subset_columns) {
  print(toupper(col)) 
  transformed_data <- laptop_prices_modified
  transformed_data$sqrt_price <- sqrt(transformed_data$price_euros)
  if (is.numeric(transformed_data[[col]])) {
    transformed_data[[paste0("sqrt_", col)]] <- sqrt(transformed_data[[col]])
  }
  formula <- as.formula(paste("sqrt_price ~", paste0("sqrt_", col)))
  model <- lm(formula, data = transformed_data)
  fitted_values <- fitted(model)
  residuals_values <- residuals(model)
  par(mfrow = c(2, 2))  # Arrange the plot layout (2x2 grid)
  plot(model, main = paste("Diagnostics for", toupper(col)))
}

