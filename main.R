library(readr)
laptop_prices_modified <- read_csv("laptop_prices_modified.csv")
View(laptop_prices_modified)

subset_columns <- c("inches", "ram", "weight", "storageamount", "storagetype") # "company"

for (col in subset_columns) {
  print(toupper(col))
  formula <- as.formula(paste("price_euros ~", col))
  model <- lm(formula, data = laptop_prices_modified)
  print(summary(model))
}

# Descriptive statistics
for (col in subset_columns) {
  print(toupper(col)) 
  column_data <- laptop_prices_modified[[col]]
  if (is.numeric(column_data)) {
    stats <- c(
      Mean = mean(column_data, na.rm = TRUE),
      Median = median(column_data, na.rm = TRUE),
      SD = sd(column_data, na.rm = TRUE),
      Min = min(column_data, na.rm = TRUE),
      Max = max(column_data, na.rm = TRUE)
    )
    print(stats)
  } else if (is.factor(column_data) || is.character(column_data)) {
    print(table(column_data))
  } else {
    print("Unsupported data type")
  }
}

#