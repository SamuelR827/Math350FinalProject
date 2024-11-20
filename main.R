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
