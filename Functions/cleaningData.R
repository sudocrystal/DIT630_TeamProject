library(readr)

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")
# Count all missing data within the dataframe
missing_values_total <- sum(apply(college_df, 1, function(x) sum(is.na(x))))

cat("missing values total = ", missing_values_total)

