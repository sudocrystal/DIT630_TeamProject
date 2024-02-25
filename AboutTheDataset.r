library(readr)

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")
# print(head(college_df, n = 10))

cat("Num Variables = ", ncol(college_df))
print(colnames(college_df))
print(" ")

cat("Num Observations = ", nrow(college_df))
colleges <- sort(unique(college_df$Institution))
cat("colleges = ", colleges)