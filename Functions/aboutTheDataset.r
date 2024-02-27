library(readr)

clear_scr <- function() cat(rep("\n", 50)) # clear the terminal
clear_scr()

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")
# print(head(college_df, n = 5))

cat("Num Variables = ", ncol(college_df))
print(colnames(college_df))

cat("Num Observations = ", nrow(college_df))

# years <- sort(unique(college_df$AcadYear))
# cat("years = ", paste(years, collapse = ", "))
# print(length(years))

# colleges <- sort(unique(college_df$Institution))
# cat("colleges = ", paste(colleges, collapse = ", "))
# print(length(colleges))

# prints summary of all data
for (i in 1:11) {
    print(colnames(college_df)[i])
    col <- sort(unique(college_df[,i]))
    if (length(col) < 20) {
        print(paste(col, collapse = ", "))
    } else {
        print("more than 20 values")
        print(paste(head(col, n = 4), collapse = ", "))
    }
    print(length(col))
}

print("Ammount Summary")
summ <- summary(college_df$Amount)
print(summ)

# These columns don't seem to match, which surprises me.
# print(all(college_df$Record.Type == college_df$Object.Type))
