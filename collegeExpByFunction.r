library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
# library(plyr)

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")
# print(head(college_df, n = 2))

collegeExpByFunction <- function(df) {
    df <- df %>% select("AcadYear", "Institution", "Function.Type", "Amount")
    print(head(df))

    # https://sparkbyexamples.com/r-programming/group-by-sum-in-r/
    sums_by_function <- df %>% group_by(AcadYear, Institution, Function.Type) %>%
        summarise(Total=sum(Amount), .groups = "drop") %>%
        as.data.frame()
    print(head(sums_by_function))

    sums_by_college <- sums_by_function %>% group_by(AcadYear, Institution) %>%
        summarise(Budget=sum(Total), .groups = "drop") %>%
        arrange(Institution) %>%
        as.data.frame()
    print(head(sums_by_college))

    scatterplot <- ggplot(sums_by_college, aes(x=AcadYear, y=Budget, color=Institution)) + 
                   geom_point()
    # print(scatterplot)
    ggsave(filename="College Budgets by Year.png")

}

collegeExpByFunction(college_df)