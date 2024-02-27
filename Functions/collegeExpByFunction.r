library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
# library(plyr)

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")
# print(head(college_df, n = 2))

collegeBudgets <- function(df) {
    df <- df %>% select("AcadYear", "Institution", "Record.Type", "Function.Type", "Amount")
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
    #print(head(sums_by_college))
    
    ordered_df <- sums_by_college[order(-sums_by_college$Budget), ]
    
    # Print the first 5 rows of the ordered data frame
    print(head(ordered_df))

    scatterplot <- ggplot(sums_by_college, aes(x=AcadYear, y=Budget, color=Institution)) + 
                   geom_point()
    # print(scatterplot)
    ggsave(path="figures", filename="College Budgets by Year.png")

}

collegeBudgetsByYearByFunction <- function(df, year) {
    df <- df %>% select("AcadYear", "Institution", "Record.Type", "Function.Type", "Amount") %>%
                 filter(AcadYear == year)
    # print(head(df))

    # https://sparkbyexamples.com/r-programming/group-by-sum-in-r/
    sums_by_function <- df %>% group_by(AcadYear, Institution, Function.Type) %>%
        summarise(Total=sum(Amount), .groups = "drop") %>%
        as.data.frame()
    # print(head(sums_by_function))

    # Visualize as bar graphs
    barplot1 <- ggplot(data = sums_by_function, aes(x = Function.Type, y = Total, fill=as.factor(Function.Type))) +
    geom_bar(stat = "identity", alpha = 0.7) +
    facet_grid(. ~Institution)  +
    labs(y = "Amount", title = paste("Budget by Function by Institution for AY", year)) +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.length = unit(0, "mm")
    )
    # print(barplot1)
    ggsave(path="figures", filename="barplot1.png")

}

collegeBudgets(college_df)
collegeBudgetsByYearByFunction(college_df, "2021-22")