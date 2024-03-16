library(readr)
library(ggplot2)
library(dplyr)

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")

genFunds <- function(df) {
    # reduce dataset to only general funds
    # and group by institution, year, and function
    df <- df %>% filter(Fund=="General Fund") %>%
        group_by(AcadYear, Institution, Fund, Function.Type) %>%
        summarise(Total=sum(Amount), .groups = "drop") %>%
        as.data.frame()

    # print(head(df, 10))
    scatterplot <- ggplot(df, aes(x=AcadYear, y=Total, color=Institution)) + 
                   geom_point()
    # print(scatterplot)
    ggsave(path="figures/Gen Funds by College", filename="__Gen Funds by Year.png")


    years <- sort(unique(df[, 1]))
    colleges <- sort(unique(df[, 2]))

    for (i in 1:11) {
        # Look at data for 1 college
        one_college <- colleges[i]
        # print(one_college)
        one_df <- df %>% filter(Institution==one_college)
        # print(head(one_df))

        gg <- ggplot(one_df, aes(x=AcadYear, y=Total, color=Function.Type)) + 
            geom_point() + 
            geom_smooth(method="lm")
        print(gg)
        ggsave(path="figures/Gen Funds by College", filename=paste("Gen Funds - ", one_college, ".png"))
    }
}

revenue <- function(df) {
    # convert years to numbers for linear model
    allYears <- sort(unique(df[, 1]))
    yrNum <- 2007:2021
    yearsVLookUp <- data.frame(yrStr=allYears, yrNum)
    # print(head(yearsVLookUp))
    lookUp <- setNames(yearsVLookUp$yrNum, yearsVLookUp$yrStr)
    # print(lookUp['2015-16'])

    df <- df %>% 
    mutate_if(names(.) %in% c("AcadYear"),
        function(x) lookUp[x])

    # reduce dataset to only general funds
    # and group by institution, year, and function
    df <- df %>% filter(Fund=="General Fund") %>%
        filter(Function.Type=="Revenue Function Code") %>%
        group_by(AcadYear, Institution, Fund, Function.Type) %>%
        summarise(Total=sum(Amount), .groups = "drop") %>%
        as.data.frame()

    scatterplot <- ggplot(df, aes(x=AcadYear, y=Total, color=Institution)) + 
                   geom_point()
    # print(scatterplot)
    ggsave(path="figures/Revenues", filename="__Revenues by Year.png")

    colleges <- sort(unique(df[, 2]))

    for (i in 1:11) {
        # Look at data for 1 college
        one_college <- colleges[i]
        one_df <- df %>% filter(Institution==one_college)

        gg <- ggplot(one_df, aes(x=AcadYear, y=Total)) + 
            geom_point() + 
            geom_smooth(method="lm")
        print(gg)
        ggsave(path="figures/Revenues", filename=paste("Revenue - ", one_college, ".png"))

        print(one_college)
        print(head(one_df))
        model <- lm(Total ~ AcadYear, data=one_df)
        print(summary(model))
    }
}

# genFunds(college_df)
revenue(college_df)