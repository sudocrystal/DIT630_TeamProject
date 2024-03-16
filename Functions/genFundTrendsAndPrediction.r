library(readr)
library(ggplot2)

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")

genFunds <- function(df) {
    # reduce dataset to only general funds
    # and group by institution, year, and function
    df <- df %>% filter(Fund=="General Fund") %>%
        group_by(AcadYear, Institution, Fund, Function.Type) %>%
        summarise(Total=sum(Amount), .groups = "drop") %>%
        as.data.frame()

    # print(head(df, 10))
    # scatterplot <- ggplot(df, aes(x=AcadYear, y=Total, color=Institution)) + 
    #                geom_point()
    # print(scatterplot)
    # ggsave(path="figures", filename="Gen Funds by Year.png")


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
            geom_smooth(alpha=0.3, method="lm")
        print(gg)
        ggsave(path="figures/Gen Funds by College", filename=paste("Gen Funds - ", one_college, ".png"))
    }
}

genFunds(college_df)