library(readr)
library(ggplot2)

college_df <- read.csv("Community_Colleges_-_Budget__Revenue__Expenditure_-_Detail_Data.csv")

# Analysis of the amount
summary(college_df$Amount)
ggplot(college_df,aes(y=Amount)) + geom_boxplot()
ggsave(path="figures", filename="amount_analysis.png")

# Analysis of the fund
ggplot(college_df, aes(x=Fund)) +
  geom_histogram(color="black",fill="white", stat = "count") 
ggsave(path="figures", filename="fund_analysis.png", height = 10, width =16)

# Analysis of the source of funding
ggplot(college_df, aes(x=Object.Source)) +
  geom_histogram(color="black",fill="white", stat = "count") 
ggsave(path="figures", filename="fund_source_analysis.png", height = 10, width =16)

