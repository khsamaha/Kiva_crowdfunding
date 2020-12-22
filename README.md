
---
title: "Data Science for Good: Kiva Crowdfunding"
author: "Kheirallah Samaha"
date: "May 21, 2018"
output: 
  html_document:
    number_sections: true
    theme: spacelab
    toc: true
    code_folding: hide
    fig_width: 6
    fig_height: 4
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


### About Kiva {-}

  Kiva is an international nonprofit, founded in 2005 and based in San Francisco, with a mission to connect people through lending to alleviate poverty. We celebrate and support people looking to create a better future for themselves, their families and their communities.
  
  By lending as little as $25 on Kiva, anyone can help a borrower start or grow a business, go to school, access clean energy or realize their potential. For some, it's a matter of survival, for others it's the fuel for a life-long ambition.
  
  100% of every dollar you lend on Kiva goes to funding loans. Kiva covers costs primarily through optional donations, as well as through support from grants and sponsors.

**It's a loan, not a donation**
  We believe lending alongside thousands of others is one of the most powerful and sustainable ways to create economic and social good. Lending on Kiva creates a partnership of mutual dignity and makes it easy to touch more lives with the same dollar. Fund a loan, get repaid, fund another.
### Objective of the kernel{-}
  To get a better idea and understanding of the data provided by Kiva.
  so Let us first import the data using <read.csv funtion
### Import Data {-}
```{r Kiva}
#setwd("C:/Users/ksamaha/Desktop/kiva-crowdfunding")
Kiva_imp_df <- read.csv("kiva_loans.csv")
loan_theme_imp_df <- read.csv("loan_theme_ids.csv")
```
### loading Libraries {-}
Then Let us import the necessary libraries
- tidyverse
- ggplot2
- gridExtra
```{r libraries, echo=FALSE}
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(DT))
```
let us check is any dublicates in ID Feature, and then check the missing data
```{r diff}
#summary(Kiva_imp_df)
anyDuplicated(Kiva_imp_df$id)
```
No duplicates
Now im goint add a new feature to see the diffrence between loan_amount and funded_amoun, just curiosity
```{r new feature}
Kiva_imp_df <- Kiva_imp_df %>%
  mutate(diff = Kiva_imp_df$loan_amount-Kiva_imp_df$funded_amount)
head(Kiva_imp_df)
```
```{r diff2}
Kiva_theme <-  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
   theme(
plot.title = element_text(size=10),
axis.title.x = element_text(size=10),
axis.title.y = element_text(size=10))
diff_less_0 <- Kiva_imp_df %>% select(diff,loan_amount,funded_amount,sector,activity)%>%
  filter(diff < 0)%>%
  ggplot(aes(x=diff))+
  geom_histogram(bins = 100, fill = "steelblue")+
  ggtitle("Difference of Loan Ammount \n and Funded Amount, \n less than 0")+
  labs(x="Difference",
       y="Count")+
   Kiva_theme
diff_0_5000 <-  Kiva_imp_df %>% select(diff,loan_amount,funded_amount,sector,activity)%>%
  filter(diff > 0 &
    diff < 5000)%>%
  ggplot(aes(x=diff))+
  geom_histogram(bins = 100, fill = "steelblue")+
  ggtitle("Difference of Loan Ammount \n and Funded Amount, \n Grt 0 less 5000")+
  labs(x="Difference",
       y="Count")+
   Kiva_theme
diff_4000_5000 <- Kiva_imp_df %>% select(diff,loan_amount,funded_amount,sector,activity)%>%
  filter(diff >= 4000 &
         diff <= 5000)%>%
  ggplot(aes(x=diff))+
  geom_histogram(bins = 50, fill = "steelblue")+
  ggtitle("Difference of Loan Ammount \n and Funded Amount, \n 4000 to 5000")+
  labs(x="Difference",
       y="Count")+
  Kiva_theme
diff_5000_10000 <- Kiva_imp_df %>% select(diff,loan_amount,funded_amount,sector,activity)%>%
  filter(diff > 5000 &
         diff <= 10000)%>%
  ggplot(aes(x=diff))+
  geom_histogram(bins = 50, fill = "steelblue")+
  ggtitle("Difference of Loan Ammount \n and Funded Amount, \n Grt 5000 to 10000")+
  labs(x="Difference",
       y="Count")+
 Kiva_theme
grid.arrange(diff_less_0,diff_0_5000,diff_4000_5000,diff_5000_10000,ncol=2)
Kiva_imp_df[Kiva_imp_df$diff < 0,1:8]
```
Distribution of loans by Sector:
```{r}
sec_loan <- Kiva_imp_df %>% select(sector,loan_amount)%>%
  group_by(sector)%>%
  summarise(sum_ta = sum(loan_amount))%>%
  mutate(perct = sum_ta/sum(Kiva_imp_df$loan_amount)*100)%>%
  ggplot(aes(x= reorder(sector, perct),y = perct))+
  geom_bar(stat="identity",fill = "steelblue")+
  coord_flip()+
  geom_label(aes(label = round(perct,2)), size = 3, y= 2.5, col = "darkgreen")+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loans disribution %")+
  labs(x="Sectors",
       y="Percentage")
sec_loan
```
Agriculture 25.3 % has the highest number of loans followed by food 21.51% and retail 17.35%. Now let us look at the loan details at activity level.
```{r}
act_loan <- Kiva_imp_df %>% select(activity,loan_amount)%>%
  group_by(activity)%>%
  summarise(sum_ta = sum(loan_amount))%>%
  mutate(perct = sum_ta/sum(Kiva_imp_df$loan_amount)*100)%>%
  filter(perct>=2.5)%>%
  ggplot(aes(x= reorder(activity, perct),y = perct))+
  geom_bar(stat="identity",fill = "steelblue")+
  coord_flip()+
  geom_label(aes(label = round(perct,2)), size = 3, y= 2, col = "darkgreen")+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loans disribution VS Activities %")+
  labs(x="Activities",
       y="Percentage")
act_loan
```
Farming 9.14% has the highest number of loans followed by General Store 6.8% and again Agriculture 4.85%. Now let us look at the loan details at countries level.
```{r}
country_loan <- Kiva_imp_df %>% select(country,loan_amount,sector)%>%
  group_by(country,sector)%>%
  summarise(sum_ta = sum(loan_amount))%>%
  mutate(perct = sum_ta/sum(Kiva_imp_df$loan_amount)*100)%>%
  filter(perct >= 1)%>%
  ggplot(aes(x= reorder(country, perct),y = perct))+
  geom_bar(stat="identity",fill = "steelblue")+
  geom_text(aes(label = round(perct,2)), size = 3.5, y= 0.5, col = "white", angle = 90, fontface="bold")+
  facet_wrap(~sector)+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loans disribution by countries VS Sectors %")+
  labs(x="Country",
       y="Percentage")
country_loan
```
Philippines - Retail 3.4%
Kenya - Agriculture 3.11%
Philippines Food 2.61%
Philippines Agriculture 2.11
**So let me create a new data frame for loan distribution by countries**
```{r}
kiva_plot_country <- Kiva_imp_df %>% select(country,loan_amount,sector)%>%
  group_by(country)%>%
  summarise(sum_ta = sum(loan_amount))%>%
  mutate(perct = sum_ta/sum(Kiva_imp_df$loan_amount)*100)
```
#### Loans disribution by countries {.tabset .tabset-fade .unnumbered} 
##### Greater than or equal to 2.5% {-}
```{r}
kiva_plot_country%>%
  filter(perct >= 2.5)%>%
  ggplot(aes(x= reorder(country, perct),y = perct))+
  geom_bar(stat="identity",fill = "steelblue")+
  coord_flip()+
  geom_label(aes(label = round(perct,2)), size = 3, y= 2, col = "darkgreen")+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loans disribution by countries\n Grt than or equal to 2.5%")+
  labs(x="Country",
       y="Percentage")
```
##### less than 2.5 and greater or equal to 0.5% {-}
```{r}
kiva_plot_country %>%
  filter(perct < 2.5 & perct >= 0.5) %>%
  ggplot(aes(x= reorder(country, perct),y = perct))+
  geom_bar(stat="identity",fill = "steelblue")+
  coord_flip()+
  geom_label(aes(label = round(perct,2)), size = 1.5, y= 0.4, col = "darkgreen")+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loans disribution by countries\n less than 2.5 and grt or equal to 0.5%")+
  labs(x="Country",
       y="Percentage")
```
##### less than 0.5 greater than or equal to 0.1% {-}
```{r}
kiva_plot_country %>%
  filter(perct < 0.5 & perct >= 0.1) %>%
  ggplot(aes(x= reorder(country, perct),y = perct))+
  geom_bar(stat="identity",fill = "steelblue")+
  coord_flip()+
  geom_label(aes(label = round(perct,2)), size = 2, y = 0.25, col = "darkgreen")+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loans disribution by countries\n less than 0.5 grt than or equal to 0.1%")+
  labs(x="Country",
       y="Percentage")
```
##### less than 0.1% {-}
```{r}
kiva_plot_country %>%
  filter(perct < 0.1) %>%
  ggplot(aes(x= reorder(country, perct),y = perct))+
  geom_bar(stat="identity",fill = "steelblue")+
  coord_flip()+
  geom_label(aes(label = round(perct,2)), size = 2, y= 0.025, col = "darkgreen")+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loans disribution by countries\n less than 0.1%")+
  labs(x="Country",
       y="Percentage")
```
-----------
Table using DT package, and note that you may find different ranking ! it is just rounding effect..
```{r}
kiva_plot_country$perct <- round(kiva_plot_country$perct,2)
datatable(
  kiva_plot_country %>%
         arrange(desc(round(perct,2))),
  options = list(pageLength = 10, 
                 dom = "tip"), rownames = FALSE
) %>% formatStyle(c("country","perct"),  color = "white", backgroundColor = "Steelblue", fontWeight = "bold")%>%
formatStyle(c("sum_ta"),  color = "darkgreen", backgroundColor = "white")
```
Philippines 9.79% has the highest number of loans followed by Kenya 6.11% and USA 5.57. Now let us look at the loan details at countries VS Sector
I am sure there is onther way to manage the labels issue in the 3rd plot, let me worry about it later..geom_text is not an option for now :) split the the table again?, maybe?
### Loan Grouping and distributions {-}
```{r}
summary(Kiva_imp_df$loan_amount)
amtbreaks <- c(25,100,200,500,1000,1250,2000,2250,3000,3250,4000,4250,5000,10000)
amtlabels <- c("25-100","101-200","201-500","501-1000","1001-1250","1251-2000","2001-2250",
               "2251-3000","3001-3250","3251-4000","4001-4250","4251-5000","5001-10000")
Kiva_imp_df$loan_amount_group <- cut(Kiva_imp_df$loan_amount, 
                                breaks = amtbreaks, 
                                right = FALSE, 
                                labels = amtlabels)
```
```{r}
loan_amt_group <- Kiva_imp_df %>% select(country,loan_amount,sector,loan_amount_group)%>%
  filter (!is.na(loan_amount_group))%>%
  group_by(loan_amount_group)%>%
  summarise(count_grp = n())%>%
  arrange(desc(count_grp))%>%
  ggplot(aes(x= reorder(loan_amount_group, count_grp),y = count_grp))+
  geom_bar(stat="identity",fill = "steelblue")+
  coord_flip()+
  geom_label(aes(label = round(count_grp,2)), size = 3, y= 30000, col = "darkgreen")+
  theme_minimal()+
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 1))+
  ggtitle("Loan Amount Distributions  %")+
  labs(x="Loan amount group",
       y="Count")
loan_amt_group
```
NO comments for now...
stay tuned!
To be contunied
