## Statscannabis Jan 25 - Apr 24, 2018

setwd("/Users/philipsmith/Documents/R/statscannabis_apr_24_2018")
library(dplyr)
library(ggplot2)
library(mosaic)
library(lubridate)
library(tidyverse)

## Read the statscannabis raw data file
d0 <- read.csv("data/April24_EN_rPS.csv", header = TRUE, 
  colClasses= c("character","numeric","numeric", "factor",
  "factor","factor","factor","factor","numeric"), 
  sep = ",", skip = 2)

## Calculate Value and add it to the database
d0 <- dplyr::mutate(d0,Value=Price*Quantity)

## Convert date variable from char to date
d0$SubDate <- as.Date(d0$SubDate, format="%d/%m/%Y") 

## Filter out the bad data
d1 <- filter(d0,Quantity>0.99)
# d2 <- filter(d1,Price>1.5)
# d3 <- filter(d2,Price<=20.0)
# d4 <- filter(d3,Consumption>0.05)
# d5 <- filter(d4,Consumption<150)
# d6 <- filter(d5,Usage!="")
# d7 <- filter(d6,Quality=="High" | Quality=="Low" | Quality=="Medium")

## Create SubDate as a Date class variable and sort by SubDate
## d8 <- mutate(d1,SubDate = as.Date(SubDate,"%d/%m/%Y"))

## Sort data frame by SubDate
d9 <- arrange(d1,SubDate) # Make sure rows are in order by date

## Limit the analysis to April
d1 <- filter(d9,SubDate >= as.Date("2018-04-01"))
## do a box and whiskers plot
g <- ggplot(d1, aes(x = Province, y = Price), fill=Province) + 
  geom_boxplot(colour = "#757575", alpha = 0.8) + scale_fill_brewer(palette = "Set2") + 
  labs(title = "", subtitle = "Price distribution box and whiskers plots, April 1-24, 2018", caption = "Source: Statscannabis  |  Statistics Canada", 
       x = "Province or territory", y = "Dollars per gram") # + theme_lab() + theme(panel.grid.major.x = element_blank(), legend.position = "none")
g
ggsave("Box_and_whiskers_by_prov_April.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

## Limit the analysis to March
d1 <- filter(d9,SubDate>=as.Date("2018-03-01") & SubDate<as.Date("2018-04-01"))
## do a box and whiskers plot
g <- ggplot(d1, aes(x = Province, y = Price), fill=Province) + 
  geom_boxplot(colour = "#757575", alpha = 0.8) + scale_fill_brewer(palette = "Set2") + 
  labs(title = "", subtitle = "Price distribution box and whiskers plots, March 1-31, 2018", caption = "Source: Statscannabis  |  Statistics Canada", 
       x = "Province or territory", y = "Dollars per gram") # + theme_lab() + theme(panel.grid.major.x = element_blank(), legend.position = "none")
g
ggsave("Box_and_whiskers_by_prov_March.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

## Limit the analysis to February
d1 <- filter(d9,SubDate>=as.Date("2018-02-01") & SubDate<as.Date("2018-03-01"))
## do a box and whiskers plot
g <- ggplot(d1, aes(x = Province, y = Price), fill=Province) + 
  geom_boxplot(colour = "#757575", alpha = 0.8) + scale_fill_brewer(palette = "Set2") + 
  labs(title = "", subtitle = "Price distribution box and whiskers plots, February 1-28, 2018", caption = "Source: Statscannabis  |  Statistics Canada", 
       x = "Province or territory", y = "Dollars per gram") # + theme_lab() + theme(panel.grid.major.x = element_blank(), legend.position = "none")
g
ggsave("Box_and_whiskers_by_prov_February.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

## Limit the analysis to January
d1 <- filter(d9,SubDate>=as.Date("2018-01-25") & SubDate<as.Date("2018-02-01"))
## do a box and whiskers plot
g <- ggplot(d1, aes(x = Province, y = Price), fill=Province) + 
  geom_boxplot(colour = "#757575", alpha = 0.8) + scale_fill_brewer(palette = "Set2") + 
  labs(title = "", subtitle = "Price distribution box and whiskers plots, January 25-31, 2018", caption = "Source: Statscannabis  |  Statistics Canada", 
       x = "Province or territory", y = "Dollars per gram") # + theme_lab() + theme(panel.grid.major.x = element_blank(), legend.position = "none")
g
ggsave("Box_and_whiskers_by_prov_January.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

## Calculate and plot a daily average Canada-level price
d1 <- filter(d9,SubDate>=as.Date("2018-01-25") & SubDate<as.Date("2018-05-01"))
d11 <- group_by(d1,SubDate)
sum_d11 <- summarize(d11,Price=mean(Price))
g <- ggplot(sum_d11,aes(x=SubDate,y=Price)) + geom_line() + geom_smooth(se=FALSE) +
  labs(title="Daily Canada-level average cannabis price",
  x="Day starting Jan 25, 2018", y="Average price in dollars per gram")
g
ggsave("Daily Canada cannabis average price.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

sum_d12 <- summarize(d11,Consumption=mean(Consumption))
head(sum_d12)
g <- ggplot(sum_d12,aes(x=SubDate,y=Consumption)) + geom_line() +
  labs(title="Daily Canada-level average monthly cannabis consumption",
       x="Day starting Jan 25, 2018", y="Average monthly consumption in grams")
g
ggsave("Daily Canada cannabis average monthly cannabis consumption.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

sum_d13 <- summarize(d11,Quantity=mean(Quantity))
head(sum_d13)
g <- ggplot(sum_d13,aes(x=SubDate,y=Quantity)) + geom_line() +
  labs(title="Daily Canada-level average transaction quantity",
       x="Day starting Jan 25, 2018", y="Average transaction quantity in grams")
g
ggsave("Daily Canada cannabis average transaction quantity.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

## Calculate and plot a weekly average Canada-level price
d1 <- filter(d9,SubDate>=as.Date("2018-01-25") & SubDate<as.Date("2018-05-01"))
d14 <- mutate(d1,SubWeek = week(SubDate))
d15 <- group_by(d14,SubWeek)
sum_d15 <- summarize(d15,Price=mean(Price))
g <- ggplot(sum_d15,aes(x=SubWeek,y=Price)) + geom_line() +
  labs(title="Weekly Canada-level average cannabis price",
       x="Week of the year 2018", y="Average price in dollars per gram")
g
ggsave("Weekly Canada cannabis average price.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

## Calculate and plot a monthly average Canada-level price
d1 <- filter(d9,SubDate>=as.Date("2018-01-25") & SubDate<as.Date("2018-05-01"))
d16 <- mutate(d1,SubMonth = month(SubDate))
d17 <- group_by(d16,SubMonth)
sum_d17 <- summarize(d17,Price=mean(Price))
g <- ggplot(sum_d17,aes(x=SubMonth,y=Price)) + geom_line() +
  labs(title="Monthly Canada-level average cannabis price",
       x="Month of the year 2018", y="Average price in dollars per gram")
g
ggsave("Monthly Canada cannabis average price.png",g,height=5,width=8,dpi=500) # 5x5_500dpi


ggplot(d1,aes(x=Price)) + geom_density(bw=0.50, 
  colour="blue", fill="red") + 
  labs(title="Skewed cannabis price frequency density",
  x="Price per gram ($)", y="Relative frequency density") +
  theme(plot.title=element_text(colour="black",size=14),
  plot.background=element_rect("bisque"))

d1 <- filter(d9,SubDate>=as.Date("2018-02-25") & SubDate<as.Date("2018-05-01"))
g <- ggplot(d1,aes(x=SubDate)) + geom_bar(colour="blue",fill="red") + 
  geom_vline(aes(xintercept = as.Date("2018-03-09"))) +
  geom_vline(aes(xintercept = as.Date("2018-04-13"))) +
  geom_vline(aes(xintercept = as.Date("2018-04-18"))) +
  geom_text(label="Update",x=as.Date("2018-04-13"),y=130) +
  geom_text(label="NCS",x=as.Date("2018-04-18"),y=130) +
  geom_text(label="Update",x=as.Date("2018-03-09"),y=130) +
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) + 
  labs(title="Statscannabis submissions, Feb 25 to Apr 24",x="Submission date") +
  theme(plot.title=element_text(color="black",size=12),plot.background=element_rect("bisque"))
g

ggsave("Number of daily submissions Feb 25 to Apr 24.png",g,height=5,width=8,dpi=500) # 5x5_500dpi

d1 <- filter(d9,SubDate>=as.Date("2018-01-25") & SubDate<as.Date("2018-05-01"))
ggplot(d1,aes(x=SubDate)) + geom_bar(colour="blue",fill="red") + 
  theme(axis.text.x = element_text(angle = -45, vjust = 1, 
  hjust = 0)) + 
  labs(title="Statscannabis submissions peaked on January 30",x="Submission date") +
  theme(plot.title=element_text(colour="black",size=12),plot.background=element_rect("bisque"))

ggplot(d1,aes(x=Consumption)) + 
  geom_density(bw=5,colour="blue", fill="red") + 
  labs(title="Monthly consumption generally one ounce (28 grams) or less",
  x="Consumption in grams",y="Relative frequency density") +
  theme(plot.title=element_text(colour="black",size=11),
  plot.background=element_rect("bisque"))

d9a <- group_by(d9, Province)
d9b <- summarise(d9a, AvgPrice = mean(Price))
d9c <- arrange(d9b,desc(AvgPrice))
# Ordered point plot of average price by province
ggplot(d9c,aes(reorder(Province,AvgPrice),AvgPrice)) + 
  geom_bar(stat="identity",position="identity",colour="blue",fill="red") + 
  geom_text(aes(label=sprintf("%0.2f", round(AvgPrice,digits=2))),
  vjust=1.6,colour="black",fontface="bold",size=3) +
  labs(title="Average cannabis price highest in the north",
       x = "Province or territory", y = "Average price per gram ($)") +
  theme(plot.title=element_text(colour="black",size=14),plot.background=element_rect("bisque"))

ggplot(d9,aes(x=Price,y=Quantity)) + 
  geom_point(colour="red",size=0.5) + 
  labs(title="Demand/supply scatterplot for cannabis",
  x="Price per gram ($)",y="Quantity bought/sold in grams") +
  theme(plot.title=element_text(colour="black",size=12),plot.background=element_rect("bisque"))

ggplot(d9,aes(x=Reason,y=Price)) + 
  geom_bar(stat="summary_bin",fun.y=mean,colour="blue",fill="red") + 
  theme(plot.title=element_text(colour="black",size=14),
  plot.background=element_rect("bisque"),
  axis.text.x = element_text(angle = -10, vjust = 1, 
  hjust = 0)) + 
  labs(title="Medical cannabis costs more than recreational", 
  x = "Reason for usage", y = "Average price per gram ($)")

ggplot(d7,aes(x=Quality,y=Price)) + 
  geom_bar(stat="summary_bin",fun.y=mean,colour="blue",fill="red") + #binwidth=1, colour="blue") + 
  labs(title="Low quality cannabis costs more than high quality", 
  x = "Quality of cannabis bought/sold", y = "Average price per gram ($)") +
  theme(plot.title=element_text(colour="black",size=13),plot.background=element_rect("bisque"))

ggplot(d7,aes(x=Province,fill=Quality)) + 
  geom_bar() +
  labs(title="Very few reported transactions in low quality cannabis",
       x="Province or territory") +
  theme(plot.title=element_text(colour="black",size=11),plot.background=element_rect("bisque"))

ggplot(d7,aes(x=Frequency,y=Price)) + 
  geom_bar(stat="summary_bin",fun.y=mean,colour="blue",fill="red") + #binwidth=1, colour="blue") + 
  theme(axis.text.x = element_text(angle = -10, vjust = 1, 
  hjust = 0),plot.title=element_text(colour="black",size=13),plot.background=element_rect("bisque")) +  
  labs(title="Cannabis costs less when bought more frequently",
  x = "Frequency of purchase/sale", y = "Average price per gram ($)")

ggplot(d1,aes(x=Province,fill=Frequency)) + 
  geom_bar() +
  labs(title="Most submissions by frequent consumers",
  x="Province or territory") +
  theme(plot.title=element_text(colour="black",size=13),plot.background=element_rect("bisque"),legend.position="bottom")

ggplot(d1,aes(x=Province,fill=Reason)) + 
  geom_bar() +
  labs(title="Most submissions by recreational consumers",
  x="Province or territory") +
  theme(plot.title=element_text(colour="black",size=13),
  plot.background=element_rect("bisque"))

d10 <- group_by(d9, City)
d11 <- summarise(d10, AvgPrice = mean(Price), NumInCity = n())
d12 <- filter(d11,NumInCity>100)
d13 <- arrange(d12,desc(NumInCity))
# Ordered point plot of submission counts by top cities
ggplot(d13,aes(NumInCity,reorder(City,NumInCity))) + 
  geom_point(colour="blue",fill="red",shape=21,size=3) + 
  labs(title="Toronto and Montreal with most submissions",
  x = "Number of submissions", y = "City") +
  theme(plot.title=element_text(colour="black",size=13),
  plot.background=element_rect("bisque"))

# Ordered point plot of average price by top cities
g <- ggplot(d13,aes(AvgPrice,reorder(City,AvgPrice))) + 
  geom_point(colour="blue",fill="red",shape=21,size=3) +
  labs(title="Toronto has the highest average price per gram",
  subtitle="among top reporting cities",
  x = "Average price per gram ($)", y = "City") +
  theme(plot.title=element_text(colour="black",size=13),
  plot.background=element_rect("bisque"))
g
ggsave("5x5_500dpi.png",g,height=5,width=8,dpi=500)
