library(dplyr)
library(stringr)
library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(rjson)


df <- read_csv2("~/GitHub/Business-Intelligence-Groep-5/data/customerSatisfactionBenchmark.csv")
df$brand <- as.factor(df$brand)

df2 <- read_csv("car_database.csv")
df2$Brand <- as.factor(df2$Brand)
df2%>%
  filter(Brand == "BMW") -> test

write.csv(test, "car_BMW.csv")


df%>%
  group_by(df$year)%>%
  filter(brand == "BMW" | brand == "Kia")%>%
  ggplot()+
  geom_line(aes(year, satisfactionScore, color = brand))+
  scale_y_continuous(name = "satisfaction score", limits = c(0,100))+
  theme_economist()

test <- df%>%
  spread(brand, satisfactionScore)
