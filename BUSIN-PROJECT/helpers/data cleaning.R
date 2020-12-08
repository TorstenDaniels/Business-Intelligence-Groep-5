#libraries--------
library(dplyr);library(stringr);library(tidyverse);library(readr);library(ggplot2);library(plotly)
library(lubridate);library(ggthemes);library(RColorBrewer);library(rworldmap);library(mapproj)
library(readxl);library(GGally);library(shiny);library(shinydashboard); library(shinydashboardPlus);
library(naniar);library(ggimage); library(gghighlight)

#Transforming Stock Prices ----------
Stock <- read.csv("BMW.DE.csv")
Stock%>%
  mutate(Date = ymd(Date),
         Open = round(Open, 2),
         High = round(High, 2),
         Low = round(Low, 2),
         Close = round(Close, 2),
         Adj.Close = round(Adj.Close, 2)) -> Stock

write.csv(Stock, "data/BMW.DE.csv", row.names = F)

