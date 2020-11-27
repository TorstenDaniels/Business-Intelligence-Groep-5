library(dplyr)
library(stringr)
library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(rjson)

df <- read.csv("fullsegment_sales.csv")
df%>%
  mutate(sales = as.numeric(as.character(sales)),
         year = as.factor(year)) -> df

write.csv2(df, "full_segment_sales.csv", row.names = F)
