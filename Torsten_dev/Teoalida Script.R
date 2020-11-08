library(readxl)
library(tibble)
library(dplyr)
library(stringr)
library(zoo)

setwd("C:/Users/Torsten Daniels/Google Drive/Universiteit/Courses/Business Intelligence (3522)/Groepswerk/Business-Intelligence-Groep-5/data")
Car_Models_Database <- read_xls("Car-Models-Database-by-Teoalida-SAMPLE.xls")
colnames(Car_Models_Database) <- ifelse(is.na(Car_Models_Database[1,]), colnames(Car_Models_Database), as.character(Car_Models_Database[1,]))
Car_Models_Database <- Car_Models_Database[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), ]
Car_Models_Database <- Car_Models_Database[Car_Models_Database$`Sold in Europe`=="Europe",]
Car_Models_Database$`Sold in Europe` <- NULL

Car_Models_Database$Class <- na.locf(Car_Models_Database$Class)
Car_Models_Database$`Launch year` <- na.locf(Car_Models_Database$`Launch year`)
Car_Models_Database$`Platform / generation` <- ifelse(str_match(Car_Models_Database$`Platform / generation`), "?", NA, Car_Models_Database$`Platform / generation`)

