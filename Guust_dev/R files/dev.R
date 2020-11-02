library(dplyr)
library(stringr)
library(tidyverse)
library(readr)
library(ggplot2)

customerSatisfactionBenchmark <- read_delim("~/GitHub/Business-Intelligence-Groep-5/data/customerSatisfactionBenchmark.csv", ";")
str(customerSatisfactionBenchmark)
summary(customerSatisfactionBenchmark)
