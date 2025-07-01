##### checking C:N ratios
# May 26/ 2025


#set wd
setwd("~/Desktop/BSM")

#load packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(car)


#read in data 

dat <- read.csv("all_raw_iso_finalmay26.csv")

##check C:N ratios
# what percentage is under 3.5? 
#what percentage is under 4? 
#from Post et al 2007


#check what percentage is under 4

dat %>%
  summarise(percent_under_4 = mean(CN_Ratio < 4, na.rm = TRUE) * 100)

#66.72517%



#check what percentage is under 3.5
dat %>%
  summarise(percent_under_3.5 = mean(CN_Ratio < 3.5, na.rm = TRUE) * 100)

#46.99895%


