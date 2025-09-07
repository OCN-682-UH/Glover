## This is my Week 2 Script for MBIO 682.
## Created by: Kent Glover
## Created on: 2025-09-06
###############################################

#Load Libraries
library(tidyverse)
library(here)

#read in weight data
weightdata<-read_csv(here("Week_02", "Data", "weightdata.csv"))
