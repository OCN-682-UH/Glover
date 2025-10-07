# this is my week 7 in class script going over maps

# install libs
install.packages(c("maps", "mapproj", "mapdata"))

#load libs
library(tidyverse)
library(here)
library(maps)
library(mapproj)
library(mapdata)

# Read in data on population in California by county
popdata<-read_csv(here("Week_07","data","CApopdata.csv"))
#read in data on number of seastars at different field sites
stars<-read_csv(here("Week_07","data","stars.csv"))