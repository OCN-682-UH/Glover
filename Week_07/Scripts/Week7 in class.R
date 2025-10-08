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

# get data for the entire world
world<-map_data("world")
head(world)

# get data for the USA
usa<-map_data("usa")
head(usa)

# get data for italy
italy<-map_data("italy")
head(italy)

# get data for states
states<-map_data("state")
head(states)

# get data for counties
counties<-map_data("county")
head(counties)

ggplot()+
  geom_polygon(data = world, aes(x = long, y = lat, group = group))

ggplot()+
  geom_polygon(data = world, aes(x = long, y = lat))

#Mercator projection
ggplot()+
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = region),
               color = "black")+
  theme_minimal()+
  guides(fill = FALSE)+
  theme(panel.background = element_rect(fill = "lightblue"))+
  coord_map(projection = "mercator",
            xlim = c(-180,180))


#sinusoidal
ggplot()+
  geom_polygon(data = world, 
               aes(x = long,
                   y = lat,
                   group = group, 
                   fill = region),
               color = "black")+
  theme_minimal()+
  guides(fill = FALSE)+
  theme(panel.background = element_rect(fill = "lightblue"))+
  coord_map(projection = "sinusoidal",
            xlim = c(-180,180))

# Use the states dataset
head(states)

# Use the states dataset
CA_data<-states %>%
  filter(region == "california")

#cali first try
ggplot()+
  geom_polygon(data = CA_data, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = region),
               color = "black")+
  theme_minimal()+
  guides(fill = FALSE)+
  theme(panel.background = element_rect(fill = "lightblue"))+
  coord_map(projection = "mercator",
            xlim = c(-180,180))


# Use the states dataset
AL_data<-states %>%
  filter(region == "alabama")

#cali first try
ggplot()+
  geom_polygon(data = AL_data, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = region),
               color = "black")+
  theme_minimal()+
  guides(fill = FALSE)+
  theme(panel.background = element_rect(fill = "lightblue"))+
  coord_map(projection = "mercator")



ggplot()+
  geom_polygon(data = CA_data, 
               aes(x = long, 
                   y = lat, 
                   group = group), 
               color = "black")+
  coord_map()+
  theme_void()

# Look at the county data
head(counties)[1:3,] # only showing the first 3 rows for space

# Look at the county data
head(popdata)

CApop_county<-popdata %>%
  select("subregion" = County, Population)  %>% # rename the county col
  inner_join(counties) %>%
  filter(region == "california") # some counties have same names in other states
head(CApop_county)

ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),
               color = "black")+
  coord_map()+
  theme_void()


ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),  
               color = "black")+
  coord_map()+
  theme_void() +
  scale_fill_gradient(trans = "log10")

head(stars)

ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),  
               color = "black")+
  geom_point(data = stars, # add a point at all my sites 
             aes(x = long, 
                 y = lat,
                 size = star_no))+
  coord_map()+
  theme_void() +
  scale_fill_gradient(trans = "log10")


ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),  
               color = "black")+
  geom_point(data = stars, # add a point at all my sites 
             aes(x = long, 
                 y = lat,
                 size = star_no))+ 
  coord_map()+
  theme_void() +
  scale_fill_gradient(trans = "log10")+
  labs(size = "# stars/m2")



# Title: Creating Maps in R with ggmap using Stadia Maps and Google Maps
# Description: This script provides a step-by-step guide to generating maps
#              using two different providers within the R environment. It covers
#              package installation, API key setup, fetching map tiles, and
#              plotting data points with ggplot2 syntax.
# STEP 1: INSTALL AND LOAD PACKAGES

# install.packages("ggplot2")
# install.packages("ggmap")

# Load the libraries for this session
library(ggplot2)
library(ggmap)


# --------------------------------------------------------------------------
# STEP 2: API KEY SETUP (IMPORTANT!)
# --------------------------------------------------------------------------
# Both Stadia Maps and Google Maps require an API key to use their services.

# --- Stadia Maps API Key ---
# 1. Go to your Stamen account dashboard: https://my.stadiamaps.com/
# 2. If you don't have an account, sign up for a free one.
# 3. Create a new property and copy your API key.
# 4. Paste your key below.
stadia_api_key <- ""
register_stadiamaps(stadia_api_key)


# --- Google Maps API Key ---
# 1. Go to the Google Cloud Platform Console: https://console.cloud.google.com/
# 2. Create a new project.
# 3. Enable the "Maps Static API" and "Geocoding API".
# 4. Set up a billing account (required by Google, but they have a generous free tier).
# 5. Create and copy your API key.
# 6. Paste your key below.
google_api_key <- ""
register_google(google_api_key)


# --------------------------------------------------------------------------
# STEP 3: CREATE A MAP WITH STADIA MAPS
# --------------------------------------------------------------------------
# Stadia offers several beautiful map styles. We'll use the 'stamen_toner_lite'.

# Define a location of interest
location_sf <- "Golden Gate Bridge, San Francisco"

# Use geocode to get the latitude and longitude for our location
# This function uses the Google Geocoding API, which we enabled in Step 2.
coords_sf <- geocode(location_sf)

# Fetch the map tiles from Stadia Maps
# Note: get_stadiamap() is the function for Stadia maps.
# maptype can be: stamen_toner, stamen_toner_lite, stamen_watercolor, etc.
map_stadia <- get_stadiamap(
  bbox = c(left = -122.52, bottom = 37.79, right = -122.42, top = 37.83),
  zoom = 13,
  maptype = "stamen_toner_lite"
)

# Now, let's plot the map and add a point for the Golden Gate Bridge
plot_stadia <- ggmap(map_stadia) +
  geom_point(data = coords_sf, aes(x = lon, y = lat), color = "red", size = 4, alpha = 0.8) +
  labs(
    title = "Map by Stadia Maps: Stamen Toner Lite",
    subtitle = "Golden Gate Bridge, San Francisco",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Display the plot
print(plot_stadia)


# --------------------------------------------------------------------------
# STEP 4: CREATE A MAP WITH GOOGLE MAPS
# --------------------------------------------------------------------------
# Google Maps provides familiar map styles like roadmap and satellite.

# Define a new location
location_nyc <- "Times Square, New York"

# Get the coordinates for this new location
coords_nyc <- geocode(location_nyc)

# Fetch the map tiles from Google Maps
# Note: get_googlemap() is the function for Google maps.
# maptype can be: "terrain", "satellite", "roadmap", "hybrid"
map_google <- get_googlemap(
  center = location_nyc,
  zoom = 15,
  maptype = "roadmap"
)

# Create a small data frame of points to plot around Times Square
theater_locations <- data.frame(
  name = c("New Amsterdam Theatre", "Lyceum Theatre"),
  lon = c(-73.9868, -73.9846),
  lat = c(40.7561, 40.7569)
)


# Plot the Google map and add our points
plot_google <- ggmap(map_google) +
  geom_point(data = theater_locations, aes(x = lon, y = lat), color = "blue", size = 4) +
  geom_text(data = theater_locations, aes(x = lon, y = lat, label = name), hjust = 0.5, vjust = -1, color = "black") +
  labs(
    title = "Map by Google Maps: Roadmap",
    subtitle = "Theaters near Times Square, NYC",
    x = "", # Hide axis labels for a cleaner look
    y = ""
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Display the plot
print(plot_google)

