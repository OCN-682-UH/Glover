# Load necessary libraries
library(tidyverse)
library(viridis) # For the good plot color palette
library(maps)    # For the US map data
library(plotrix) # For the 3D pie chart
library(tidytuesdayR)
library(here)
library(readr)
library(ggmap)
library(leaflet) # For the interactive Good Plot
library(leaflet.extras)
library(htmltools) # For customizing leaflet popups



# Load the Tidy Tuesday Data
haunted_places<- read_csv(here("GoodplotBadplot", "Data", "haunted_places.csv"))

# Let's count hauntings by state to create some data to plot
state_counts <- haunted_places %>%
  count(state, sort = TRUE) %>%
  # Filter to top 15 to make the pie chart "readable"
  slice_head(n = 15)

# For a truly bad plot, we use the ultimate evil: a 3D pie chart in base R
pie3D(
  state_counts$n,
  labels = paste(state_counts$state, "\n(", state_counts$n, " hauntings)"),
  explode = 0.1,
  main = "!!! GHOST-O-RAMA: TOP HAUNTED STATES !!!",
  col = rainbow(nrow(state_counts)),
  labelcex = 0.8,
  font = 2,
  family = "serif" # Using a generic family for better compatibility
)

# A masterpiece of ggplot2 crime
us_map <- map_data("state")
bad_map_plot <- ggplot(haunted_places, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "black", color = "yellow") +
  geom_point(aes(color = state), size = 3, alpha = 0.5) +
  scale_y_reverse() + # Horrifyingly flip the map upside down
  theme_void() +
  labs(
    title = "EVERY GHOST IN AMERICA (MAP VIEW)",
    subtitle = "lol"
  ) +
  theme(
    plot.background = element_rect(fill = "purple"),
    plot.title = element_text(color = "orange", hjust = 0.5, family = "serif", size = 20),
    plot.subtitle = element_text(color = "orange", hjust = 0.5, family = "serif"),
    legend.position = "none" # Hide the legend with 50+ colors
  )
print(bad_map_plot)




bad_map_plot <- ggplot(haunted_places, aes(x = longitude, y = latitude)) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "black", color = "yellow") +
  # Add points with an impossible aesthetic mapping (shape = city)
  geom_point(aes(color = state, shape = city), size = 3, alpha = 0.5) +
  # Add overlapping text for every single point
  geom_text(aes(label = location), color = "white", size = 1.5, check_overlap = TRUE, family = "serif") +
  scale_y_reverse() + # Horrifyingly flip the map upside down
  facet_wrap(~ state) + # Add nonsensical faceting for maximum chaos
  coord_polar() + # Destroy all geographic meaning with polar coordinates
  labs(
    title = "EVERY GHOST IN AMERICA (RADIAL VIEW)",
    subtitle = "lol."
  ) +
  theme(
    plot.background = element_rect(fill = "pink"),
    plot.title = element_text(color = "orange", hjust = 0.5, family = "serif", size = 20),
    plot.subtitle = element_text(color = "orange", hjust = 0.5, family = "serif"),
    legend.position = "none", # The legend would be a mile long
    strip.text = element_text(color = "yellow", face = "bold"),
    strip.background = element_rect(fill = "black"),
    axis.text = element_blank(), # Hide useless polar axes text
    axis.ticks = element_blank()
  )
print(bad_map_plot)



# Using the same `haunted_places` dataframe and `us_map` from before.
# We will focus on the contiguous US for a cleaner map.
haunted_contiguous <- haunted_places %>%
  filter(longitude > -130 & longitude < -65 & latitude > 20 & latitude < 50)

# We will focus on the contiguous US for a cleaner map.
haunted_contiguous <- haunted_places %>%
  filter(longitude > -130 & longitude < -65 & latitude > 20 & latitude < 50)

# Define a bounding box for the contiguous US
us_bounds <- c(left = -125, bottom = 24, right = -67, top = 50)
register_stadiamaps(stadia_api_key)

# Get a clean, black-and-white base map
# This may take a moment to download the map tiles.
us_map_tiles <- get_stadiamap(bbox = us_bounds, zoom = 5, maptype = "stamen_toner_lite")

# Create the publication-quality plot
publication_plot <- ggmap(us_map_tiles) +
  # Use geom_density_2d to draw contour lines instead of filled polygons.
  # This shows hotspots without completely obscuring the map.
  geom_density_2d(
    data = haunted_contiguous,
    aes(x = longitude, y = latitude, color = after_stat(level)),
    linewidth = 0.6
  ) +
  # Use the same excellent, colorblind-friendly palette for the lines
  scale_color_viridis_c(option = "inferno") +
  labs(
    title = "Contour Density of Reported Haunted Locations in the U.S.",
    subtitle = "Lines connect areas of equal density, highlighting hotspots in the Northeast.",
    caption = "Data Source: Tidy Tuesday (2023, Week 44) | Stadia Maps",
    color = "Density\nLevel" # Update the legend title for color
  ) +
  # Refine the theme for a clean, final look
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(face = "italic")
  )

print(publication_plot)

# Filter for the contiguous US and create a popup content column
haunted_interactive <- haunted_places %>%
  filter(longitude > -130 & longitude < -65 & latitude > 20 & latitude < 50) %>%
  # Create a clean HTML string for our popups
  mutate(popup_info = paste0("<b>", location, "</b><br/>", 
                             "<i>", city, ", ", state_abbrev, "</i><br/><br/>",
                             description))

# Filter for the contiguous US and create a popup content column
haunted_interactive <- haunted_places %>%
  filter(longitude > -130 & longitude < -65 & latitude > 20 & latitude < 50) %>%
  # Create a clean HTML string for our popups
  mutate(popup_info = paste0("<b>", location, "</b><br/>", 
                             "<i>", city, ", ", state_abbrev, "</i><br/><br/>",
                             description))

# Create an HTML string for the title
map_title <- htmltools::tags$div(
  htmltools::tags$h3("Interactive Map of U.S. Haunted Places", style = "font-size: 20px; font-weight: bold; margin-bottom: 5px;"),
  htmltools::tags$p("Data Source: Tidy Tuesday (October 10th, 2023)", style = "font-size: 12px; font-style: italic;")
)

# Create the interactive leaflet map
interactive_map <- leaflet(haunted_interactive) %>%
  # Add a clean, light base map
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Set the initial view to the center of the US
  setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
  # Add clustered markers for exploring individual points
  addMarkers(
    lng = ~longitude, lat = ~latitude,
    popup = ~popup_info,
    clusterOptions = markerClusterOptions(),
    group = "Individual Hauntings" # Assign this layer to a group
  ) %>%
  # Add a heatmap layer as another group
  addHeatmap(
    lng = ~longitude, lat = ~latitude,
    intensity = 1,
    blur = 20, max = 0.05, radius = 15,
    group = "Density Heatmap" # Assign this layer to a group
  ) %>%
  # Add a layer control so the user can toggle overlays
  addLayersControl(
    overlayGroups = c("Density Heatmap", "Individual Hauntings"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Add the custom title box
  addControl(map_title, position = "topright")

# Print the map. In the rendered HTML, this will be a fully interactive widget.
interactive_map

