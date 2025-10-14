# Week7HW
Kent Glover
2025-10-14

## Introduction

The goal of this **HW** is to visualize the distribution of plastic
waste collected across different countries from the TidyTuesday Plastic
Dataset.

## Setup

First, load the necessary R packages. We’ll use `tidyverse` for a lot,
`sf` and `rnaturalearth` for spatial data, `here` for file path ease,
`ggspatial` for maps, and `patchwork` to combine plots.

``` r
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(here)
library(ggspatial)
library(patchwork)
```

## Load and Prep Data

Load the `plastics.csv` dataset. The dataset contains information about
plastic pollution collected during clean-up events.

Preparation steps are:

1.  Read the raw data.

2.  Group the data by country.

3.  Calculate the sum of `grand_total` plastic items for each country.

4.  Rename the `country` column to `name` to facilitate joining with the
    map data.

5.  Correct a country name from “United Kingdom of Great Britain and
    Northern Ireland” to “United Kingdom”.

``` r
#Load plastic pollution data
plastics_raw <- read_csv(here("Week_07", "Data","plastics.csv"))
```

    Rows: 13380 Columns: 14
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (2): country, parent_company
    dbl (12): year, empty, hdpe, ldpe, o, pet, pp, ps, pvc, grand_total, num_eve...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Get total plastic per country
plastic_by_country <- plastics_raw %>%
  filter(year == 2020) %>% #Focusing on the most recent year
  group_by(country) %>%
  summarise(total_plastic = sum(grand_total, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(name = country) %>% 
  #Change country name for joining with map data
  mutate(name = case_when(
    name == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
    TRUE ~ name
  ))
```

## Creating the World Map

Now, create the choropleth map.

1.  Obtain world map spatial data using the `ne_countries` function from
    the `rnaturalearth` package.

2.  Perform a left join to merge the aggregated plastic pollution data
    with the world map data based on country names.

``` r
#world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

##Join plastic data with world map data
world_plastics <- world %>%
  left_join(plastic_by_country, by = "name")
```

With the data merged, I can now plot the map using `ggplot2`. I will use
a color scale to represent the amount of plastic pollution.

``` r
map_plot <- ggplot(data = world_plastics) +
  geom_sf(aes(fill = total_plastic), color = "black", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma", 
    na.value = "grey90", 
    trans = "log10",
    name = "Total Plastic Items (log scale)",
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = 15,
      barheight = 0.5,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Global Plastic Pollution by Country (2020)",
    subtitle = "Total plastic items collected during clean-up events",
    caption = "Source: Tidy Tuesday #BreakFreeFromPlastic"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 8),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering)
```

## Top 10 Polluting Countries

To provide another perspective, I can visualize the top 10 countries
with the most plastic pollution recorded.

``` r
#bar chart for the top 10 countries
top_10_plot <- plastic_by_country %>%
  arrange(desc(total_plastic)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, total_plastic), y = total_plastic)) +
  geom_col(fill = "skyblue", color = "black") +
  coord_flip() +
  labs(
    title = "Top 10 Countries",
    x = "",
    y = "Total Plastic Items"
  ) +
  theme_minimal() +
    theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
```

## Combined View

Finally, I can combine our map and the bar chart into a single graph
using the `patchwork` package.

``` r
# Combine plots
combined_plot <- map_plot + top_10_plot +
  plot_layout(widths = c(3, 1))

#Display combined plot
combined_plot
```

    Scale on map varies by more than 10%, scale bar may be inaccurate

![Global plastic pollution map and top 10
countries.](Week7HW_files/figure-commonmark/combined-plot-1.png)

## Conclusions

The map reveals that plastic pollution is a global issue, with data
reported from countries across every continent. The bar chart further
highlights that a few countries report *significantly* higher amounts of
collected plastic waste. For a more comprehensive analysis, data from
more years and more locations would be beneficial.
