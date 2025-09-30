#Kent's week 5b homework assignment script

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(here)


# Read, clean, and process the conductivity and depth data in a single piped workflow
# This approach avoids creating multiple intermediate dataframes
processed_data <- read_csv(here("Week_05","data","CondData.csv")) %>%
  # 1. Convert the 'date' column to a proper datetime object
  # The mdy_hms function from lubridate is used to parse the specific date format
  mutate(date = mdy_hms(date)) %>%
  # 2. Round the datetime to the nearest 10 seconds.
  # This is crucial for aligning the conductivity data with the depth dat
  mutate(date = round_date(date, unit = "10 seconds")) %>%
  # 3. Join with the depth data.
  # An inner_join is used here, which means only rows with matching timestamps
  # in both datasets will be kept effectively removing NAs from mismatched times
  inner_join(
    read_csv(here("Week_05","data","DepthData.csv")) %>%
      # Convert the 'date' column in the depth data to a datetime object as well.
      mutate(date = ymd_hms(date)),
    by = "date"
  ) %>%
  # 4. Group the data by minute
  # The floor_date function rounds the date down to the beginning of the minute
  group_by(date_minute = floor_date(date, unit = "minute")) %>%
  # 5. Calculate the average of key variables for each minute
  summarise(
    avg_depth = mean(Depth, na.rm = TRUE),
    avg_temp = mean(Temperature, na.rm = TRUE),
    avg_salinity = mean(Salinity, na.rm = TRUE)
  )

head(processed_data)

# Plotting

# This plot shows how the average water temperature changes over time
time_vs_temp_plot <- ggplot(processed_data, aes(x = date_minute, y = avg_temp)) +
  geom_line(color = "blue", size = 1) + # Use a line plot for time-series data
  geom_point(color = "darkblue", size = 2) + # Add points to highlight the measurements
  labs(
    title = "Average Water Temperature per Minute",
    subtitle = "Data collected on Jan 15, 2021",
    x = "Time (by minute)",
    y = "Average Temperature (°C)"
  ) +
  theme_minimal() + # Use a clean and minimal theme for the plot
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

time_vs_temp_plot

# Saving outputs
ggsave(
  here("Week_05", "Outputs", "Week5b_hwplot.png"),
  time_vs_temp_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# Save the processed (averaged) data to a CSV file in  Outputs
write_csv(
  processed_data,
  here("Week_05", "Outputs", "Week5bHWOutputData.csv")
)
