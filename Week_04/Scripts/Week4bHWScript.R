# This is Kent's script for week 4 homework assignment b.
# Date: 2024-09-22

# 1. Load necessary packages

library(tidyverse)
library(here)


# 2. Load the data

chem_data <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))

chem_data_clean <- chem_data %>%
 filter(complete.cases(.))

view(chem_data_clean)


# 3. Separate the Tide_time Column
chem_data_separated <- chem_data_clean %>%
  separate(Tide_time, into = c("tide", "time_of_day"), sep = "_")

view(chem_data_separated)

# 4. Filter a subset of the Data
diffuse_samples <- chem_data_separated %>%
  filter(Zone == "Diffuse")

view(diffuse_samples)

# 5. Reshape data from wide to long format
diffuse_samples_long <- diffuse_samples %>%
  pivot_longer(
    cols = c(NN, Phosphate, Silicate),
    names_to = "chemical",
    values_to = "measurement"
  )

view(diffuse_samples_long)

# 6. Calculate and export summary stats
diffuse_summary_stats <- diffuse_samples_long %>%
  group_by(Site, tide, chemical) %>%
  summarise(
    mean_measurement = mean(measurement, na.rm = TRUE),
    sd_measurement = sd(measurement, na.rm = TRUE),
    .groups = 'drop' # Recommended to drop grouping after summary
  )

view(summary_stats)

# Export the summary statistics to a csv file
write_csv(
  diffuse_summary_stats,
  here("Week_04", "Outputs", "chemistry_diffuse_summary_stats.csv")
)


# 7. Create and export plot

chem_plot <- ggplot(diffuse_summary_stats, aes(x = chemical, y = mean_measurement, fill = tide)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = mean_measurement - sd_measurement, ymax = mean_measurement + sd_measurement),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  facet_wrap(~ Site, scales = "free_y") +
  labs(
    title = "Mean Chemical Concentrations by Tide Level During the Day",
    subtitle = "Error bars represent one standard deviation",
    x = "Chemical",
    y = "Mean Measurement (units)",
    fill = "Tide Level"
  ) +
  theme_light() +
  scale_fill_brewer(palette = "Set1") # used a colorblind-friendly palette

chem_plot #view plot

# save the plot to the outputs folder.
ggsave(
  here("Week_04", "Outputs", "mean_chemical_plot.png"),
  plot = chem_plot,
  width = 8,
  height = 6,
  dpi = 300
)

