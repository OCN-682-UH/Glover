# This is my week 4 homework 1 script

#load packages
library(palmerpenguins)
library(tidyverse)
library(here)

glimpse(penguins) # look at data
head(penguins)

#1. 
hw1 = penguins %>%
  drop_na(species, island, sex, body_mass_g) %>% #dropped NAs, cleaned up data
  group_by(island, sex, species) %>% # group by island, sex, and species
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE), variance = var(body_mass_g, na.rm = TRUE)) # give summary of mean and variance

view(hw1)

#2. 
hw2 = penguins %>% # use penguin dataframe
  drop_na(species, island, sex, body_mass_g) %>% #drop all NAs
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(species, island, sex, log_mass) # select certain columns to stay in result

view(hw2)

#attempt number 1 at plot
hw2plot = ggplot(hw2, aes(x = island, y = log_mass, fill = species)) + #Plot!
  geom_boxplot() +
  labs(
    x = "Island",
    y = "Log Body Mass (g)",
    title = "Female Penguin Log Body Mass Across Islands"
  ) +
  theme_minimal()

hw2plot

ggsave(
  here("Week_04", "Outputs", "hw2plot.png"),
  hw2plot,
  width = 7,
  height = 5,
  dpi = 300
)


#another try but may stick with the one above.
ggplot(hw2, aes(x = species, y = log_mass, color = island)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  labs(
    x = "Species",
    y = "Log Body Mass (g)",
    title = "Female Penguin Log Body Mass by Species and Island"
  ) +
  theme_minimal()

