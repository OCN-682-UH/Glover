# This is my week 4 script going over dplyr

#load packages
library(palmerpenguins)
library(tidyverse)

glimpse(penguins) # look at data
head(penguins)

dplyr::filter(.data = penguins, sex == "female") #select for only females

dplyr::filter(.data = penguins, year == "2008") #select for year 2008

dplyr::filter(.data = penguins, body_mass_g > "5000") #filter for any above 5000g

filter(.data = penguins, sex == "female", body_mass_g >5000) #these both do the same
filter(.data = penguins, sex == "female" & body_mass_g >5000) # & symbol also works

dplyr::filter(.data = penguins, year == "2008" | year == "2009") #had to take out quotes so each year is a number, filters for both 2008 and 2009

dplyr::filter(.data = penguins, island != "Dream") #select for all islands other than Dream

dplyr::filter(.data = penguins, species == "Adelie" | species == "Gentoo") #select for only adelie and gentoo species

filter(.data = penguins,
       island %in% c("Dream","Biscoe")) #use %in% for multiple calls

mutate(.data = penguins,
       after_2008 = ifelse(year>2008, "After 2008", "Before 2008")) #column showing if before or after 2008

mutate(.data = penguins,
       added = flipper_length_mm + body_mass_g) #new column showing flipper length added to body mass

mutate(.data = penguins,
       big = ifelse(body_mass_g>4000, "big", "smol")) #new column shwoing if big or small, above or below 4,000

penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) #calculate log biomass

penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(species, island, sex, log_mass) # select certain columns to stay in result

penguins %>% # use penguin dataframe
  filter(sex == "female") %>% #select females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(Species = species, island, sex, log_mass) #rename column to capital S Species

penguins %>% # summarize data
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE)) #calculate mean flipper length excluding any NAs

penguins %>% # 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE),
            min_flipper = min(flipper_length_mm, na.rm=TRUE)) # minimum flipper length
penguins %>%
  group_by(island) %>% #summary by island!
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))

penguins %>%
  group_by(island, sex) %>% # group by island and sex
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))

penguins %>%
  drop_na(sex) # drop NAs

penguins %>%
  drop_na(sex) %>% #dropped NAs, cleaned up data
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) + #can drop straight into ggplot!
  geom_boxplot()
