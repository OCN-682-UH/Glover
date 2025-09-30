# This is Kent's in person week 5 script

#Load Libraries
library(tidyverse)
library(here)
### Load data ######
# Environmental data from each site
EnviroData<-read_csv(here("Week_05","data", "site.characteristics.data.csv"))
#Thermal performance data
TPCData<-read_csv(here("Week_05","data","Topt_data.csv"))

glimpse(EnviroData)
glimpse(TPCData)
# also use View()

EnviroData_wide <- EnviroData %>% 
  pivot_wider(names_from = parameter.measured,
              values_from = values)
View(EnviroData_wide)

EnviroData_wide <- EnviroData %>% 
  pivot_wider(names_from = parameter.measured, # pivot the data wider
              values_from = values) %>%
  arrange(site.letter) # arrange the dataframe by site
View(EnviroData_wide)

FullData_left<- left_join(TPCData, EnviroData_wide)
## Joining with by = join_by(site.letter)
head(FullData_left)

FullData_left<- left_join(TPCData, EnviroData_wide) %>%
  relocate(where(is.numeric), .after = where(is.character)) # relocate all the numeric data after the character data
## Joining with by = join_by(site.letter)
head(FullData_left)
view(FullData_left)



TCPElong <- FullData_left %>%
  pivot_longer(
    cols = E:substrate.cover,
    names_to = "Parameters",
    values_to = "Measurements")

view(TCPElong)

TPCEstats <- TCPElong %>%
  group_by(name, Parameters) %>%
  summarise(
    mean_measurement = mean(Measurements, na.rm = TRUE),
    sd_measurement = sd(Measurements, na.rm = TRUE),
    .groups = 'drop' # Recommended to drop grouping after summary
  )

view(TPCEstats)

write_csv(
  TPCEstats,
  here("Week_05", "Outputs", "TPCE_summary_stats.csv")
)

ggplot(TPCEstats, aes(x = name, y = mean_measurement, fill = name)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_measurement - sd_measurement,
                    ymax = mean_measurement + sd_measurement),
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~Parameters, scales = "free_y") +
  labs(x = "Site", y = "Mean ± SD", title = "Summary Statistics by Site and Parameter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
#LOL thats a lot and not great, but good practice.

#TIBBLES
# Make 1 tibble
T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
             Temperature = c(14.1, 16.7, 15.3, 12.8))
T1

# make another tibble
T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
            pH = c(7.3, 7.8, 8.1, 7.9))
T2

left_join(T1, T2)

right_join(T1, T2)

inner_join(T1, T2)

full_join(T1, T2)

semi_join(T1, T2)

anti_join(T1, T2)
 
install.packages("cowsay")
library(cowsay)

say("best fishes", by = "shark")

say("best fishes", by = "yoda")
