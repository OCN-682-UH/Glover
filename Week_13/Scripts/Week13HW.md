# Week 13 HW
Kent Glover

## Introduction

For this assignment, I need to process temperature and light data from
four tide pools. The goal is to calculate summary statistics (mean and
standard deviation) for each pool using two different methods: a
standard `for loop` and a `purrr::map` function.

The main challenge is that the `Intensity.lux` column contains commas
which R reads as characters. I have to clean this before calculating
means or the code will error out.

``` r
#load libraries
library(tidyverse)
library(here)
library(knitr)

#set path to data
data_dir <- here("Week_13", "Data", "homework")

#get list of csv files
files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
```

## Data Processing with a For Loop

This approach explicitly iterates through each file path in our list.
Inside the loop, the code reads the CSV, cleans the Intensity.lux and
Temp.C columns by converting them to numeric types, and then calculates
the mean and standard deviation for both temperature and light
intensity. These individual dataframes are stored in a list and then
combined into a final summary table using bind_rows().

``` r
#empty list to store results
results_list <- vector("list", length(files))

#start the loop
for (i in seq_along(files)) {
  
  #read file, suppress messages
  d <- read_csv(files[i], show_col_types = FALSE)
  
  #clean the data
  #fix the comma problem in intensity.lux
  d_clean <- d %>% 
    mutate(Intensity.lux = parse_number(as.character(Intensity.lux)))
  
  #calculate stats
  #group by poolid so we know which pool is which
  summ <- d_clean %>% 
    group_by(PoolID) %>% 
    summarise(
      mean_temp = mean(Temp.C, na.rm = TRUE),
      sd_temp = sd(Temp.C, na.rm = TRUE),
      mean_lux = mean(Intensity.lux, na.rm = TRUE),
      sd_lux = sd(Intensity.lux, na.rm = TRUE)
    )
  
  #save to list
  results_list[[i]] <- summ
}

#combine list into one dataframe
final_loop <- bind_rows(results_list)

# print table
kable(final_loop, caption = "Summary Stats (Calculated with For Loop)", digits = 2)
```

| PoolID | mean_temp | sd_temp | mean_lux |   sd_lux |
|-------:|----------:|--------:|---------:|---------:|
|      1 |     13.27 |    2.32 |   426.83 |  1660.55 |
|      2 |     13.17 |    2.31 |  5603.16 | 11928.92 |
|      3 |     13.10 |    2.32 |  5605.07 | 12101.10 |
|      4 |     13.22 |    2.27 |   655.10 |  2088.84 |

Summary Stats (Calculated with For Loop)

## Data Processing with `purrr::map`

Instead of managing indices and empty lists manually, I defined a custom
function, calc_pool_stats(), that handles the reading, cleaning, and
summarizing for a single file. I then used map_dfr() to apply this
function to the list of files. This method is more concise and less
prone to indexing errors than a for loop.

``` r
# define function to process one file
calc_pool_stats <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% 
    # cleaning step
    mutate(Intensity.lux = parse_number(as.character(Intensity.lux))) %>% 
    group_by(PoolID) %>% 
    summarise(
      mean_temp = mean(Temp.C, na.rm = TRUE),
      sd_temp = sd(Temp.C, na.rm = TRUE),
      mean_lux = mean(Intensity.lux, na.rm = TRUE),
      sd_lux = sd(Intensity.lux, na.rm = TRUE)
    )
}

# map the function over the file list
# map_dfr automatically binds the rows
final_map <- map_dfr(files, calc_pool_stats)

# print table
kable(final_map, caption = "Summary Stats (Calculated with Map)", digits = 2)
```

| PoolID | mean_temp | sd_temp | mean_lux |   sd_lux |
|-------:|----------:|--------:|---------:|---------:|
|      1 |     13.27 |    2.32 |   426.83 |  1660.55 |
|      2 |     13.17 |    2.31 |  5603.16 | 11928.92 |
|      3 |     13.10 |    2.32 |  5605.07 | 12101.10 |
|      4 |     13.22 |    2.27 |   655.10 |  2088.84 |

Summary Stats (Calculated with Map)

## Data Visualization

``` r
#load and clean all data for plotting
all_data <- map_dfr(files, read_csv, show_col_types = FALSE) %>% 
  mutate(
    Intensity.lux = parse_number(as.character(Intensity.lux)),
    Temp.C = as.numeric(as.character(Temp.C))
  )

#create the figure
ggplot(all_data, aes(x = factor(PoolID), y = Temp.C, fill = Removal_Control)) +
  geom_boxplot() +
  labs(x = "Pool ID", y = "Temperature (°C)", title = "Temperature Distribution by Pool") +
  theme_minimal()
```

    Warning: Removed 41 rows containing non-finite outside the scale range
    (`stat_boxplot()`).

![Temperature by
Pool](Week13HW_files/figure-commonmark/make-figures-1.png)
