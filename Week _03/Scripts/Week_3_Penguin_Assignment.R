# Load the necessary libraries
library(ggplot2)
library(palmerpenguins)

# Create the violin plot
# The aes() mapping is changed to show the distribution of bill depth by species.
# The geom_violin() and geom_jitter() functions are used to create the plot.
# We're using the 'fill' aesthetic for the violins instead of 'color'
# to color the entire shape.
ggplot(data = penguins, 
       mapping = aes(x = species, 
                     y = bill_depth_mm, 
                     fill = species)) +
  
  # Add the violin geometry. 'trim = FALSE' ensures the violins extend to the min/max values.
  geom_violin(trim = FALSE) +
  
  # Add individual data points to the plot using geom_jitter()
  # This helps to see the exact number of data points at each location.
  geom_jitter(shape = 16, 
              position = position_jitter(0.2), 
              alpha = 0.5) +
  
  # Add titles and labels for clarity
  labs(title = "Bill Depth Distribution by Penguin Species",
       subtitle = "Comparing bill depth of Adelie, Chinstrap, and Gentoo penguins",
       x = "Species",
       y = "Bill Depth (mm)",
       fill = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  
  # Use a colorblind-friendly scale from the viridis package
  # The viridis palette is highly effective for visualizations
  # because it's both aesthetically pleasing and accessible.
  scale_fill_viridis_d() +
  
  # Adjust the theme for a clean, professional look
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

