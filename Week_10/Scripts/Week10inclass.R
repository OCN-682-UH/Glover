#week 10 in class lecture script


library(tidyverse)
df <- tibble(
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
head(df)

df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)))

df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

head(df)


rescale01 <- function(x) {
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value)
}
df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))

df %>%
  mutate_all(rescale01)  # mutate every column with the rescale01 function


temp_C <- (temp_F - 32) * 5 / 9

fahrenheit_to_celsius <- function() {
}

fahrenheit_to_celsius <- function() { 
  temp_C <- (temp_F - 32) * 5 / 9
}

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9 
}

fahrenheit_to_celsius <- function(temp_F) { 
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
}

fahrenheit_to_celsius(32)
fahrenheit_to_celsius(212)
fahrenheit_to_celsius(78)

temp_K <- (temp_C + 273.15)

celcius_to_kelvin <- function(temp_C) { 
  temp_K <- (temp_C + 273.15)
  return(temp_K)
}

celcius_to_kelvin(100)

library(palmerpenguins)
library(PNWColors) # for the PNW color palette 
# you may need to install the PNWColors library if you haven't used it yet
pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
  geom_point()+
  geom_smooth(method = "lm")+ # add a linear model
  scale_color_manual("Island", values=pal)+   # use pretty colors and another example of how to manually change the legend title for colors
  theme_bw()

myplot<-function(){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}

myplot<-function(data, x, y){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = x, y =y , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}

  myplot(data = penguins, x = body_mass_g, y = bill_length_mm)
  
  
myplot<-function(data, x, y){ 
    pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
}

myplot(data = penguins, x = body_mass_g, y = bill_length_mm)  

myplot(data = penguins, x = body_mass_g, y = flipper_length_mm)


myplot<-function(data = penguins, x, y){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}    

myplot(x = body_mass_g, y = flipper_length_mm)

myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")



a <- 4
b <- 5

if (a > b) { # my question
  f <- 20 # if it is true give me answer 1
} else { # else give me answer 2
  f <- 10
}

f



myplot<-function(data = penguins, x, y ,lines=TRUE ){ # add new argument for lines
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}

myplot<-function(data = penguins, x, y, lines=TRUE ){ # add new argument for lines
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  if(lines==TRUE){
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
  else{
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
}

myplot(x = body_mass_g, y = flipper_length_mm)

myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE)
