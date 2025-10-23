library(palmerpenguins)
library(tidyverse)

# Load the penguins dataset
#Explore penguin datasets
glimpse(penguins)
view(penguins)

#Visualize bill length vs bill depth
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  labs(title = "Bill Length vs Bill Depth",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  theme_minimal()

#Visualize flipper length vs body mass with shape for different sexes
ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = species, shape = sex)) +
  geom_point() +
  labs(title = "Bill Length vs Bill Depth",
       x = "Bill Length (mm)",
       y = "Body Mass (g)") +
  theme_minimal()

library(gapminder)
install.packages("knittr")
library(knittr)
library(janitor)

head(gapminder) |> 
  kable()

gapminder_clean <- gapminder |> 
  clean_names()
