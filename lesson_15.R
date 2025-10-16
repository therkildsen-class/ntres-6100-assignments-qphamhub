library(tidyverse)
library(gapminder) #install.packages("gapminder")
library(knitr)

#For loop today
head(gapminder) |>
  kable()

gapminder <- gapminder |> 
  rename("life_exp" = lifeExp, "gdp_per_cap" = gdpPercap)

View(gapminder)

#Experience subset first with Afghanistan 
## filter the country to plot
gap_to_plot <- gapminder |>
  filter(country == "Afghanistan")
## make my plot
my_plot <- ggplot(gap_to_plot, mapping = aes(x = year, y = gdp_per_cap)) + 
  geom_point() +
  #labs(title = "Afghanistan")
  labs(title = str_c("Afghanistan", "GDP per capita", sep = " ")) #str_c to combine together different text strings
my_plot
ggsave(filename = "Afghanistan_gdp_per_cap.png",  plot = my_plot)

#Test with object 
## create country variable
cntry <- "Afghanistan"
## filter the country to plot
gap_to_plot <- gapminder |>
  filter(country == cntry)
## plot
my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdp_per_cap)) + 
  geom_point() +
  ## add title and save
  labs(title = str_c(cntry, "GDP per capita", sep = " "))
## note: there are many ways to create filenames with str_c(), str_c() or file.path(); we are doing this way for a reason.
ggsave(filename = str_c(cntry, "_gdp_per_cap.png", sep = ""), plot = my_plot) #str_c here bc cntry is an object 

#Automate to rotate country names --> For loop structure 
##Nothing in list_of_countries for now 
##Every time, change what is assigned to cntry
##Format of loop
for (each cntry in list_of_countries) {
  
}
##Real test - render all options in country_list 
cntry <- "Japan"
country_list <- c("Japan", "Canada", "Spain")
for (cntry in country_list) {
  
  ## filter the country to plot
  gap_to_plot <- gapminder |>
    filter(country == cntry)
  
  ## plot
  my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdp_per_cap)) + 
    geom_point() +
    ## add title and save
    labs(title = str_c(cntry, "GDP per capita", sep = " "))
  
#Make a directory for all graphs so it is less messy 
dir.create("figures") 
###Add graphs to new directory
ggsave(filename = str_c("figures/", cntry, "_gdp_per_cap.png", sep = ""), plot = my_plot)
} 

##Now use the gapminder data 
cntry <- "Japan"
country_list <- unique(gapminder$country) # ?unique() returns the unique values
for (cntry in country_list) {
  
  ## filter the country to plot
  gap_to_plot <- gapminder |>
    filter(country == cntry)
  
  ## plot
  my_plot <- ggplot(data = gap_to_plot, aes(x = year, y = gdp_per_cap)) + 
    geom_point() +
    ## add title and save
    labs(title = str_c(cntry, "GDP per capita", sep = " "))
  
  ## add the figures/ folder
  ggsave(filename = str_c("figures/", cntry, "_gdp_per_cap.png", sep = ""), plot = my_plot)
} 
