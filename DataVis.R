rm(list=ls())
setwd('/home/yichen/Documents/MIT/11A11')

library(readxl) # For importing Excel data
library(ggplot2) # For plotting
library(stringr) # For regular expressions
library(rnaturalearth) # For map data

countryPercent = read_excel("who_cooking_category_fuels.xlsx", sheet = "Country (%)") # Import Country % sheet

# We want to weight data (https://math.stackexchange.com/questions/684519/what-is-the-most-scientific-way-to-assign-weights-to-historical-data) to reduce variance while maintaining the most relevant data
# For now, I will just use 2019 data. Serious errors should not have too much of an impact on the current use case and I don't know enough about using exponential weighting
countryPercent$Avg2019 = sapply(countryPercent["2019"], function(x) as.numeric(str_match(x, "\\d+\\.\\d+")[,1])) # Extracts 2019 averages
countryPercent$Avg2019[is.na(countryPercent$Avg2019)] = 0 # The previous function replaced 0 values with NA. This re-adds the 0 values (they are not NA)
countryPercent$Avg2019 <- as.numeric(countryPercent$Avg2019)

plot_percent_use <- function(country_data = countryPercent, fuel) { # ChatGPT wrote most of this. Took so long
  # Get a higher resolution map of the world
  world_map <- ne_countries(scale = "medium", returnclass = "sf")
  
  country_data = country_data[country_data$FUEL == fuel,]
  
  # Merge world map with country data based on country names
  merged_data <- merge(world_map, country_data, by.x = "name", by.y = "COUNTRY", all.x = TRUE)
  
  # Plot using ggplot2 with geom_sf
  p <- ggplot() +
    geom_sf(data = merged_data, aes(fill = Avg2019)) +
    scale_fill_continuous(low = "lightblue", high = "darkblue", 
                          name = "Percent Use in 2019",
                          guide = guide_colorbar(title.position = "top", 
                                                 title.hjust = 0.5)) +
    labs(title = paste("Percent Use by Country,", fuel)) +
    theme_minimal()
  
  return(p)
}

plot_percent_use(countryPercent, "Biomass") # Biomass, charcoal, and coal are the fuels of interest as they pollute the heaviest
plot_percent_use(countryPercent, "Charcoal")
plot_percent_use(countryPercent, "Coal")
