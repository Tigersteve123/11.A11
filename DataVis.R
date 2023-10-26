rm(list=ls())
setwd('/home/yichen/Documents/MIT/11A11')

library(readxl) # For importing Excel data
library(ggplot2) # For plotting?
library(maptools) # Deprecated. Using for the wrld_simpl map dataset
library(stringr) # For regular expressions

data(wrld_simpl) # Map data for plotting

countryPercent = read_excel("who_cooking_category_fuels.xlsx", sheet = "Country (%)") # Import Country % sheet

# We want to weight data (https://math.stackexchange.com/questions/684519/what-is-the-most-scientific-way-to-assign-weights-to-historical-data) to reduce variance while maintaining the most relevant data
# For now, I will just use 2019 data. Serious errors should not have too much of an impact on the current use case and I don't know enough about using exponential weighting
countryPercent$Avg2019 = sapply(countryPercent["2019"], function(x) as.numeric(str_match(x, "\\d+\\.\\d+")[,1])) # Extracts 2019 averages
countryPercent$Avg2019[is.na(countryPercent$Avg2019)] = 0 # The previous function replaced 0 values with NA. This re-adds the 0 values (they are not NA)

# Separate fuel types into different sets for visualization
countryPercentKerosene = countryPercent[countryPercent$FUEL == "Kerosene",]
countryPercentGas = countryPercent[countryPercent$FUEL == "Gas",]
countryPercentElectricity = countryPercent[countryPercent$FUEL == "Electricity",]
countryPercentBiomass = countryPercent[countryPercent$FUEL == "Biomass",] # Biomass, charcoal, and coal are the fuels of interest as they pollute the heaviest
countryPercentCharcoal = countryPercent[countryPercent$FUEL == "Charcoal",]
countryPercentCoal = countryPercent[countryPercent$FUEL == "Coal",]

plot_percent_use <- function(country_data) { # ChatGPT generated function to plot percent use on the map
  # Merge world map with country data
  merged_data <- merge(wrld_simpl, country_data, by.x = "NAME", by.y = "COUNTRY")
  
  # Plot using ggplot2
  p <- ggplot() +
    geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = country_data$Avg2019)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Percent Use") +
    labs(title = "Percent Use by Country") +
    theme_minimal()
  
  return(p)
}

plot_percent_use(countryPercentBiomass)
