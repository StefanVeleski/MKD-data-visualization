# Loading libraries

library(raster) 
library(tidyverse)
library(sf)
library(geodata)
library(ggthemes)
library(plotly)
library(scales)

# Loading datasets

library(readr)

census_2002 <- read_csv("Datasets/Population data/census_2002.csv")
census_2021 <- read_csv("Datasets/Population data/census_2021.csv") 

# Getting the province shape file for Macedonia

macedonia <- getData('GADM', country='MKD', level=1) %>%
  st_as_sf() # Converting it into an sf object

# Adding census_2022 dataset sf object and making sure each column is in the correct data type

macedonia <- bind_cols(macedonia, census_2002) 

macedonia$Municipality <- as.factor(macedonia$Municipality)
macedonia$`Total resident population` <- as.numeric(macedonia$`Total resident population`)

# Data wrangling, removing unnecessary boundaries, and adjusting boundaries according to 2013 municipal reform

macedonia_new <- macedonia %>%
  filter(!Municipality %in% c("Lake Ohrid")) %>% # Remove Lake Ohrid boundaries
  mutate(Municipality = fct_collapse(Municipality, Kichevo = c("Drugovo", "Zajas", "Kichevo","Oslomej","Vraneshtica"))) %>% # Merge Drugovo, Zajas, Oslomej and Vraneshtica into Kichevo
  group_by(Municipality) %>%
  summarise(`Total resident population` = sum(`Total resident population`))

macedonia_new$`Total resident population 2021` <- census_2021$`Total resident population`

# Adding a variable with the percentage change between the two censuses

macedonia_new$Change <- (macedonia_new$`Total resident population 2021`-macedonia_new$`Total resident population`)/macedonia_new$`Total resident population`

macedonia_new$Change <- round(macedonia_new$Change, 4) # Round to four decimal places

limit <- max(abs(macedonia_new$Change)) * c(-1, 1) # Centering color palette so that white = 0

# Plotting 

p <- ggplot(macedonia_new) +
  geom_sf(aes(text = paste0("Municipality: ", Municipality, "\n2002 population: ", `Total resident population`, "\n2021 population: ", `Total resident population 2021`), fill = Change)) + # Adding hover text and choosing a variable to determine the color
  scale_fill_distiller(type = "div", limit = limit, palette = "BrBG", direction = 1,labels = percent) + # Set color scheme 
  labs(title = "Population change in Macedonia between 2002 and 2021") + # Add title
  theme(axis.text.x = element_blank(), # Remove unnecessary elements of plot
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                rect = element_blank())

# Converting ggplot object to plotly object (non-interactive to interactive)
  
ggplotly(p)

# This object was exported to R Pubs using the built in functionality of RStudio IDE 