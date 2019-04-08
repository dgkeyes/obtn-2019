# Packages ----------------------------------------------------------------

library(tidyverse) 
library(ggrepel)
library(readxl)
library(scales)
library(extrafont)
library(janitor)
library(tigris)
library(here)

# Load stuff --------------------------------------------------------------

source(here("R", "tfff-themes.R"))
source(here("R", "functions.R"))


# Get Data ----------------------------------------------------------------




oregon_counties_geodata <- dk_oregon_counties_geodata()
     

tribes <- read_excel(here("data", "obtn-by-county.xlsx"),
                     sheet = "Tribes") %>% 
     clean_names() %>% 
     gather(key = "tribe", value = "present", -geography) %>% 
     drop_na(present) %>% 
     mutate(present = "Y") %>% 
     right_join(oregon_counties_geodata, by = c("geography" = "name"))


# Plot --------------------------------------------------------------------

ggplot(oregon_counties_geodata) +
     geom_sf(fill = tfff_light_gray,
             color = "white",
             size = .25) +
     geom_sf(data = tribes,
             fill = tfff_dark_green) +
     coord_sf(datum = NA) +
     scale_fill_manual(values = tfff_choropleth_colors) +
     tfff_map_theme +
     theme(legend.position = "bottom")
