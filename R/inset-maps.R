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



# Get data ----------------------------------------------------------------

oregon_counties <- dk_get_oregon_counties()

# Make maps ---------------------------------------------------------------


oregon_counties_geodata <- dk_oregon_counties_geodata()

oregon_state_geodata <- dk_oregon_state_geodata()

dk_make_inset_map <- function(county) {
     ggplot(oregon_state_geodata) +
          geom_sf(fill = tfff_medium_gray,
                  color = "transparent") +
          geom_sf(data = filter(oregon_counties_geodata,
                                name == county),
                  fill = tfff_light_green,
                  color = "white",
                  size = 0.3) +
          coord_sf(datum = NA) +
          tfff_map_theme
}


for (i in 1:36) {
     dk_make_inset_map(oregon_counties[i])
     dk_save_plot("inset-map", 1.2313, 1.1225)
}

