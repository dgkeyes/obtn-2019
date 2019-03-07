# Packages ----------------------------------------------------------------

library(tidyverse) 
library(ggrepel)
library(readxl)
# library(ggmap)
# library(maps)
library(scales)
library(extrafont)
library(janitor)
library(tigris)
library(here)




# LOAD DATA ---------------------------------------------------------------

source(here("R", "load-data.R"))


# LOAD THEMES + COLORS ----------------------------------------------------

source(here("R", "tfff-themes.R"))

# COUNTY LEVEL ------------------------------------------------------------

# Inset map ---------------------------------------------------------------


oregon_counties_map <- counties(cb = T, class="sf") %>% 
     clean_names() %>% 
     filter(statefp == 41)

oregon_map <- states(cb = T, class="sf") %>% 
     clean_names() %>% 
     filter(statefp == 41)

dk_make_inset_map <- function(county) {
     ggplot(oregon_map) +
          geom_sf(fill = tfff_medium_gray,
                  color = "transparent") +
          geom_sf(data = filter(oregon_counties_map,
                                name == county),
                  fill = tfff_light_green,
                  color = "white",
                  size = 0.1) +
          coord_sf(datum = NA) +
          tfff_map_theme

}



for (i in 1:36) {
     dk_make_inset_map(str_to_title(oregon_counties[i]))
     dk_save_plot("inset-map", 1.2313, 1.1225)
}



dk_make_inset_map("Multnomah")




dk_save_plot("inset-map", 1.2313, 1.1225)



# County map --------------------------------------------------------------


# Largest community + notable features ------------------------------------


# Race/ethnicity ----------------------------------------------------------


# Population pyramid ------------------------------------------------------



# STATE LEVEL -------------------------------------------------------------

# Race/ethnicity ----------------------------------------------------------


# Population pyramid ------------------------------------------------------


# Single measure choropleth map -------------------------------------------


# Employment categories ---------------------------------------------------


# Tribes map --------------------------------------------------------------


# Notable features map ----------------------------------------------------



# Largest community map ---------------------------------------------------





