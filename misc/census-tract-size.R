

# Packages ----------------------------------------------------------------

library(tidyverse)
library(tigris)


# Get Data ----------------------------------------------------------------

options(scipen=999)

oregon_tracts_geodata <- tracts("OR", cb = T, class="sf") %>% 
     clean_names() %>% 
     mutate(square_miles = aland / 3.861e+7)
