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

oregon_counties <- dk_get_oregon_counties()

oregon_counties_geodata <- dk_oregon_counties_geodata()


# Get Data ----------------------------------------------------------------

employment_industries <- read_excel(here("data", "obtn-by-county.xlsx"),
                                    sheet = "Employment Industries") %>% 
     clean_names() %>% 
     mutate(geography = str_trim(geography)) %>% 
     gather("ranking", "industry", -geography) %>% 
     select(-ranking) %>% 
     filter(geography %in% oregon_counties) %>% 
     mutate(industry_exists = "Y") %>% 
     complete(geography, industry, fill = list(industry_exists = "N")) 


employment_industries_geodata <- dk_oregon_counties_geodata() %>% 
     left_join(employment_industries, by = c("name" = "geography"))



# Plot --------------------------------------------------------------------

dk_make_employment_plot <- function(industry_name) {
     
     employment_industries_geodata_filtered <- employment_industries_geodata %>% 
          filter(industry == industry_name)
     
     ggplot(employment_industries_geodata_filtered) +
          geom_sf(aes(fill = industry_exists),
                  color = "white",
                  size = .35) +
          coord_sf(datum = NA) +
          scale_fill_manual(values = c(tfff_light_gray, tfff_dark_green)) +
          tfff_map_theme +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          theme(legend.position = "none")
     
}

dk_save_employment_plot <- function(industry_name) {
     
     industry_name <- str_to_lower(industry_name)
     industry_name <- str_replace_all(industry_name, " ", "-")
     industry_name <- str_remove_all(industry_name, ",")
     
     ggsave(filename = paste0("plots/by-measure/employment-industries/2019-",
                              industry_name,
                              ".pdf"),
            device = cairo_pdf,
            width = 2,
            height = 1.5)
}


employment_industries_vector <- employment_industries %>% 
     distinct(industry) %>% 
     pull(industry)


for (i in 1:length(employment_industries_vector)) {
     dk_make_employment_plot(employment_industries_vector[i])
     dk_save_employment_plot(employment_industries_vector[i])
}


