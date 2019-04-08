# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(scales)
library(sf)

# Load functions ----------------------------------------------------------

source(here("R", "functions.R"))
source(here("R", "tfff-themes.R"))


# Get Data ----------------------------------------------------------------

race_ethnicity_data <- read_excel(here("data", "obtn-by-county.xlsx"),
                                  sheet = "Race Ethnicity") %>% 
     clean_names() %>% 
     filter(geography != "Rural") %>% 
     filter(geography != "Urban") %>% 
     filter(geography != "Oregon") %>% 
     gather("population", "pct", -geography) %>% 
     mutate(population = case_when(
          population == "percentage_of_population_white_non_latino" ~ "White",
          population == "percentage_of_population_black_non_latino" ~ "African American",
          population == "percentage_of_population_asian_non_latino" ~ "Asian",
          population == "percentage_of_population_american_indian_or_alaska_native_non_latino" ~ "Am Indian/Alaska Native",
          population == "percentage_of_population_native_hawaiian_pacific_islander_non_latino" ~ "Native Hawaiian/Pacific Islander",
          population == "percentage_of_population_multi_racial_non_latino" ~ "Multiracial",
          population == "percentage_of_population_other_race_non_latino" ~ "Other Race",
          population == "percentage_of_population_latino" ~ "Latino"
     )) 

race_ethnicity_groups <- race_ethnicity_data %>% 
     distinct(population) %>% 
     pull()


# Get geodata -------------------------------------------------------------

by_county_by_pct_within_county <- dk_oregon_counties_geodata() %>% 
     left_join(race_ethnicity_data, by = c("name" = "geography")) %>%
     group_by(population) %>% 
     mutate(tertile = ntile(pct, 3)) %>% 
     mutate(tertile_numeric = as.numeric(tertile)) %>% 
     ungroup() %>% 
     mutate(tertile_label = case_when(
          tertile_numeric == 1 ~ " Bottom third\n 2.7% - 7.1% ",
          tertile_numeric == 2 ~ " Middle third\n 7.2% - 11.3% ",
          tertile_numeric == 3 ~ " Top third\n 12.2% - 35.1% "
     )) %>% 
     mutate(tertile_label = factor(tertile_label, levels = c(" Top third\n 12.2% - 35.1% ", 
                                                             " Middle third\n 7.2% - 11.3% ",
                                                             " Bottom third\n 2.7% - 7.1% "))) %>% 
     filter(population == "Latino")

# I'm doing this to manually figure out percentages to add to the labels above
by_county_by_pct_within_county %>% 
     group_by(tertile_label) %>% 
     summarize(max_pct = max(pct),
               min_pct = min(pct)) %>% 
     select(tertile_label, max_pct, min_pct) %>% 
     st_drop_geometry()




# Plot Function --------------------------------------------------------------------

ggplot(by_county_by_pct_within_county) +
     geom_sf(aes(fill = tertile_label),
             color = "white") +
     coord_sf(datum = NA) +
     scale_fill_manual(values = tfff_choropleth_colors) +
     tfff_map_theme +
     theme(legend.position = "bottom") +
     guides(fill = guide_legend(nrow = 1))


# Save Function -----------------------------------------------------------


dk_save_race_ethnicity_map <- function(group) {
     
     group <- str_to_lower(group)
     group <- str_replace_all(group, " ", "-")
     group <- str_replace_all(group, "/", "-")
     
     ggsave(filename = paste0("plots/by-measure/race-ethnicity-maps/2019-race-ethnicity-map-",
                              group,
                              ".pdf"),
            device = cairo_pdf,
            width = 4.3684,
            height = 3.25)
}

# Make maps ---------------------------------------------------------------

dk_make_race_ethnicity_map("Latino")
dk_save_race_ethnicity_map("Latino")

