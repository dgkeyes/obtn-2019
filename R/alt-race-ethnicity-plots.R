# Packages ----------------------------------------------------------------

library(tidyverse)
library(tigris)
library(here)
library(readxl)
library(janitor)
library(scales)
library(Cairo)

# Load functions ----------------------------------------------------------

source(here("R", "functions.R"))
source(here("R", "tfff-themes.R"))


# Get OBTN data -----------------------------------------------------------

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
     )) %>% 
     group_by(population) %>% 
     mutate(tertile = ntile(pct, 3)) %>% 
     mutate(tertile_numeric = as.numeric(tertile)) %>% 
     ungroup()

race_ethnicity_groups <- race_ethnicity_data %>% 
     distinct(population) %>% 
     pull()


# Get geodata -------------------------------------------------------------

oregon_counties_geodata <- dk_oregon_counties_geodata() %>% 
     left_join(race_ethnicity_data, by = c("name" = "geography")) 


# Plot --------------------------------------------------------------------

dk_make_race_ethnicity_map <- function(race_ethnicity_group) {
     
     oregon_counties_geodata_filtered <- oregon_counties_geodata %>% 
          filter(population == race_ethnicity_group)
     
     ggplot(oregon_counties_geodata_filtered) +
          geom_sf(aes(fill = pct),
                  # fill =  tfff_dark_green,
                  color = "white") +
          coord_sf(datum = NA) +
          scale_fill_gradient(low = tfff_light_green,
                              high = tfff_dark_green,
                              labels = percent_format(1)) +
          tfff_map_theme +
          theme(legend.position = "bottom") +
          guides(fill = guide_legend(nrow = 1))
     
}

dk_save_race_ethnicity_map <- function(plot_category, plotwidth, plotheight) {
     
     plot_category <- str_to_lower(plot_category)
     plot_category <- str_replace_all(plot_category, " ", "-")
     plot_category <- str_replace(plot_category, "\\/", "-")
     
     ggsave(filename = paste0("plots/by-measure/race-ethnicity/",
                              plot_category,
                              ".pdf"),
            device = cairo_pdf,
            width = plotwidth,
            height = plotheight)
}



for (i in 1:8) {
     dk_make_race_ethnicity_map(race_ethnicity_groups[i])
     dk_save_race_ethnicity_map(race_ethnicity_groups[i], 10, 8)
}




# By total population -----------------------------------------------------

counties_total_pop <- read_excel(here("data", "obtn-by-county.xlsx"),
                                 sheet = "Total Population") %>% 
     clean_names() %>% 
     set_names(c("county", "total_pop"))

oregon_counties_geodata_total_pop <- oregon_counties_geodata %>% 
     left_join(counties_total_pop, by = c("name" = "county")) %>% 
     mutate(subgroup_pop = total_pop * pct) %>% 
     filter(population == "Latino") %>% 
     mutate(pct_of_total = prop.table(subgroup_pop))

ggplot(oregon_counties_geodata_total_pop) +
     geom_sf(aes(fill = pct_of_total),
             # fill =  tfff_dark_green,
             color = "white") +
     coord_sf(datum = NA) +
     scale_fill_gradient(low = tfff_light_green,
                         high = tfff_dark_green,
                         labels = percent_format(1)) +
     tfff_map_theme +
     theme(legend.position = "bottom") +
     guides(fill = guide_legend(nrow = 1))


# At census tract level (tidycensus) ---------------------------------------------------

library(tidycensus)
options(tigris_use_cache = TRUE)

# acs_vars <- load_variables(2016, "acs5", cache = TRUE)


white_population <- get_acs("tract",
                            variables = (white = "B01001H_001"),
                            state = "OR",
                            geometry = TRUE) %>% 
     rename("white_pop" = "estimate")

total_population <- get_acs("tract",
                            variables = (total_pop = "B01001_001"),
                            state = "OR") %>% 
     rename("total_pop" = "estimate")

white_pop_by_census_tract <- white_population %>% 
     left_join(total_population, by = "GEOID") %>% 
     mutate(pct = white_pop / total_pop)

ggplot(white_pop_by_census_tract) +
     geom_sf(aes(fill = pct),
             color = tfff_light_gray,
             size = .1) +
     geom_sf(data = oregon_counties_geodata,
             fill =  "transparent",
             color = "white",
             size = 0.2) +
     coord_sf(datum = NA) +
     coord_sf(datum = NA) +
     scale_fill_gradient(low = tfff_light_green,
                         high = tfff_dark_green,
                         labels = percent_format(1)) +
     tfff_map_theme +
     theme(legend.position = "bottom") +
     guides(fill = guide_legend(nrow = 1))



# At census tract (OSU data) ----------------------------------------------

race_ethnicity_by_tract <- read_excel(here("data", "race-ethnicity-by-tract.xlsx"),
                                      skip = 2) %>% 
     clean_names() %>% 
     filter(str_length(id2) > 5) %>% # Drop all Oregon and county data
     select(-contains("margin")) %>% 
     select(-estimate_not_hispanic_or_latino) %>% 
     select(id:geography, estimate_total:estimate_hispanic_or_latino) %>% 
     gather("group", "number", -c(id, id2, geography)) %>% 
     mutate(id2 = as.character(id2)) %>% 
     filter(group != "estimate_total") %>% 
     group_by(id2) %>% 
     mutate(pct = prop.table(number)) %>% 
     ungroup()

oregon_tracts_geodata <- tracts("OR", cb = T, class="sf") %>% 
     clean_names() %>% 
     left_join(race_ethnicity_by_tract, by = c("geoid" = "id2")) %>% 
     filter(str_detect(group, "estimate_hispanic_or_latino"))

ggplot(oregon_tracts_geodata) +
     geom_sf(aes(fill = pct),
             color = "transparent",
             size = .1) +
     geom_sf(data = oregon_counties_geodata,
             fill =  "transparent",
             color = "white",
             size = 0.2) +
     coord_sf(datum = NA) +
     scale_fill_gradient(low = tfff_light_green,
                         high = tfff_dark_green,
                         labels = percent_format(1)) +
     tfff_map_theme +
     theme(legend.position = "bottom") +
     guides(fill = guide_legend(nrow = 1))
