# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(scales)

# Load functions ----------------------------------------------------------

source(here("R", "functions.R"))
source(here("R", "tfff-themes.R"))


# Get Data ----------------------------------------------------------------

race_ethnicity_order <- rev(c("White",
                              "Latino",
                              "African American",
                              "Asian",
                              "Am Indian/Alaska Native",
                              "Native Hawaiian/Pacific Islander",
                              "Multiracial",
                              "Other Race"))

race_ethnicity <- read_excel(here("data", "obtn-by-county.xlsx"),
                             sheet = "Race Ethnicity") %>% 
     clean_names() %>% 
     # filter(geography != "Rural") %>% 
     # filter(geography != "Urban") %>% 
     # filter(geography != "Oregon") %>% 
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
     mutate(pct_formatted = percent(pct)) %>% 
     mutate(population = fct_relevel(population, race_ethnicity_order))


# Plot Function --------------------------------------------------------------------

dk_make_race_ethnicity_bar_chart <- function(county) {

race_ethnicity_filtered <- race_ethnicity %>% 
     filter(geography == county)

ggplot(race_ethnicity_filtered, 
       aes(x = population, y = pct)) +
     geom_bar(stat = "identity", fill = tfff_dark_green) +
     geom_text(data = filter(race_ethnicity_filtered, population != "White"),
               aes(population, pct + .025,
                   label = str_glue("{population}: {pct_formatted}")),
               hjust = 0,
               color = tfff_dark_gray,
               family = "Calibri") +
     geom_text(data = filter(race_ethnicity_filtered, population == "White"),
               aes(population, pct - .025,
                   label = str_glue("{population}: {pct_formatted}")),
               hjust = 1,
               color = "white",
               family = "Calibri") +
     scale_x_discrete(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0)) +
     tfff_bar_chart_theme +
     coord_flip()

}


# Save Plot Function ------------------------------------------------------



dk_save_race_ethnicity_bar_chart <- function(county) {
     
     county <- str_to_lower(county)
     county <- str_replace(county, " ", "-")
     
     ggsave(filename = paste0("plots/by-county/",
                              county,
                              "/2019-race-ethnicity-bar-chart-",
                              county,
                              ".pdf"),
            device = cairo_pdf,
            width = 3.1,
            height = 2.1)
}

# Make and Save Plots -----------------------------------------------------

oregon_counties <- dk_get_oregon_counties()

for (i in 1:36) {
     dk_make_race_ethnicity_bar_chart(oregon_counties[i])
     dk_save_race_ethnicity_bar_chart(oregon_counties[i])
}


# Save Plots for Statewide Maps -------------------------------------------

dk_save_race_ethnicity_bar_chart_non_county <- function(geography) {
     
     geography <- str_to_lower(geography)
     geography <- str_replace(geography, " ", "-")
     
     ggsave(filename = paste0("plots/by-measure/race-ethnicity-charts/2019-race-ethnicity-chart-",
                              geography,
                              ".pdf"),
            device = cairo_pdf,
            width = 4,
            height = 2.5)
}


dk_make_race_ethnicity_bar_chart("Urban") +
     scale_x_discrete(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        limits = c(0, .85))
dk_save_race_ethnicity_bar_chart_non_county("Urban")

dk_make_race_ethnicity_bar_chart("Rural") +
     scale_x_discrete(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        limits = c(0, .85))
dk_save_race_ethnicity_bar_chart_non_county("Rural")

dk_make_race_ethnicity_bar_chart("Oregon") +
     scale_x_discrete(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0),
                        limits = c(0, .85))
dk_save_race_ethnicity_bar_chart_non_county("Oregon")
