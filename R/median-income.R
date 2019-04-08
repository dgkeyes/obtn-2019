# Packages ----------------------------------------------------------------

library(tidyverse) 
library(ggrepel)
library(readxl)
library(scales)
library(extrafont)
library(janitor)
library(tigris)
library(here)
library(Cairo)

# Load stuff --------------------------------------------------------------

source(here("R", "tfff-themes.R"))
source(here("R", "functions.R"))

loadfonts(device = "pdf")


# Get Data ----------------------------------------------------------------

median_income_data <- read_excel(here("data", "obtn-by-county.xlsx"),
                                 sheet = "Median Income") %>% 
     clean_names() %>% 
     mutate(geography = str_trim(geography))

# oregon_median_income <- median_income_data %>% 
#      filter(geography == "Oregon") 
# 
# 
# median_income_data <- median_income_data %>% 
#      drop_na(median_income) %>% 
#      filter(geography != "Oregon")


# Plot --------------------------------------------------------------------

dk_make_median_income_plot <- function(county_name) {

median_income_data_filtered <- median_income_data %>% 
     filter(geography %in% c(county_name, "Oregon")) %>% 
     mutate(type = case_when(
          geography == "Oregon" ~ "state",
          TRUE ~ "county"
     )) %>% 
     mutate(geography = fct_reorder(geography, type)) %>% 
     mutate(geography = fct_rev(geography))

ggplot(median_income_data_filtered, aes(geography, median_income,
                 fill = geography)) +
     geom_col(width = 0.75) +
     # Add county/state name
     geom_text(label = median_income_data_filtered$geography,
               aes(geography, 2000),
               hjust = 0,
               color = "white",
               family = "Calibri") +
     # Add median income amount text
     geom_text(label = dollar(median_income_data_filtered$median_income),
               aes(geography, median_income - 2000),
               hjust = 1,
               color = "white",
               family = "Calibri") +
     scale_fill_manual(values = c(tfff_medium_gray, tfff_dark_green)) +
     scale_x_discrete(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0)) +
     coord_flip() +
     tfff_bar_chart_theme

}

dk_save_median_income_plot <- function(county, plotwidth, plotheight) {
     county <- str_to_lower(county)
     county <- str_replace_all(county, " ", "-")
     
     ggsave(filename = paste0("plots/by-county/",
                              county,
                              "/2019-median-income-",
                              county,
                              ".pdf"),
            device = cairo_pdf,
            width = plotwidth,
            height = plotheight)
}


oregon_counties <- dk_get_oregon_counties()

for (i in 1:36) {
     dk_make_median_income_plot(oregon_counties[i])
     dk_save_median_income_plot(oregon_counties[i], 2.43, .61)
}
