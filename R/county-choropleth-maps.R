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

# Get Choropleth Data ----------------------------------------------------------------

dk_get_choropleth_data <- function(sheet_name) {
     read_excel(here("data", "obtn-by-measure.xlsx"),
                sheet = sheet_name) %>% 
          clean_names() %>% 
          select(county, numeric_only)
}

choropleth_sheets <- here("data", "obtn-by-measure.xlsx") %>% 
     excel_sheets() %>% 
     tibble() %>% 
     slice(-1) %>% 
     set_names("sheet_name") %>% 
     mutate(n = row_number()) %>% 
     mutate(n = as.numeric(n))


choropleth_data <- map_df(choropleth_sheets$sheet_name, dk_get_choropleth_data,
                          .id = "sheet") %>% 
     mutate(sheet = as.numeric(sheet)) %>% 
     left_join(choropleth_sheets, by = c("sheet" = "n")) %>% 
     group_by(sheet_name) %>% 
     mutate(tertile_numeric = ntile(numeric_only, 3)) %>% 
     mutate(tertile_text = case_when(
          tertile_numeric == 3 ~ "Top third",
          tertile_numeric == 2 ~ "Middle third",
          tertile_numeric == 1 ~ "Bottom third"
     ))



# Get Map Data and Merge It -----------------------------------------------

oregon_counties_geodata <- dk_oregon_counties_geodata() %>% 
     left_join(choropleth_data, by = c("name" = "county"))


# Get List of Oregon Counties ---------------------------------------------

oregon_counties <- dk_get_oregon_counties()

# Make maps ---------------------------------------------------------------

dk_make_choropleth_map <- function(measure) {
     ggplot(filter(oregon_counties_geodata, sheet_name == measure)) +
          geom_sf(aes(fill = tertile_text),
                  color = "transparent") +
          coord_sf(datum = NA) +
          scale_fill_manual(values = rev(c("#dddddd", 
                                           "#B5CC8E", 
                                           "#6E8F68", 
                                           "#265142"))) +
          tfff_map_theme +
          theme(legend.position = "bottom")
}

dk_save_choropleth_map <- function(plot_category, plotwidth, plotheight) {
     
     plot_category <- str_to_lower(plot_category)
     plot_category <- str_replace(plot_category, " ", "-")
     
     ggsave(filename = paste0("plots/by-measure/choropleth-maps/",
                              plot_category,
                              ".pdf"),
            device = cairo_pdf,
            width = plotwidth,
            height = plotheight)
}


for (i in 1:31) {
     dk_make_choropleth_map(choropleth_sheets$sheet_name[i])
     dk_save_choropleth_map(choropleth_sheets$sheet_name[i], 4.3684, 3.25)
}
