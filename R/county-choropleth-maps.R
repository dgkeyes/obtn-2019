# Packages ----------------------------------------------------------------

library(tidyverse) 
library(ggrepel)
library(readxl)
library(scales)
library(extrafont)
library(janitor)
library(tigris)
library(here)
library(naniar)

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
     mutate(county = str_remove(county, "\\*")) %>% 
     filter(county != "Urban Oregon") %>% 
     filter(county != "Rural Oregon") %>% 
     filter(county != "Oregon") %>% 
     mutate(county = str_trim(county))
     



# Get Map Data and Merge It -----------------------------------------------

oregon_counties_geodata <- dk_oregon_counties_geodata() %>% 
     left_join(choropleth_data, by = c("name" = "county")) %>% 
     group_by(sheet_name) %>% 
     mutate(tertile_numeric = ntile(numeric_only, 3)) %>% 
     mutate(tertile_numeric = as.numeric(tertile_numeric)) %>% 
     mutate(tertile_text = case_when(
          tertile_numeric == 3 ~ " Top third ",
          tertile_numeric == 2 ~ " Middle third ",
          tertile_numeric == 1 ~ " Bottom third "
     )) %>% 
     # Add ID for all missing values
     mutate(tertile_text = replace_na(tertile_text, " ID ")) %>% 
     # Change ID to no college for higher ed enrollment
     mutate(tertile_text = case_when(
          sheet_name == "Higher ed enrollment" & tertile_text == " ID " ~ " No college ",
          TRUE ~ tertile_text
     )) %>% 
     mutate(tertile_text = factor(tertile_text, levels = c(" Top third ",
                                                           " Middle third ",
                                                           " Bottom third ",
                                                           " No college ",
                                                           " ID "))) 


# Get List of Oregon Counties ---------------------------------------------

oregon_counties <- dk_get_oregon_counties()

# Make maps ---------------------------------------------------------------

dk_make_choropleth_map <- function(measure) {
     ggplot(filter(oregon_counties_geodata, sheet_name == measure)) +
          geom_sf(aes(fill = tertile_text),
                  color = "white",
                  size = .5) +
          coord_sf(datum = NA) +
          scale_fill_manual(values = tfff_choropleth_colors) +
          tfff_map_theme +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          theme(legend.position = "bottom")
}

dk_save_choropleth_map <- function(plot_category) {
     
     plot_category <- str_to_lower(plot_category)
     plot_category <- str_replace_all(plot_category, " ", "-")
     
     ggsave(filename = paste0("plots/by-measure/choropleth-maps/2019-",
                              plot_category,
                              ".pdf"),
            device = cairo_pdf,
            width = 4.3684,
            height = 3.25)
}


for (i in 1:nrow(choropleth_sheets)) {
     dk_make_choropleth_map(choropleth_sheets$sheet_name[i])
     dk_save_choropleth_map(choropleth_sheets$sheet_name[i])
}
