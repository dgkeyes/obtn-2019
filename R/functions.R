# Packages ----------------------------------------------------------------

library(tidyverse)
library(tigris)
library(here)


# List of counties --------------------------------------------------------

dk_get_oregon_counties <- function() {
     read_excel(here("data", "obtn-by-county.xlsx"), 
                sheet = 2) %>% 
          clean_names() %>% 
          filter(geography != "Rural") %>% 
          filter(geography != "Urban") %>% 
          filter(geography != "Oregon") %>% 
          pull(geography) 
}


# Maps stuff --------------------------------------------------------------

dk_oregon_tracts_geodata <- function() {
     tracts("OR", cb = T, class="sf") %>% 
          clean_names()
}

dk_oregon_counties_geodata <- function() {
     counties(cb = T, class="sf") %>% 
          clean_names() %>% 
          filter(statefp == 41)
}

dk_oregon_state_geodata <- function() {
     states(cb = T, class="sf") %>% 
          clean_names() %>% 
          filter(statefp == 41)
}


# Save plots --------------------------------------------------------------

dk_save_plot <- function(plot_category, plotwidth, plotheight) {
     ggsave(filename = paste0("./plots/by-county/",
                              oregon_counties[i],
                              "/",
                              plot_category,
                              "-",
                              oregon_counties[i],
                              ".pdf"),
            device = cairo_pdf,
            height = plotheight,
            width = plotwidth, 
            units = "in")
}

dk_save_plot_by_measure <- function(plot_category, plotwidth, plotheight) {
     ggsave(filename = paste0("plots/by-measure/",
                             plot_category,
                             "/",
                             oregon.counties[i],
                             ".pdf"),
            device = cairo_pdf,
            width = plotwidth,
            height = plotheight)
}

