# Packages ----------------------------------------------------------------

library(tidyverse) 
library(ggrepel)
library(readxl)
library(scales)
library(janitor)
library(here)



# Load stuff --------------------------------------------------------------

source(here("R", "tfff-themes.R"))
source(here("R", "functions.R"))

# Age Order ---------------------------------------------------------------

# Create this vector to use later to plot things in order

age_order <- c("0-4",
               "5-9",
               "10-14",
               "15-19",
               "20-24",
               "25-29",
               "30-34",
               "35-39",
               "40-44",
               "45-49",
               "50-54",
               "55-59",
               "60-64",
               "65-69",
               "70-74",
               "75-79",
               "80-84",
               "85+")  

# Get Data ----------------------------------------------------------------

population_pyramid <- read_excel(here("data", "obtn-by-county.xlsx"),
                                 sheet = "Gender Age") %>% 
     clean_names() %>% 
     gather("age_gender", "pct", -geography) %>% 
     # Add gender variable
     mutate(gender = case_when(
          str_detect(age_gender, "females") ~ "female",
          TRUE ~ "male"
     )) %>% 
     mutate(age_gender = str_remove(age_gender, "females_")) %>% 
     mutate(age_gender = str_remove(age_gender, "males_")) %>% 
     mutate(age_gender = str_replace(age_gender, "_", "-")) %>% 
     mutate(age_gender = str_replace(age_gender, "80-85", "80-84")) %>%  
     mutate(age_gender = str_replace(age_gender, "85", "85+")) %>% 
     # Since we've removed gender from the age_gender variable, let's rename it to age
     rename("age" = "age_gender") %>% 
     # Just reorder to make things nice
     select(geography, age, gender, pct) %>% 
     # If pct is less than .01 it won't show up in the final plot so anything that's less than .01 gets changed to .01
     mutate(pct = case_when(
          pct < .01 ~ .01,
          TRUE ~ pct
     )) %>% 
     # Add padding to age_labels variables that are shorter so they also show up equal length
     mutate(age_labels = case_when(
          str_length(age) == 3 ~ str_pad(age, width = 7, side = "both", pad = " "),
          TRUE ~ age
     )) %>% 
     # Make age a factor using the age_order vector above
     mutate(age = fct_relevel(age, age_order)) %>% 
     mutate(pct_formatted = case_when(
          gender == "female" ~ -pct,
          gender == "male" ~ pct
     ))


# Plotting Function -------------------------------------------------------

dk_population_pyramid_plot <- function(county_name) {
     
     # Create data frame for this county
     
     population_pyramid_filtered <- population_pyramid %>%
          filter(geography == county_name)
     
     
     # Define labels etc
     
     largest_group_pct <- max(population_pyramid_filtered$pct)
     
     if (largest_group_pct < .04) {
          population_pyramid_labels <- c("4%", "2%", 
                                         "0", 
                                         "2%", "4%")  
          population_pyramid_limit <- .04
          population_pyramid_labels_placement <- .01
          
          population_pyramid_filtered <- population_pyramid_filtered %>% 
               mutate(pct = ifelse(pct <= .01, .008, pct)) %>%
               mutate(pct_formatted = ifelse(gender == "female", -pct, pct))
          
     } else if (largest_group_pct < .06) {
          population_pyramid_labels <- c("6%", "4%", "2%", 
                                         "0", 
                                         "2%", "4%", "6%")
          
          population_pyramid_limit <- .06
          population_pyramid_labels_placement <- .01
          
          population_pyramid_filtered <- population_pyramid_filtered %>% 
               mutate(pct = ifelse(pct <= .011, .012, pct)) %>%
               mutate(pct_formatted = ifelse(gender == "female", -pct, pct))
          
     } else if (largest_group_pct < .08) {
          population_pyramid_labels <- c("8%", "6%", "4%", "2%", 
                                         "0", 
                                         "2%", "4%", "6%", "8%")  
          
          population_pyramid_limit <- .08
          population_pyramid_labels_placement <- .02
          
          population_pyramid_filtered <- population_pyramid_filtered %>% 
               mutate(pct = ifelse(pct < .019, .015, pct)) %>%
               mutate(pct_formatted = ifelse(gender == "female", -pct, pct))
          
     } else if (largest_group_pct < .1) {
          population_pyramid_labels <- c("10%", "8%", "6%", "4%", "2%", 
                                         "0", 
                                         "2%", "4%", "6%", "8%", "10%")  
          population_pyramid_limit <- .1
          population_pyramid_labels_placement <- .03
          
          population_pyramid_filtered <- population_pyramid_filtered %>% 
               mutate(pct = ifelse(pct < .019, .02, pct)) %>%
               mutate(pct_formatted = ifelse(gender == "female", -pct, pct))
          
     } 
     
     
     ggplot(population_pyramid_filtered, aes(x = age, y = pct_formatted, 
                                             fill = gender,
                                             frame = geography)) +
          geom_hline(yintercept = 0, color = "white") +
          geom_bar(data = population_pyramid_filtered, 
                   stat = "identity",
                   width = .7) +
          # Add labels in middle of two sets of bars
          geom_label(label = population_pyramid_filtered$age_labels, 
                     aes(x = age, y = 0), 
                     fill = "white",
                     family = "Calibri",
                     label.size = NA, 
                     color = tfff_dark_gray) +
          # Add men label
          geom_label(aes(x = 17, y = (population_pyramid_limit - 
                                           population_pyramid_labels_placement) * 1 ),
                     label = "Men",
                     color = "white",
                     family = "Calibri",
                     fill = tfff_dark_green,
                     label.size = 0,
                     label.r = unit(0, "lines"),
                     label.padding = unit(.3, "lines")) +
          geom_label(aes(x = 17, y = (population_pyramid_limit -
                                           population_pyramid_labels_placement) * -1),
                     label = "Women",
                     color = "white",
                     family = "Calibri",
                     fill = tfff_light_green,
                     label.size = 0,
                     label.r = unit(0, "lines"),
                     label.padding = unit(.3, "lines")) +
          coord_flip() +
          scale_y_continuous(breaks = seq(population_pyramid_limit * -1, 
                                          population_pyramid_limit, 
                                          by = .02),
                             limits = c(population_pyramid_limit * -1, 
                                        population_pyramid_limit),
                             labels = population_pyramid_labels) +
          scale_fill_manual(values = c(tfff_light_green, tfff_dark_green)) +
          tfff_population_pyramid_theme
     
     
     
}



# Save functions -----------------------------------------------------------

dk_save_county_population_pyramid <- function(county) {
     county <- str_to_lower(county)
     county <- str_replace(county, " ", "-")
     
     ggsave(filename = paste0("plots/by-county/",
                              county,
                              "/2019-population-pyramid-",
                              county,
                              ".pdf"),
            device = cairo_pdf,
            width = 3.27,
            height = 4.0752)
}




# Make all county plots ----------------------------------------------------------

oregon_counties <- dk_get_oregon_counties()

for (i in 1:36) {
     dk_population_pyramid_plot(oregon_counties[i])
     dk_save_county_population_pyramid(oregon_counties[i])
}



# Make statewide plots ----------------------------------------------------

dk_save_statewide_population_pyramid <- function(geography) {
     geography <- str_to_lower(geography)
     geography <- str_replace(geography, " ", "-")
     
     ggsave(filename = paste0("plots/by-measure/population-pyramid/2019-population-pyramid-",
                              geography,
                              ".pdf"),
            device = cairo_pdf,
            width = 3,
            height = 4)
}


dk_population_pyramid_plot_statewide <- function(state_urban_rural) {
     
     population_pyramid_filtered <- population_pyramid %>%
          filter(geography == state_urban_rural)
     
     temp <<- population_pyramid_filtered
     
     population_pyramid_labels <- c("5%", "4%", "3%", "2%", "1%",
                                    "0",
                                    "1%", "2%", "3%", "4%", "5%")
     population_pyramid_limit <- .05
     population_pyramid_labels_placement <- .01

     
     ggplot(population_pyramid_filtered, aes(x = age, y = pct_formatted, 
                                             fill = gender,
                                             frame = geography)) +
          geom_hline(yintercept = 0, color = "white") +
          geom_bar(data = population_pyramid_filtered, 
                   stat = "identity",
                   width = .7) +
          # Add labels in middle of two sets of bars
          geom_label(label = population_pyramid_filtered$age_labels, 
                     aes(x = age, y = 0), 
                     fill = "white",
                     family = "Calibri",
                     label.size = NA, 
                     color = tfff_dark_gray) +
          # Add men label
          geom_label(aes(x = 17, y = .04),
                         label = "Men",
                         color = "white",
                         family = "Calibri",
                         fill = tfff_dark_green,
                         label.size = 0,
                         label.r = unit(0, "lines"),
                         label.padding = unit(.3, "lines")) +
          geom_label(aes(x = 17, y = -.04),
                         label = "Women",
                         color = "white",
                         family = "Calibri",
                         fill = tfff_light_green,
                         label.size = 0,
                         label.r = unit(0, "lines"),
                         label.padding = unit(.3, "lines")) +
          coord_flip() +
          scale_y_continuous(breaks = seq(-.05,
                                          .05,
                                          by = .01),
                             limits = c(-.05,
                                        .05),
                             labels = population_pyramid_labels) +
          scale_fill_manual(values = c(tfff_light_green, tfff_dark_green)) +
          tfff_population_pyramid_theme
     
}

dk_population_pyramid_plot_statewide("Urban")
dk_save_statewide_population_pyramid("Urban")

dk_population_pyramid_plot_statewide("Rural")
dk_save_statewide_population_pyramid("Rural")

dk_population_pyramid_plot_statewide("Oregon")
dk_save_statewide_population_pyramid("Oregon")
