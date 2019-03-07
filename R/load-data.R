
# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(fs)


# BY COUNTY DATA -------------------------------------------------------------


# IMPORT DATA FROM DROPBOX ------------------------------------------------

file_copy("/Users/davidkeyes/Dropbox/ObtN 2019-2020/2019 ObtN/2019 ObtN Data/OBTN 2019 data by county.xlsx",
          here("data"),
          overwrite = T)


file_move(here("data", "OBTN 2019 data by county.xlsx"),
          here("data", "obtn-by-county.xlsx"))



# Read Data ---------------------------------------------------------------

county_data_path <- here("data", "obtn-by-measure.xlsx")

temp <- county_data_path %>% 
     excel_sheets() %>%
     # select(-1) %>% 
     set_names() %>% 
     map_df(~ read_excel(path = county_data_path, sheet = .x), .id = "sheet")
     


# Load Data ---------------------------------------------------------------

county_data <- read_excel(path = "data/OBTN 2018 final data by county.xlsx", sheet = 2, skip = 1) %>% 
     clean_names() %>% 
     mutate(geography = str_replace(geography, " County, Oregon", "")) %>%
     mutate(geography = str_replace(geography, "Rural Oregon", "Rural")) %>%
     mutate(geography = str_replace(geography, "Urban Oregon", "Urban")) %>%
     mutate(geography = str_to_lower(geography)) %>% 
     filter(geography != "rural") %>%
     filter(geography != "urban") %>%
     filter(geography != "oregon")

# Create Oregon counties data frame ----------------------------------------------

oregon_counties <- county_data %>% 
     pull(geography)


# BY MEASURE DATA ---------------------------------------------------------


file_copy("/Users/davidkeyes/Dropbox/ObtN 2019-2020/2019 ObtN/2019 ObtN Data/OBTN 2019 data by measure.xlsx",
          here("data"),
          overwrite = T)

file_move(here("data", "OBTN 2019 data by measure.xlsx"),
          here("data", "obtn-by-measure.xlsx"))

# MISC DATA ---------------------------------------------------------------


