# Packages ----------------------------------------------------------------

library(tidyverse) 
library(readxl)
library(here)
library(janitor)
library(scales)



# Get County Data ----------------------------------------------------------------

dk_get_county_data <- function(sheet_name) {
     read_excel(here("data", "obtn-by-measure.xlsx"),
                sheet = sheet_name) %>% 
          clean_names() %>% 
          select(county, numeric_only, trend)
}

county_data_sheets <- here("data", "obtn-by-measure.xlsx") %>% 
     excel_sheets() %>% 
     tibble() %>% 
     set_names("sheet_name") %>% 
     mutate(n = row_number()) %>% 
     mutate(n = as.numeric(n)) %>% 
     filter(n >= 8) 



county_data <- map_df(county_data_sheets$sheet_name, dk_get_county_data,
                          .id = "sheet") %>% 
     mutate(sheet = as.numeric(sheet) + 7) %>% 
     left_join(county_data_sheets, by = c("sheet" = "n")) %>% 
     mutate(county = str_remove(county, "\\*")) %>% 
     filter(county != "Urban Oregon") %>% 
     filter(county != "Rural Oregon") %>% 
     filter(county != "Oregon") %>% 
     mutate(county = str_trim(county))


# Organize County Data -----------------------------------------------------------

# I'm doing this so that I can copy and paste into the tables

# Create custom round function
# Taken from https://stackoverflow.com/questions/12688717/round-up-from-5

round2 = function(x, n) {
     posneg = sign(x)
     z = abs(x)*10^n
     z = z + 0.5
     z = trunc(z)
     z = z/10^n
     z*posneg
}


county_data_organized <- county_data %>% 
     mutate(for_table = numeric_only) %>% 
     mutate(for_table = round2(for_table, 0)) %>% 
     add_row(sheet = 7.5, sheet_name = "SOCIAL") %>% 
     add_row(sheet = 7.5 + 5, sheet_name = "EDUCATION") %>% 
     add_row(sheet = 7.5 + 10, sheet_name = "ECONOMY") %>% 
     add_row(sheet = 7.5 + 15, sheet_name = "HEALTH") %>% 
     add_row(sheet = 7.5 + 20, sheet_name = "INFRASTRUCTURE") %>% 
     complete(sheet, county, fill = ) %>% 
     arrange(county, sheet) %>% 
     select(county, sheet_name, for_table, trend) %>% 
     mutate(for_table = case_when(
          sheet_name %in% c("Property Tax per Person") ~ dollar(for_table, 1),
          sheet_name %in% c("Child Abuse",
                            "Index Crime",
                            "Job Growth",
                            "Vehicle Miles Traveled",
                            "Letter Sounds") ~ number(for_table, big.mark = ","),
          TRUE ~ percent((for_table / 100), 1)
     )) %>% 
     mutate(for_table = na_if(for_table, "NA%"))

write_csv(county_data_organized, 
          "county-data-for-table.csv",
          na = "")



# Get Statewide Data ------------------------------------------------------

statewide_data <- map_df(county_data_sheets$sheet_name, dk_get_county_data,
                      .id = "sheet") %>% 
     mutate(sheet = as.numeric(sheet) + 7) %>% 
     left_join(county_data_sheets, by = c("sheet" = "n")) %>% 
     rename("geography" = "county") %>% 
     filter(geography %in% c("Urban", "Rural", "Oregon")) %>% 
     select(-trend)


# Organize Statewide Data -------------------------------------------------

# I gave this up because it didn't seem worth it
# 
statewide_data_organized <- statewide_data %>%
     complete(sheet, geography, fill = ) %>%
     mutate(for_table = numeric_only) %>%
     mutate(for_table = round(for_table, 0)) %>%
     add_row(sheet = 7.5, sheet_name = "SOCIAL") %>%
     add_row(sheet = 7.5 + 5, sheet_name = "EDUCATION") %>%
     add_row(sheet = 7.5 + 10, sheet_name = "ECONOMY") %>%
     add_row(sheet = 7.5 + 15, sheet_name = "HEALTH") %>%
     add_row(sheet = 7.5 + 20, sheet_name = "INFRASTRUCTURE") 
# %>%
#      spread(geography, for_table)
#      arrange(sheet) %>%
#      select(geography, sheet_name, for_table) %>%
#      mutate(for_table = case_when(
#           sheet_name %in% c("Property Tax per Person") ~ dollar(for_table),
#           sheet_name %in% c("Child Abuse",
#                             "Index Crime",
#                             "Job Growth",
#                             "Vehicle Miles Traveled") ~ number(for_table),
#           TRUE ~ percent((for_table / 100), 1)
#      )) %>%
#      mutate(for_table = na_if(for_table, "NA%"))



