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

temp <- median_income_data %>% 
     filter(geography %in% c("Sherman", "Oregon")) %>% 
     mutate(geography = fct_rev(geography))

ggplot(temp, aes(geography, median_income,
                 fill = geography)) +
     geom_col(width = 0.75) +
     # Add county/state name
     geom_text(label = temp$geography,
               aes(geography, 0),
               hjust = -0.125,
               color = "white",
               family = "Calibri") +
     # Add median income amount text
     geom_text(label = dollar(temp$median_income),
               aes(geography, median_income * .85),
               color = "white",
               family = "Calibri") +
     scale_fill_manual(values = c(tfff_medium_gray, tfff_dark_green)) +
     scale_x_discrete(expand = c(0, 0)) +
     scale_y_continuous(expand = c(0, 0)) +
     coord_flip() +
     tfff_bar_chart_theme

ggsave("temp.pdf", device = cairo_pdf,
       width = 2.43, 
       height = .61)
