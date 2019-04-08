
# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(fs)

# IMPORT DATA FROM DROPBOX ------------------------------------------------

file_copy("/Users/davidkeyes/Dropbox/ObtN 2019-2020/2019 ObtN/2019 ObtN Data/OBTN 2019 data by county.xlsx",
          here("data"),
          overwrite = T)

file_move(here("data", "OBTN 2019 data by county.xlsx"),
          here("data", "obtn-by-county.xlsx"))

file_copy("/Users/davidkeyes/Dropbox/ObtN 2019-2020/2019 ObtN/2019 ObtN Data/OBTN 2019 data by measure.xlsx",
          here("data"),
          overwrite = T)

file_move(here("data", "OBTN 2019 data by measure.xlsx"),
          here("data", "obtn-by-measure.xlsx"))




