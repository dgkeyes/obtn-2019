# Run this file to make all plots

library(here)
library(beepr)


# Make directories --------------------------------------------------------

source(here("R", "make-directories.R"))


# Make plots --------------------------------------------------------------

source(here("R", "county-choropleth-maps.R"))
source(here("R", "employment-industries.R"))
source(here("R", "inset-maps.R"))
source(here("R", "median-income.R"))
source(here("R", "population-pyramids.R"))
source(here("R", "race-ethnicity-statewide-maps.R"))
source(here("R", "race-ethnicity.R"))


# Copy to Dropbox ---------------------------------------------------------

source(here("R", "copy-plots-to-dropbox.R"))


# Beep --------------------------------------------------------------------

beep()
