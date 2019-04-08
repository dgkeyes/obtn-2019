library(fs)


dir_create("plots/")
dir_create("plots/by-county")
dir_create("plots/by-measure")
dir_create("plots/by-measure/choropleth-maps")
dir_create("plots/by-measure/employment-industries")
dir_create("plots/by-measure/population-pyramid")
dir_create("plots/by-measure/race-ethnicity-maps")
dir_create("plots/by-measure/race-ethnicity-charts")
dir_create("plots/by-measure/tribes")


for (i in 1:36) {
     county <- oregon_counties[i]
     county <- str_to_lower(county)
     county <- str_replace_all(county, " ", "-")
     
     dir_create(paste("plots/by-county/", county, sep =""))
}
