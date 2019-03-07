library(fs)


dir_create("plots/")
dir_create("plots/by-county")
dir_create("plots/by-measure")
dir_create("plots/by-measure/choropleth-maps")
dir_create("plots/by-measure/industries")


for (i in 1:36) {
     dir_create(paste("plots/by county/", oregon_counties[i], sep =""))
}
