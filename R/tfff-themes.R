
# Packages ----------------------------------------------------------------

library(extrafont)

loadfonts()

# Define colors -----------------------------------------------------------

tfff_dark_green <- "#265142"
tfff_light_green <- "#B5CC8E"
tfff_orange <- "#e65100"
tfff_yellow <- "#FBC02D"
tfff_blue <- "#283593"
tfff_red <- "#B71C1C"
tfff_dark_gray <- "#545454"
tfff_medium_gray <- "#a8a8a8"
tfff_light_gray <- "#eeeeee"

tfff_blues <- c("#1a237e", "#303f9f", "#3f51b5", "#7986cb", "#c5cae9" )
tfff_oranges <- c("#ffe0b2", "#ffb74d", "#fb8c00", "#e65100")


# tfff_choropleth_colors <- c("#dddddd", 
#                             "#B5CC8E", 
#                             "#6E8F68", 
#                             "#265142")

tfff_choropleth_colors <- rev(c("#dddddd",
                                "#B5CC8E", 
                                "#6E8F68", 
                                "#265142"))


# Misc --------------------------------------------------------------------

tfff_base_size <- 2
tfff_point_size <- tfff_base_size * 10
tfff_line_point_size <- tfff_base_size * 10
tfff_stroke_size <- tfff_base_size * 1
tfff_label_size <- tfff_base_size * 3



# Themes ------------------------------------------------------------------

tfff_base_theme <- theme(
     panel.grid.minor = element_blank(),
     panel.background = element_rect(fill = "transparent",colour = NA),
     plot.background = element_rect(fill = "transparent",colour = NA),
     text = element_text(color = tfff_dark_gray, 
                         family = "Calibri",
                         size = 10),
     title = element_text(family = "Calibri"),
     axis.text = element_text(color = tfff_dark_gray),
     axis.title = element_blank(),
     axis.ticks = element_blank(),
     panel.grid.major = element_line(color = tfff_light_gray),
     legend.position = "none",
     plot.margin = margin(-3, -3, -3, -3, "pt"),
     panel.spacing = margin(-3, -3, -3, -3, "pt")
)

tfff_area_theme <- tfff_base_theme + theme (
     panel.grid.major.x = element_blank()
)

tfff_line_theme <- tfff_base_theme + theme (
     panel.grid.major.x = element_blank()
)

tfff_line_theme_faceted <- tfff_area_theme + theme (
     panel.spacing = unit(1, "lines"),
     strip.background = element_rect(fill = "white"),
     panel.grid.major.x = element_blank(),
     strip.text = element_blank()
     # axis.text.x = element_blank()
)

tfff_bar_chart_theme <- tfff_base_theme + theme(
     axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_blank()
)

tfff_column_chart_theme <- tfff_base_theme + theme(
     axis.text.y = element_blank(),
     axis.text.x = element_text(color = tfff_dark_gray),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_blank()
)

tfff_population_pyramid_theme <- tfff_base_theme + theme(
     axis.text.x = element_text(color = tfff_dark_gray),
     axis.text.y = element_blank(),
     panel.grid.major.x = element_line(color = tfff_light_gray),
     panel.grid.major.y = element_blank(),
     plot.margin = margin(3, 3, 3, 3, "pt"),
     panel.spacing = margin(3, 3, 3, 3, "pt")
)

tfff_bar_chart_with_benchmark_theme <- tfff_base_theme + theme(
     axis.text.x = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_blank()
)


tfff_map_theme <- tfff_base_theme + theme(
     panel.grid.major.x = element_blank(),
     panel.grid.major.y = element_blank(),
     axis.text = element_blank(),
     legend.position = "none",
     legend.title = element_blank()
)