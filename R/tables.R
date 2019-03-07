library(gt)

tab_rtf <-
     gtcars %>%
     dplyr::select(mfr, model) %>%
     dplyr::slice(1:2) %>%
     gt() %>%
     tab_header(
          title = md("Data listing from **gtcars**"),
          subtitle = md("`gtcars` is an R dataset")
     ) %>%
     as_rtf()

gtcars %>% 
     select(hp:hp_rpm) %>%
     gt() %>% 
     data_color(hp_rpm,
                apply_to = "text",
                colors = "red")
