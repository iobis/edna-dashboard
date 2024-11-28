# HOME server code
# Main map ----
#sites_shape <- sf::read_sf("https://samples.ednaexpeditions.org/sites.geojson") # transfer to start server
output$mainMap <- renderLeaflet({
  sites_shape <- sites_shape[sites_shape$name %in% localities$parent_area_name,]
  leaflet(width = "100%") %>%
   addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") %>%
   addPolygons(stroke = TRUE, color = "#efa16e", weight = 1, opacity = 0.6, fill = TRUE,
    label = ~name, data = sites_shape, layerId = ~name) %>%
   addMarkers(~lon, ~lat, popup = ~as.character(popup_name), label = ~as.character(popup_name),
              layerId = ~as.character(popup_name),
              data = map_localities, clusterOptions = markerClusterOptions(maxClusterRadius = 5)) %>%
   setView(0, 0, zoom = 2)
})

# Context info ----
output$higherGeography <- renderText({
  if (input$higherGeography == "") {
    general_description$title[[1]]
  } else {
    sel_site <- sites_info[sites_info$name == input$higherGeography,]
    sel_site$name
  }
})

output$siteDescription <- renderUI({
  if (input$higherGeography == "") {
    htmltools::HTML(general_description$description[[1]])
  } else {
    sel_site <- sites_info[sites_info$name == input$higherGeography,]
    sel_site$description
  }
})

output$whs_front_code <- renderText({
  if (input$higherGeography == "") {
    ""
  } else {
    sel_site <- sites_info[sites_info$name == input$higherGeography,]
    paste("World Heritage Site", paste(basename(sel_site$url), " "), " |")
  }
})

output$whs_front_website <- renderUI({
  sel_site <- sites_info[sites_info$name == input$higherGeography,]
  htmltools::a(
    href = sel_site$url, target = "_blank",
    paste("  ", sel_site$url), style = "text-decoration: none; font-color: #2761e3 !important;"
  )
})

output$eventDate <- renderText({
  if (input$higherGeography == "") {
    format(as.Date("2022-09-16"), "%Y-%m-%d")
  } else {
    sa <- samples %>%
      filter(site == input$higherGeography)

    sa$date <- as.Date(sa$date)

    format(min(sa$date, na.rm = T), "%Y-%m-%d")
  }
})
output$eventSamples <- renderText({
  if (input$higherGeography == "") {
    samples %>%
      count() %>%
      unlist()
  } else {
    samples %>%
      filter(site == input$higherGeography) %>%
      count() %>%
      unlist()
  }
})

# Value boxes ----

### NEW
source("R/value_box_gen.R")
output$valueBoxes <- renderUI({
  if (input$higherGeography == "") {
    htmltools::div(
      value_box(
        title = "Number of species", value = "value_box_species",
        icon = "shrimp", icon_source = "fa", icon_color = "#2b88d0", width = "100%"
      ),
      htmltools::span("Number of species by groups",
        style = "padding-left: 20px; padding-top: 30px; color: #8c8c8c; font-size: 14px; font-weight: 600;"
      ),
      htmltools::div(
        value_box(
          title = "Fishes", value = "value_box_fish", icon = "shrimp",
          icon_source = "fa", width = "50%", direction = "v", icon_color = "#6f42c1"
        ),
        value_box(
          title = "Mammals", value = "value_box_mammals", icon = "otter",
          icon_source = "fa", width = "50%", direction = "v", icon_color = "#6f42c1"
        ),
        style = "display: flex; flex-direction: row; justify-content: space-between; width: 100%;"
      ),
      htmltools::div(
        value_box(
          title = "Sharks", value = "value_box_sharks", icon = "images/icons/shark.svg",
          icon_source = "other", width = "50%", direction = "v", icon_color = "#6f42c1"
        ),
        value_box(
          title = "Turtles", value = "value_box_turtles", icon = "images/icons/turtle.svg",
          icon_source = "other", width = "50%", direction = "v", icon_color = "#6f42c1"
        ),
        style = "display: flex; flex-direction: row; justify-content: space-between; width: 100%;"
      ),
      htmltools::span("IUCN Red List",
        style = "padding-left: 20px; padding-top: 30px; color: #8c8c8c; font-size: 14px; font-weight: 600;"
      ),
      value_box(
        title = "Threatened species", value = "value_box_iucn",
        icon = "circle-exclamation", icon_source = "fa", icon_color = "#2b88d0", width = "100%"
      ), style = "width: 100%;"
    )
  } else {
    htmltools::div(
      htmltools::div(
        value_box(
          title = "Number of species", value = "value_box_species",
          icon = "shrimp", icon_source = "fa", icon_color = "#2b88d0", width = "50%"
        ),
        value_box(
          title = "Threatened species", value = "value_box_iucn",
          icon = "circle-exclamation", icon_source = "fa", icon_color = "#2b88d0", width = "50%"
        ),
        style = "display: flex; flex-direction: row; justify-content: space-between; width: 100%;"
      ),
      htmltools::span("Number of species by groups",
        style = "padding-left: 20px; padding-top: 30px; color: #8c8c8c; font-size: 14px; font-weight: 600;"
      ),
      htmltools::div(
        value_box(
          title = "Fishes", value = "value_box_fish", icon = "shrimp",
          icon_source = "fa", width = "25%", direction = "v", icon_color = "#6f42c1"
        ),
        value_box(
          title = "Mammals", value = "value_box_mammals", icon = "otter",
          icon_source = "fa", width = "25%", direction = "v", icon_color = "#6f42c1"
        ),
        value_box(
          title = "Sharks", value = "value_box_sharks", icon = "images/icons/shark.svg",
          icon_source = "other", width = "25%", direction = "v", icon_color = "#6f42c1"
        ),
        value_box(
          title = "Turtles", value = "value_box_turtles", icon = "images/icons/turtle.svg",
          icon_source = "other", width = "25%", direction = "v", icon_color = "#6f42c1"
        ),
        style = "display: flex; flex-direction: row; justify-content: space-between; width: 100%;"
      ), style = "width: 100%;"
    )
  }
})

####



boxes_data <- reactiveValues()
observe({
  boxes_data$data <- n_species(input$higherGeography, occurrence)
}) %>% bindEvent(input$higherGeography)

output$value_box_species <- renderText({
  boxes_data$data$n_species
})

output$value_box_fish <- renderText({
  boxes_data$data$n_fish
})

output$value_box_mammals <- renderText({
  boxes_data$data$n_mammals
})

output$value_box_sharks <- renderText({
  ceiling(boxes_data$data$n_sharks)
})

output$value_box_turtles <- renderText({
  boxes_data$data$n_turtles
})

output$value_box_iucn <- renderText({
  boxes_data$data$n_iucn
})

# Front page gallery ----
output$imageGalleryFront <- renderUI({
  site <- input$higherGeography

  images_table <- "data/front-images.txt"
  images_table <- data.table::fread(images_table)
  images_table <- as.data.frame(images_table)

  if (site == "") {
    images_table_sel <- images_table %>%
      dplyr::slice_sample(n = 6)
  } else {
    images_table_sel <- images_table[images_table$site == site, ]
  }

  # if (nrow(images_table_sel) > 4) {
  #   images_table_sel <- images_table_sel[1:4,]
  # }

  cards_images <- lapply(seq_len(nrow(images_table_sel)), function(x) {
    url <- images_table_sel$image_url[x]
    caption <- images_table_sel$caption[x]

    if (length(unique(images_table_sel$caption)) > 1 && site != "") {
      card(
        height = "100%", full_screen = T,
        card_header(
          caption # , style = "font-style: italic;"
        ),
        card_body(tags$img(
          src = url
        ), class = "p-0")#,
        #max_height = 300
      )
    } else {
      card(
        height = "100%", full_screen = T,
        card_body(tags$img(
          src = url
        ), class = "p-0")#,
        #max_height = 300
      )
    }
  })

  bslib::layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    !!!cards_images,
  )

})

# Map changes
# Map input changes ----
map_info <- reactiveValues(parent_area_name = NULL)

observe({
  if (!is.null(input$mainMap_shape_click)) {
    click <- input$mainMap_shape_click
    #print(click)
    if (!is.null(click$id)) {
      locs <- localities[!is.na(localities$station),]
      loc_sel <- locs[locs$parent_area_name == click$id, ]
      map_info$parent_area_name <- loc_sel$parent_area_name[1]
    }
  }
}) %>%
  bindEvent(input$mainMap_shape_click, ignoreInit = T)

observe({
  if (!is.null(input$mainMap_marker_click)) {
    click <- input$mainMap_marker_click
    #print(click)
    if (!is.null(click$id)) {
      locs <- map_localities[!is.na(map_localities$popup_name),]
      loc_sel <- locs[locs$popup_name == click$id, ]
      map_info$parent_area_name <- loc_sel$parent_area_name[1]
    }
  }
}) %>%
  bindEvent(input$mainMap_marker_click, ignoreInit = T)


observe({
  proxy <- leafletProxy("mainMap")

  # sel_loc <- localities[localities$parent_area_name == input$higherGeography,]
  # sel_loc <- sel_loc[!is.na(sel_loc$lon) & !is.na(sel_loc$lat),]
  # lng1 <- min(sel_loc$lon)
  # lat1 <- min(sel_loc$lat)
  # lng2 <- max(sel_loc$lon)
  # lat2 <- max(sel_loc$lat)
  # print(c(lng1, lng2, lat1, lat2))
  # if (nrow(sel_loc) > 0) {
  #   proxy %>% flyToBounds(lng1 = lng1, lat1 = lat1,
  #                         lng2 = lng2, lat2 = lat2,
  #                         options = list(duration = 0.95))
  # }

  feature_data <- sites_shape[sites_shape$name == input$higherGeography, ]
  bbox <- sf::st_bbox(feature_data)
  
  lng1 <- unname(bbox["xmin"])
  lat1 <- unname(bbox["ymin"])
  lng2 <- unname(bbox["xmax"])
  lat2 <- unname(bbox["ymax"])

  proxy %>% flyToBounds(lng1 = lng1, lat1 = lat1,
                        lng2 = lng2, lat2 = lat2,
                        options = list(duration = 0.95))
  
}) %>%
  bindEvent(input$higherGeography, ignoreInit = T)

observe({
  proxy <- leafletProxy("mainMap")
  
  sel_loc <- localities[localities$parent_area_name == input$higherGeography,]
  sel_loc <- sel_loc[!is.na(sel_loc$lon) & !is.na(sel_loc$lat),]
  
  if (nrow(sel_loc) > 0) {
    proxy %>% setView(0, 0, zoom = 2)
  }
}) %>%
  bindEvent(input$homeTrigger, ignoreInit = T)