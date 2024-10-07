# HOME server code
# Main map ----
sites_shape <- sf::read_sf("https://samples.ednaexpeditions.org/sites.geojson")
output$mainMap <- renderLeaflet({
  sites_shape <- sites_shape[sites_shape$name %in% localities$parent_area_name,]
  leaflet(width = "100%") %>%
   addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") %>%
   addPolygons(stroke = TRUE, color = "#efa16e", weight = 1, opacity = 0.6, fill = TRUE,
    label = ~name, data = sites_shape, layerId = ~name) %>%
   addMarkers(~lon, ~lat, popup = ~as.character(area_name), label = ~as.character(area_name),
              layerId = ~as.character(area_name),
              data = localities, clusterOptions = markerClusterOptions()) %>%
   setView(0, 0, zoom = 2)
})

# Context info ----
output$higherGeography <- renderText({
  sel_site <- sites_info[sites_info$name == input$higherGeography,]
  sel_site$name
})

output$siteDescription <- renderText({
  sel_site <- sites_info[sites_info$name == input$higherGeography,]
  sel_site$description
})

output$whs_front_code <- renderText({
  sel_site <- sites_info[sites_info$name == input$higherGeography,]
  paste(basename(sel_site$url), " ")
})

output$whs_front_website <- renderUI({
  sel_site <- sites_info[sites_info$name == input$higherGeography,]
  htmltools::a(
    href = sel_site$url, target = "_blank",
    paste("  ", sel_site$url), style = "text-decoration: none; font-color: #2761e3 !important;"
  )
})

output$eventDate <- renderText({"2024-05-01"})
output$eventSamples <- renderText({"1000"})

# Value boxes ----
boxes_data <- reactiveValues()
observe({
  sel_site <- sites_info[sites_info$name == input$higherGeography,]
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
  front_gallery(site, "data/front-images.txt", show_counter = FALSE)
})

# Map changes
# Map input changes ----
map_info <- reactiveValues(parent_area_name = NULL)

observe({
  if (!is.null(input$mainMap_shape_click)) {
    click <- input$mainMap_shape_click
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
    if (!is.null(click$id)) {
      locs <- localities[!is.na(localities$station),]
      loc_sel <- locs[locs$area_name == click$id, ]
      map_info$parent_area_name <- loc_sel$parent_area_name[1]
    }
  }
}) %>%
  bindEvent(input$mainMap_marker_click, ignoreInit = T)


observe({
  proxy <- leafletProxy("mainMap")
  
  sel_loc <- localities[localities$parent_area_name == input$higherGeography,]
  sel_loc <- sel_loc[!is.na(sel_loc$lon) & !is.na(sel_loc$lat),]
  
  if (nrow(sel_loc) > 0) {
    proxy %>% flyToBounds(lng1 = min(sel_loc$lon), lat1 = min(sel_loc$lat),
                          lng2 = max(sel_loc$lon), lat2 = max(sel_loc$lat))
  }
}) %>%
  bindEvent(input$higherGeography, ignoreInit = T)