# Update inputs across pages and according to map
observe({
  updateVarSelectInput(session, "higherGeography", selected = map_info$parent_area_name[1])
  updateVarSelectInput(session, "higherGeographyDiversity", selected = map_info$parent_area_name[1])
}) %>%
  bindEvent(map_info$parent_area_name)

observeEvent(input$higherGeography, {
    updateSelectInput(session, "higherGeographyDiversity", selected = input$higherGeography)
  })

observeEvent(input$higherGeographyDiversity, {
    updateSelectInput(session, "higherGeography", selected = input$higherGeographyDiversity)
  })