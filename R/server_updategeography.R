# Update inputs across pages and according to map
observe({
  updateVarSelectInput(session, "higherGeography", selected = map_info$parent_area_name[1])
  updateVarSelectInput(session, "higherGeographyDiversity", selected = map_info$parent_area_name[1])
  updateVarSelectInput(session, "higherGeographyDNA", selected = map_info$parent_area_name[1])
  updateVarSelectInput(session, "higherGeographySpecies", selected = map_info$parent_area_name[1])
  updateVarSelectInput(session, "higherGeographyTaxonomy", selected = map_info$parent_area_name[1])
}) %>%
  bindEvent(map_info$parent_area_name)

observeEvent(input$higherGeography, {
    updateSelectInput(session, "higherGeographyDiversity", selected = input$higherGeography)
    updateSelectInput(session, "higherGeographyDNA", selected = input$higherGeography)
    updateSelectInput(session, "higherGeographySpecies", selected = input$higherGeography)
    updateSelectInput(session, "higherGeographyTaxonomy", selected = input$higherGeography)
  })

observeEvent(input$higherGeographyDiversity, {
    updateSelectInput(session, "higherGeography", selected = input$higherGeographyDiversity)
    updateSelectInput(session, "higherGeographyDNA", selected = input$higherGeographyDiversity)
    updateSelectInput(session, "higherGeographySpecies", selected = input$higherGeographyDiversity)
    updateSelectInput(session, "higherGeographyTaxonomy", selected = input$higherGeographyDiversity)
  })

observeEvent(input$higherGeographyDNA, {
    updateSelectInput(session, "higherGeography", selected = input$higherGeographyDNA)
    updateSelectInput(session, "higherGeographyDiversity", selected = input$higherGeographyDNA)
    updateSelectInput(session, "higherGeographySpecies", selected = input$higherGeographyDNA)
    updateSelectInput(session, "higherGeographyTaxonomy", selected = input$higherGeographyDNA)
  })

observeEvent(input$higherGeographySpecies, {
    updateSelectInput(session, "higherGeography", selected = input$higherGeographySpecies)
    updateSelectInput(session, "higherGeographyDiversity", selected = input$higherGeographySpecies)
    updateSelectInput(session, "higherGeographyDNA", selected = input$higherGeographySpecies)
    updateSelectInput(session, "higherGeographyTaxonomy", selected = input$higherGeographySpecies)
  })

observeEvent(input$higherGeographyTaxonomy, {
    updateSelectInput(session, "higherGeography", selected = input$higherGeographyTaxonomy)
    updateSelectInput(session, "higherGeographyDiversity", selected = input$higherGeographyTaxonomy)
    updateSelectInput(session, "higherGeographyDNA", selected = input$higherGeographyTaxonomy)
    updateSelectInput(session, "higherGeographySpecies", selected = input$higherGeographyTaxonomy)
  })