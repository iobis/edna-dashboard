# Taxonomy tab server component
taxon_table <- reactive({ 
  make_table_taxonomy(occurrence_ds, input$higherGeography)
})

output$rtable <- renderReactable({
   taxon_table()
})

# Output Krona plot
output$krona_plot <- renderUI({
    # TO BE IMPROVED: would be better to have short names as one of the columns
    # sel_loc <- localities[localities$parent_area_name == input$higherGeography,]
    # sel_loc <- substr(tolower(sel_loc$parent_area_name[1]), 1, 4)
    # f <- list.files("www/krona_plots")
    # f <- f[grepl(sel_loc, f)]
    sname <- input$higherGeography
    sname <- gsub("'", " ", sname)
    sname <- gsub("[^[:alnum:] ]", "", sname)
    sname <- gsub(" ", "_", sname)
    sname <- tolower(sname)
    sname <- stringi::stri_trans_general(sname, "Latin-ASCII")
    tags$iframe(src = paste0("www/krona_plots/", paste0(sname, ".html")), height=600, width="100%")
  })
  