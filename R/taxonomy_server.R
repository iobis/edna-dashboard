# Taxonomy tab server component
taxon_table <- reactive({ 
  if (input$higherGeography == "") {
    data.frame(matrix(ncol = 2, nrow = 0))
  } else {
    make_table_taxonomy(occurrence_ds, input$higherGeography)
  }
})

output$rtable <- renderReactable({
   taxon_table()
})

# Output Krona plot
output$krona_plot <- renderUI({
    if (input$higherGeography != "") {
      sname <- input$higherGeography
      sname <- gsub("'", " ", sname)
      sname <- gsub("[^[:alnum:] ]", "", sname)
      sname <- gsub(" ", "_", sname)
      sname <- tolower(sname)
      sname <- stringi::stri_trans_general(sname, "Latin-ASCII")
      tags$iframe(src = paste0("www/krona_plots/", paste0(sname, ".html")), height=600, width="100%")
    } else {
      tags$br()
    }
  })
  