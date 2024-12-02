wd <- waiter::Waiter$new(
  id = c("tax_bar_plot"),
  color = "white",
  html = htmltools::div(
    "Generating site statistics...",
    htmltools::br(), htmltools::br(),
    waiter::spin_1(), style = "color: #2780e3; font-size: 24px; font-weight: 900"
  )
)

wd2 <- waiter::Waiter$new(
  id = c("alpha_plot", "beta_plot"),
  color = "white",
  html = htmltools::div(
    "Generating site statistics...",
    htmltools::br(), htmltools::br(),
    waiter::spin_1(), style = "color: #2780e3; font-size: 24px; font-weight: 900"
  )
)

output$tax_bar_plot <- renderUI({
  req(input$higherGeography != "")

  wd$show()

  itax <- make_image_taxonomy(input$higherGeography, input$taxonLevelDiversity, input$plotType)

  width <- input$dimension[1]
  if (is.null(width)) {
    width <- 1100
  }

  if (width < 768) {
    page_card <- bslib::card(
      height = 500, full_screen = T,
      bslib::card_body(
        htmltools::span("Best viewed in landscape mode", style = "font-size: small; color: #949494"),
        plotOutput("tax_bar_plot_internal"),
        class = "p-0"
      )
    )
    output$tax_bar_plot_internal <- renderPlot({itax}, height = "auto")
  } else {
    itax <- plotly::ggplotly(
      itax + theme(axis.text.x = element_text(angle = 90))
    ) %>%
      plotly::layout(legend=list(x=0, 
                                 xanchor='left',
                                 yanchor='bottom',
                                 ypad = 0,
                                 y = -0.5,
                                 orientation='h'))
    page_card <- plotly::plotlyOutput("tax_bar_plot_internal", height = "500px")
    output$tax_bar_plot_internal <- plotly::renderPlotly({itax})
  }
  
  page_card
})

# output$tax_bar_plot <- renderPlot({
#   req(input$higherGeography != "")
#  #taxbarplot()  
#  make_image_taxonomy(occurrence_ds, input$higherGeography, input$taxonLevelDiversity, input$plotType)}, height=400
# )

# Diversity metrics
divplots <- reactive({
 req(input$higherGeography != "")
 wd2$show()
 get_alpha_diversity(input$higherGeography, input$taxonLevel, input$alpha_measure, input$beta_measure)
})

output$alpha_plot <- plotly::renderPlotly({
  plotly::ggplotly(divplots()[[1]])
})

output$beta_plot <- plotly::renderPlotly({
  plotly::ggplotly(divplots()[[2]])
})