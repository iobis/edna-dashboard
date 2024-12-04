# Species summary page
updateSelectizeInput(session, "speciesSummariesSelect", choices = c("Select a species" = "", unique_sp), server = TRUE)


# Create structure
species_sum_int <- function(species, image_table) {
    image_table <- as.data.frame(data.table::fread(image_table))

    image_table_filt <- image_table[match(species, species), ]

    image_table_filt <- image_table_filt[!is.na(image_table_filt$species), ]

    image_src <- image_table_filt$image_url
    alt_src <- image_table_filt$alt_url

    alt_img <- glue::glue("this.onerror=null;this.src='{alt_src}';")

    credits <- c(
        paste0(
          ifelse(image_table_filt$creator == "", "", "Creator: "),
          image_table_filt$creator
        ),
        paste0(
          ifelse(image_table_filt$publisher == "", "", "Publisher: "),
          image_table_filt$publisher
        ),
        paste0(
          ifelse(image_table_filt$image_url_old == "", "", "Source: "),
          image_table_filt$image_url_old
        )
      )
    credits <- credits[credits != ""]
    credits <- paste(
        credits, collapse = " | "
    )

    image_top <- card(
        height = "100%", full_screen = T,
        card_body(htmltools::tags$img(
            src = image_src,
            onerror = alt_img, title = credits
        ), class = "p-0"),
        max_height = 200
    )

    stats_side <- card(
        card_header("Statistics"),
        "Number of records on OBIS: 20"
    )

    column_1 <- bslib::layout_column_wrap(
        width = 1, heights_equal = "row",
        # Image
        card(htmltools::tags$i(species), class = "species-summ-title"),
        image_top,
        stats_side
    )

    species_content <- card(
        htmltools::br(),
        "Dorsal spines (total): 9; Dorsal soft rays (total): 24 - 27; Anal spines: 3; Anal soft rays: 22 - 24. Brown in color when preserved; with or without fine bluish gray longitudinal lines on body; pale pectoral fins with upper edge narrowly black; pelvic fins brown. Lips blackish brown; median upper teeth tend to be pointed. Dorsal fin base with a prominent black spot larger than 1/2 eye diameter; a smaller spot on base of anal fin. Groove of caudal spine encircled with a narrow black margin. Gill rakers on anterior row:20-24; on posterior row:18-23.",
        htmltools::span("Where it was found?", style = "font-size: 16px; font-weight: 800; color: #2780e3"),
        htmltools::tags$ul(
            htmltools::tags$li("Aldabra Atoll"), htmltools::tags$li("Fernando de Noronha")
        )
        )

    bslib::layout_column_wrap(
        fill = FALSE, height = 500,
        style = htmltools::css(grid_template_columns = "1fr 2fr", margin_top = "20px"),
        column_1, species_content
    )
}


# Render tab
output$speciesSummaries <- renderUI({
    species_sum_int(input$speciesSummariesSelect, "data/proc_images.txt")
})
