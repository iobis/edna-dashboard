species_gallery_modal <- function(species_name, species_link_1, species_link_2,
                                  sp_fb_common_names, sp_fb_content) {

    card_text <- bslib::card(
        htmltools::span(species_name, id = "species-cont-name"),
        htmltools::span(htmltools::HTML(paste("<p>", species_link_1, " | ", species_link_2, "</p>")),
            id = "species-cont-links"),
        htmltools::span(paste("Common names:", sp_fb_common_names), id = "species-cont-common"),
        htmltools::span(sp_fb_content, id = "species-cont-content"),
        full_screen = FALSE
    )

    card_table <- bslib::card(
        htmltools::div(reactable::reactableOutput("species_info"), style = "font-size:50%"),
        htmltools::div(
            htmltools::tags$span(
                bslib::input_switch("spi_dna_col", "Color DNA", TRUE),
                bslib::input_switch("spi_dna_back", "Background DNA", FALSE)
            ),
            id = "species-add-control"
        ),
        full_screen = FALSE
    )

    content <- list(
        card_text, card_table
    )

    grid <- bslib::layout_column_wrap(
        width = 1, heights_equal = "row",
        !!!content
    )

    shiny::modalDialog(grid, size = "xl",
        easyClose = T, footer = shiny::modalButton("Close"))
}