#' Create a clickable image card
#'
#' @param inputId unique id
#' @param header text to go to the header of the card
#' @param image_src image src
#' @param alt_src alternative image src to be showed if unavailable
#' @param max_image_height maximum height of the image - should be on "100px" format
#'
#' @return card
#' @export
#' 
#' @details
#' This function acts exactly as [shiny::actionLink()]. Every time it is clicked
#' the `input$inputId` will change (increase 1, starting in 0).
#' 
#' As a difference, one object called `input$clicked_image_id` is created and
#' can be used to see which image was clicked. This uses JQuery and so is unique
#' across the whole Shiny app.
#' 
#'
#' @examples
#' \dontrun{
#' imageInput("imagelink_Species_A", "My species", image_src, "500px")
#' }
imageInput <- function(inputId,
                       header,
                       image_src,
                       alt_src = NULL,
                       max_image_height) {

  path <- normalizePath("./extensions")

  deps <- htmltools::htmlDependency(
    name = "extendedInputs",
    version = "1.0.0",
    src = c(file = path),
    script = "inputextensions.js",
    stylesheet = "inputextensions.css"
  )

  # card(image_src, full_screen = T,
  #      class = "image-butt-div",
  #      id = inputId)

  if (is.null(alt_src)) {
    alt_img <- NULL
  } else {
    alt_img <- glue::glue("this.onerror=null;this.src='{alt_src}';")
  }

  cont <- card(height = "100%", full_screen = T,
               #card_header(htmltools::tags$i(image_table_filt$species[i])),
               card_header(header, style = "font-style: italic;"),
               card_body(tags$img(src = image_src,
                                  class = "gallery-img",
                                  onerror = alt_img), class= "p-0"),
               max_height = max_image_height, id = inputId, class = "image-butt-div"
               )

  htmltools::attachDependencies(cont, deps)

}