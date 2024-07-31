#' Generate the image gallery html
#'
#' @param species vector with species names (scientific name)
#' @param image_table the path to the image table
#' @param max_image_height max height of images
#' @param max_image_width max width of images
#' @param image_gap gap between elements
#'
#' @return html code
#' @export
#'
#' @examples
#' \dontrun{
#' generate_gallery("Acanthurus chirurgus", "images.txt")
#' }
generate_gallery <- function(species,
                             image_table,
                             max_image_height = "500px",
                             max_image_width = "400px",
                             image_gap = "0.1em") {
  
  image_table <- as.data.frame(data.table::fread(image_table))
  
  image_table_filt <- image_table[image_table$species %in% species,]
  
  if (nrow(image_table_filt) > 0) {
    cards <- list()
    
    for (i in 1:nrow(image_table_filt)) {
      cards[[i]] <- card(height = "100%", full_screen = T,
                         card_header(htmltools::tags$i(image_table_filt$species[i])),
                         card_body(tags$img(src = image_table_filt$image_url[i],
                                            class = "gallery-img"), class= "p-0"), max_height = max_image_height
      )
    }
    
    return(layout_column_wrap(
      width = max_image_width, gap = image_gap,
      heights_equal = "row",
      !!!cards
    ))
  } else {
    return(NULL)
  }
}