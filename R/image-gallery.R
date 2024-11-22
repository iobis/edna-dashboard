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
  
  source("extensions/inputextensions.R")
  
  image_table <- as.data.frame(data.table::fread(image_table))
  
  image_table_filt <- image_table[match(species, image_table$species),]

  image_table_filt <- image_table_filt[!is.na(image_table_filt$species),]

  # Possible correction, if wants to keep even species not available
  # if (any(!species %in% image_table_filt)) {
  #   not_av <- species[!species %in% image_table_filt]
  #   not_av_df <- data.frame(
  #     species = not_av,
  #     image_url = "images/placeholders/general.webp",
  #     alt_url = "images/placeholders/general.webp"
  #   )
  #   image_table_filt <- rbind(image_table_filt, not_av_df)
  # }

  if (!"alt_url" %in% colnames(image_table_filt)) {
    image_table_filt$alt_url <- "images/placeholders/general.webp"
  }
  
  if (nrow(image_table_filt) > 0) {
    cards <- list()
    
    for (i in 1:nrow(image_table_filt)) {

      link_id <- paste0("link_", gsub(" ", "_", image_table_filt$species[i]))
      
      header <- image_table_filt$species[i]
      
      cards[[i]] <- imageInput(
        inputId = link_id,
        header = header,
        image_src = image_table_filt$image_url[i],
        alt_src = image_table_filt$alt_url[i],
        max_image_height = max_image_height
      )
    }
    
    return(layout_column_wrap(
      width = max_image_width, gap = image_gap, fixed_width = TRUE,
      heights_equal = "row",
      !!!cards
    ))
  } else {
    return(NULL)
  }
}