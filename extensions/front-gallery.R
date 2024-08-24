#' A simple image gallery
#'
#' @param site name of the site
#' @param images_table a data.frame or a character with a path to a table
#'   containing three columns: site (site name), image_url (path to image),
#'   and caption. If the last is missing for any image, then the site name
#'   is used as a caption
#'
#' @return html component
#' @export
#'
#' @examples
#' \dontrun{
#'  images_table <- data.frame(
#' image_url = c("https://s1.static.brasilescola.uol.com.br/be/2020/06/oceano.jpg",
#'               "https://static.wixstatic.com/media/c4e423_2d038a02a77240ab8999aa853e85c01e~mv2.jpg/v1/fill/w_640,h_460,al_c,q_80,usm_0.66_1.00_0.01,enc_auto/c4e423_2d038a02a77240ab8999aa853e85c01e~mv2.jpg"),
#' caption = c("test 1", "test 2"),
#' site = "site"
#' )
#' front_gallery("site", images_table)
#' }
front_gallery <- function(site, images_table, show_counter = TRUE) {
  
  if (is.character(images_table)) {
    images_table <- data.table::fread(images_table)
  }
  
  images_table_sel <- images_table[images_table$site == site, ]
  total_length <- nrow(images_table_sel)
  
  if (total_length > 0) {
    images_table_sel$caption <- ifelse(is.na(images_table_sel$caption),
                                       site,
                                       images_table_sel$caption)
    
    inner_content_list <- lapply(1:nrow(images_table_sel), function(x){
      glue::glue('
                 htmltools::tags$div(
        class = "carousel-item active",
        htmltools::img(src = "{images_table_sel$image_url[x]}", class = "d-block w-100"),
        htmltools::tags$div(class = "carousel-caption", "{images_table_sel$caption[x]}")
      )
                 ')
    })
    
    inner_content <- paste0(unlist(inner_content_list), collapse = ",")
    
    inner_obj <- eval(parse(text = paste0(
      'htmltools::tags$div(
      class = "carousel-inner",',
      inner_content,
      ")"
    )))
    
    if (show_counter) {
      counter_obj <- htmltools::tags$div(
        id = "carousel-counter",
        class = "carousel-counter",
        paste0("1/", total_length) # Initially show "1/3" assuming you have 3 images
      )
    } else {
      counter_obj <- NULL
    }
    
    # Amazingly done by ChatGPT:
    # Create the image gallery HTML structure
    output <- htmltools::div(
      id = "carouselExample",
      class = "carousel slide",
      `data-bs-ride` = "carousel",
      
      # Counter for current image and total images
      counter_obj,
      
      # Carousel inner
      inner_obj,
      
      # Previous button
      htmltools::tags$button(
        class = "carousel-control-prev",
        type = "button",
        `data-bs-target` = "#carouselExample",
        `data-bs-slide` = "prev",
        htmltools::span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
        htmltools::span(class = "visually-hidden", "Previous")
      ),
      
      # Next button
      htmltools::tags$button(
        class = "carousel-control-next",
        type = "button",
        `data-bs-target` = "#carouselExample",
        `data-bs-slide` = "next",
        htmltools::span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
        htmltools::span(class = "visually-hidden", "Next")
      )
    )
    
    path <- normalizePath("./extensions")
    
    deps <- htmltools::htmlDependency(
      name = "front-gallery",
      version = "1.0.0",
      src = c(file = path),
      script = "front-gallery.js",
      stylesheet = "front-gallery.css"
    )
    
    return(htmltools::attachDependencies(bslib::card(output, min_height = "500px"), deps))
  } else {
    return(NULL)
  }
}
