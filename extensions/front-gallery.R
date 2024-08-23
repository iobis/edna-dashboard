front_gallery <- function(site, images_table) {
  images_table <- data.table::fread(images_table)
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
    
    # Amazingly done by ChatGPT:
    # Create the image gallery HTML structure
    output <- htmltools::div(
      id = "carouselExample",
      class = "carousel slide",
      `data-bs-ride` = "carousel",
      
      # Counter for current image and total images
      htmltools::tags$div(
        id = "carousel-counter",
        class = "carousel-counter",
        "1/3" # Initially show "1/3" assuming you have 3 images
      ),
      
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
