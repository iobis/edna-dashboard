add_image_sidebar <- function(...) {
  
  path <- normalizePath("./extensions")
  
  deps <- htmltools::htmlDependency(
    name = "image-sidebar",
    version = "1.0.0",
    src = c(file = path),
    script = "image-sidebar.js",
    stylesheet = "image-sidebar.css"
  )
  
  output <- card(
    htmltools::tags$button(htmltools::HTML('<i class="bi bi-x-circle-fill" style="color: #CED6DC;"></i>'), id = "close-btn-sidebar"),
    ...,
    id = "image-sidebar", full_screen = T
  )
  
  htmltools::attachDependencies(output, deps)
  
}