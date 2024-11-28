value_box <- function(title, valueId, icon,
                      background = "#f8f9fa", icon_source = "bi", icon_color = NULL,
                      width = 250, height = NULL, direction = "h") {

    if (icon_source == "bi") {
        icon_obj <- bsicons::bs_icon(icon)
    } else if (icon_source == "fa") {
        icon_obj <- shiny::icon(icon)
    } else {
        icon_obj <- htmltools::img(src = icon,
        width = "65px", height = "35px", opacity = 0.3)
    }

    if (is.character(width)) {
        style_box <- paste0(
            "background-color: ", background, "; width: ", width, ";"
        )
    } else {
        style_box <- paste0(
            "background-color: ", background, "; width: ", width, "px;"
        )
    }

    if (!is.null(height)) {
        style_box <- paste0(style_box, " height: ", height, "px;")
    }

    if (direction != "h") {
        style_box <- paste0(style_box, " flex-direction: column;")
    }

    htmltools::div(
        htmltools::div(
            icon_obj,
            class = "icon-column", style = ifelse(is.null(icon_color), "", paste0("color: ", icon_color, ";"))
        ),
        htmltools::div(
            htmltools::div(
                title,
                class = "vb-title"
            ),
            htmltools::div(
                shiny::textOutput(valueId),
                class = "vb-value"
            ),
            class = "vb-text-column"
        ),
        class = "value-box", style = style_box
    )
}
