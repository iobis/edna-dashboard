# Download handlers

#### Front page UI
output$dataDownload <- renderText({
    req(input$higherGeography != "")

    cleanst <- function(string) {
        gsub(
            " ", "_",
            stringi::stri_trans_general(
                gsub(
                    "[[:punct:]]", "_",
                    tolower(string)
                ), "Latin-ASCII"
            )
        )
    }

    true_name <- cleanst(input$higherGeography)

    htmltools::HTML(
        paste(
            "<a href=",
             paste0("https://obis-edna-lists.s3.amazonaws.com/lists/csv/", true_name, ".csv"), " target='_blank'> Download data for", input$higherGeography, "</a>")
             )

}) %>%
    bindEvent(input$higherGeography)