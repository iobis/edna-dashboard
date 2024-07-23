library(dplyr)
library(stringr)
library(purrr)

#' Reads the full occurrence dataset.
read_occurrence_data <- function() {
  occurrence_files <- list.files("output", "*Occurrence*", full.names = TRUE)
  occurrence <- map(occurrence_files, read.table, sep = "\t", quote = "", header = TRUE, comment.char = "") %>%
    bind_rows() %>%
    mutate_if(is.character, na_if, "")
}
