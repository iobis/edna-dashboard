library(dplyr)
library(stringr)
library(purrr)

#' Reads the full occurrence dataset.
read_occurrence_data <- function() {
  occurrence_files <- list.files("data/output", "*Occurrence*", full.names = TRUE)
  
  dna_files <- list.files("data/output", "*DNADerivedData*", full.names = TRUE)
  
  dna <- map(dna_files, read.table, sep = "\t", quote = "", header = TRUE) %>%
    bind_rows() %>%
    mutate_if(is.character, na_if, "")

  occurrence <- map(occurrence_files, read.table, sep = "\t", quote = "", header = TRUE, comment.char = "") %>%
    bind_rows() %>%
    mutate_if(is.character, na_if, "")%>%
    left_join(dna, by = "occurrenceID")

  return(occurrence)
}
