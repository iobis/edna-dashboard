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

  occurrence <- occurrence %>%
  mutate(aphiaID = as.numeric(str_extract(scientificNameID, "\\d+$")))

 #Fix taxonomic levels 
 worms_tax_levels <- read.csv2("data/supporting_data/worms_tax_levels.csv")

#  merge the taxonomic information to the dataframe by aphiaID
occurrence <- occurrence %>%
  left_join(worms_tax_levels, by = "aphiaID") %>%
  mutate(
    kingdom = coalesce(kingdom.y, kingdom.x),
    phylum = coalesce(phylum.y, phylum.x),
    class = coalesce(class.y, class.x),
    order = coalesce(order.y, order.x),
    family = coalesce(family.y, family.x),
    genus = coalesce(genus.y, genus.x)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))  # Clean up the columns

}
