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

#  merge the taxonomic information to the dataframe by aphiaID only if the value is not NA
#occurrence <- occurrence %>%
#  left_join(worms_tax_levels, by = "aphiaID") %>%
#  mutate(
#    kingdom = coalesce(kingdom.y, kingdom.x),
#    phylum = coalesce(phylum.y, phylum.x),
#    class = coalesce(class.y, class.x),
 #   order = coalesce(order.y, order.x),
#   family = coalesce(family.y, family.x),
#   genus = coalesce(genus.y, genus.x)
 # ) %>%
 # select(-ends_with(".x"), -ends_with(".y"))  # Clean up the columns

# replace all values based on the worms values (also na) 
occurrence <- occurrence %>%
  left_join(worms_tax_levels, by = "aphiaID") %>%
  mutate(
    kingdom = kingdom.y,
    phylum = phylum.y,
    class = class.y,
    order = order.y,
    family = family.y,
    genus = genus.y
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

#Other qc (still remaining, add this to a previous workflow step)

#Remove primates (already added to qc, but remove for now here)
occurrence = occurrence %>% filter(order!="Primates"|is.na(order))


#Issues with blast results for the 12Steleo fragment. Consider removing these at least at species level (or filtering from blast results)
#occurrence %>% filter(pcr_primer_name_forward=="teleo_F_L1848",  grepl("blast", identificationRemarks, ignore.case = TRUE), taxonRank=="species")

#-------------------------#
# Remove singletons - to be discussed?
#-------------------------#

occurrence_clean_no_singletons <- occurrence  %>%
  group_by(DNA_sequence, higherGeography) %>%
  mutate(total_abundance = sum(organismQuantity)) %>%
  ungroup() %>%
  filter(total_abundance > 1) %>%
  select(-total_abundance)

occurrence <- occurrence_clean_no_singletons

#-------------------------#
# Remove AA's and CC's
#-------------------------#
is_all_A_or_C <- function(seq) {
  return(grepl("^[AC]+$", seq))
}

occurrences <- occurrence %>%
  filter(!sapply(DNA_sequence, is_all_A_or_C))

#-------------------------#
# Remove prokaryotic contaminants after renaming
#-------------------------#
 occurrence <- occurrence %>% filter(kingdom !="Bacteria"|is.na(kingdom), kingdom !="Archaea"|is.na(kingdom), kingdom !="Fungi"|is.na(kingdom), kingdom !="Viruses"|is.na(kingdom))

#-------------------------#
# Remove cross-contaminants?
#-------------------------#

#These mostly removed in the annotation step. 


}
