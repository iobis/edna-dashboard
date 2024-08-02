library(dplyr)
library(tidyr)

#This is a placeholder for getting the information we want to depict on species in the sidebar (or other location)
#Should include Title, and vernacular name
#   Some species general information (fetch from fishbase?)
#   The DNA sequences of the species, and the closest matches in the database (with links?), scrollable table?
#   Which markers found the species
#   How many reads
#   Which samples

#This is to extract all the wanted information from identificationRemarks (currently still text)
#However the new version of PacMAN, will allow the this to be easier (info stored in json)
extract_info <- function(text) {
  if (str_detect(text, "No VSEARCH hits at 0.97 identity")) {
    id <- NA
    identity <- NA
    taxonomy_text <- NA
    confidences <- str_extract(text, "(?<=confidences\\s)([\\d\\.]+;?)+")
    last_confidence <- str_extract(confidences, "[^;]+$") %>% str_remove("\\.$")
  } else {
    id <- str_extract(text, "(?<=hits\\s)[^,]+")
    identity <- str_extract(text, "(?<=identities\\s)[^,]+")
    taxonomy_matches <- str_match_all(text, "taxonomy\\s([^,]+)")
    confidences <- str_extract(text, "(?<=confidences\\s)([\\d\\.]+;?)+")
    last_confidence <- str_extract(confidences, "[^;]+$") %>% str_remove("\\.$")
    if (length(taxonomy_matches[[1]]) >= 2) {
      taxonomy_text <- taxonomy_matches[[1]][2, 2]  # Second instance of taxonomy
    } else {
      taxonomy_text <- NA
    }
  }
  list(Confidence=last_confidence, ID = id, Identity = identity, Taxonomy = taxonomy_text)
}


get_species_information <- function(selected_species, occurrence, site) {

    all_info <- occurrence %>%
        filter(higherGeography == site) %>%
        filter(scientificName == selected_species)
        
    species_info <- paste("This species was found in", length(unique(all_info$materialSampleID)), "samples. The total number of reads for this species was:", sum(all_info$organismQuantity), "across", length(unique(all_info$DNA_sequence)))
    #return(species_info)

    extracted_info <- all_info %>%
        rowwise() %>%
        mutate(info = list(extract_info(identificationRemarks))) %>%
        unnest_wider(info)


    species_table <- extracted_info  %>% 
        group_by(DNA_sequence) %>% 
        summarize(  samples = paste(unique(materialSampleID), collapse = ","), 
                    localities = paste(unique(locality), collapse = ","), 
                    target_gene = paste(unique(target_gene), collapse = ","),
                    reads = sum(organismQuantity),
                    rdp_confidences=paste(unique(Confidence), collapse = ","),
                    vsearch_identity = if_else(all(is.na(Identity)), NA, paste(Identity, collapse = ",")),
                    NCBI_ID = paste(unique(ID), collapse = ",")) %>%
        relocate(DNA_sequence, .after = last_col())
    
    return(species_table)

}

#NCBI_IDs links to add structure: https://www.ncbi.nlm.nih.gov/nuccore/EU869849.1/