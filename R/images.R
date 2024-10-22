library(rgbif)
library(purrr)
library(glue)

#' Generate list of GBIF image URLs, one for every species in the dataset
generate_image_list <- function() {
  occurrence <- read_occurrence_data()
  
  species_names <- occurrence %>% 
    filter(taxonRank == "species") %>% 
    pull(scientificName) %>% 
    unique()
  
  gbif_images <- map(species_names, function(species_name) {
    message(glue("Fetching GBIF image for species name {species_name}"))
    url <- occ_search(scientificName = species_name, mediaType = "StillImage", limit = 1, curlopts = list(timeout_ms = 10000))$media[[1]][[1]][[1]]$identifier
    url
  }) %>% 
    map(~ifelse(is.null(.x), NA, .x)) %>% 
    unlist() 

  data.frame(species = species_names, image_url = gbif_images)
}

df <- generate_image_list()
write.table(df, "data/images.txt", row.names = FALSE, quote = FALSE, sep = "\t", na = "")
