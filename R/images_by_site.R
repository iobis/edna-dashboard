library(rgbif)
library(purrr)
library(glue)

# Order species names based on abundance, and fetch images
generate_image_list_bySite <- function(site) {

  occurrence <- read_occurrence_data()

  # Add categories for further filtering
  groups=read.csv("data/supporting_data/groups.csv")
  group_lookup <- setNames(groups$group, paste(groups$rank, groups$taxon, sep = "_"))

  # Add a new column for group
  occurrence <- occurrence %>%
    filter(higherGeography == site) %>%
    filter(taxonRank == "species") %>% 
    mutate(group = coalesce(
      group_lookup[paste("class", class, sep = "_")],
      group_lookup[paste("order", order, sep = "_")],
      group_lookup[paste("phylum", phylum, sep = "_")]
    ))
  
  species_names <- occurrence %>% 
    group_by(scientificName, group) %>% 
    summarise(abundance=sum(organismQuantity)) %>% 
    arrange(-abundance) 

  redlist <- read.csv("data/supporting_data/redlist.csv")
  colnames(redlist)[1] <- "scientificName"

  species_names <- species_names %>% left_join(redlist)

  image_list=read.csv("data/images.txt", sep="\t")
  colnames(image_list)[1] <- "scientificName"

  species_names <- species_names %>% left_join(image_list)

  return(species_names)
}
