library(worrms)
library(dplyr)
source("R/occurrence.R")


#This code fills in the taxonomic information based on the aphiaIDs in the dataset.
#This is done because we have many missing higher taxonomies (i.e. kingdom not known while the classification is to Chordata)
#This takes a long time, could/should be done in a previous workflow step? (Like the pipeline?)


occurrence <- read_occurrence_data()
occurrence <- occurrence %>%
  mutate(aphiaID = as.numeric(str_extract(scientificNameID, "\\d+$")))

# Extract all unique aphiaIDs
unique_ids <- unique(occurrence$aphiaID)

# Fetch taxonomic information for all unique aphiaIDs and store it in a list
taxonomic_info <- lapply(unique_ids, function(id) {
  # Fetch the taxonomic information for the current aphiaID
  res <- wm_record(id = id)
  
  # Extract the relevant taxonomic ranks (kingdom to genus)
  tibble(
    aphiaID = id,
    kingdom = res$kingdom,
    phylum = res$phylum,
    class = res$class,
    order = res$order,
    family = res$family,
    genus = res$genus
  )
})

# Combine the list into a single data frame
taxonomic_info_df <- bind_rows(taxonomic_info)

# Now merge the taxonomic information back to the original data frame by aphiaID
occurrence <- occurrence %>%
  left_join(taxonomic_info_df, by = "aphiaID") %>%
  mutate(
    kingdom = coalesce(kingdom.y, kingdom.x),
    phylum = coalesce(phylum.y, phylum.x),
    class = coalesce(class.y, class.x),
    order = coalesce(order.y, order.x),
    family = coalesce(family.y, family.x),
    genus = coalesce(genus.y, genus.x)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))  # Clean up the columns

