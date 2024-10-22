# Prepare data that will be used on dashboard

# Additional data from sites
source("R/sites-details.R")
get_sites_details()

# Calculate front page statistics
force <- FALSE # Set here as TRUE to force update
if (!file.exists("data/supporting_data/sites_stats.rds") || force) {
    cat("Creating `data/supporting_data/sites_stats.rds` for front page stats.")
    source("R/occurrence.R")
    occurrence <- read_occurrence_data()
    groups <- read.csv("data/supporting_data/groups.csv")
    redlist <- read.csv("data/supporting_data/redlist.csv")
    redlist_cat <- redlist %>% filter(category %in% c("EN", "CR", "VU")) 

    occurrence_species <- occurrence %>% 
        filter(taxonRank=="species")

    n_species <- occurrence_species %>%
        group_by(higherGeography) %>% 
        summarise(unique_species = n_distinct(scientificName))

    n_fish_species <- occurrence_species %>%
        group_by(higherGeography) %>% 
        filter(class %in% groups[groups$group=="fish", "taxon"]) %>%
        summarise(unique_fish = n_distinct(scientificName))

    n_mammals_species <- occurrence_species %>%
        group_by(higherGeography) %>% 
        filter(class %in% groups[groups$group=="mammals", "taxon"]) %>%
        summarise(unique_mammals = n_distinct(scientificName))

    n_turtles_species <- occurrence_species %>%
        group_by(higherGeography) %>% 
        filter(order %in% groups[groups$group=="turtles", "taxon"]) %>%
        summarise(unique_turtles = n_distinct(scientificName))

    n_sharks_species <- occurrence_species %>%
        group_by(higherGeography) %>% 
        filter(class %in% groups[groups$group=="sharks", "taxon"]) %>%
        summarise(unique_sharks = n_distinct(scientificName))

    n_iucn_species <- occurrence_species %>%
        group_by(higherGeography) %>% 
        filter(scientificName %in% redlist_cat$species) %>%
        summarise(unique_iucn = n_distinct(scientificName))

    full_data <- n_species %>%
        left_join(n_fish_species) %>%
        left_join(n_mammals_species) %>%
        left_join(n_turtles_species) %>%
        left_join(n_sharks_species) %>%
        left_join(n_iucn_species)

    full_data[is.na(full_data)] <- 0

    saveRDS(full_data, "data/supporting_data/sites_stats.rds")

}
