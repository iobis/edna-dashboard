#Calculate base values based on site selection

n_species <- function(site){

    occurrence %>% 
    filter(higherGeography==site) %>% 
    filter(taxonRank=="species") %>%   
    summarise(unique_species = n_distinct(scientificName)) %>%
    pull(unique_species)

}


n_fish <- function(site){

    groups=read.csv("data/supporting_data/groups.csv")

    occurrence %>%
    filter(higherGeography==site) %>% 
    filter(taxonRank=="species") %>%
    filter(class %in% groups[groups$group=="fish", "taxon"]) %>%
    summarise(unique_species = n_distinct(scientificName)) %>%
    pull(unique_species)

}

n_mammal <- function(site){

    groups=read.csv("data/supporting_data/groups.csv")

    occurrence %>%
    filter(higherGeography == site) %>% 
    filter(taxonRank == "species") %>%
    filter(class %in% groups[groups$group=="mammals", "taxon"]) %>%
    summarise(unique_species = n_distinct(scientificName)) %>%
    pull(unique_species)

}

n_iucn<- function(site){

    redlist <- read.csv("data/supporting_data/redlist.csv")
    redlist_cat <- redlist %>% 
                        filter(category %in% c("EN", "CR", "VU")) 

    occurrence %>%
    filter(higherGeography==site) %>% 
    filter(taxonRank=="species") %>%
    filter(scientificName %in% redlist_cat$species) %>%
    summarise(unique_species = n_distinct(scientificName)) %>%
    pull(unique_species)

}

