#Calculate base values based on site selection
#Calculate all 4 values in a list



n_species <- function(site, occurrence){
    
    data <- list()

    #site="Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems"

    occurrence_species <- occurrence %>% 
    filter(higherGeography==site) %>% 
    filter(taxonRank=="species")

    groups=read.csv("data/supporting_data/groups.csv")
    redlist <- read.csv("data/supporting_data/redlist.csv")
    redlist_cat <- redlist %>% filter(category %in% c("EN", "CR", "VU")) 

    data$n_species <- occurrence_species %>%   
        summarise(unique_species = n_distinct(scientificName)) %>%
        pull(unique_species)

    data$n_fish <- occurrence_species %>%  
        filter(class %in% groups[groups$group=="fish", "taxon"]) %>%
        summarise(unique_species = n_distinct(scientificName)) %>% 
        pull(unique_species)
    
    data$n_mammals <- occurrence_species %>%  
        filter(class %in% groups[groups$group=="mammals", "taxon"]) %>%
        summarise(unique_species = n_distinct(scientificName)) %>% 
        pull(unique_species)
    
    data$n_iucn <- occurrence_species %>%  
        filter(scientificName %in% redlist_cat$species) %>%
        summarise(unique_species = n_distinct(scientificName)) %>% 
        pull(unique_species)

    return(data)
}

