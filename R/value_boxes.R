#Calculate base values based on site selection
#Calculate all 4 values in a list



n_species <- function(site, occurrence){
    
    data <- list()

    #site="Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems"

    sites_stats <- readRDS("data/supporting_data/sites_stats.rds")

    stats_sel <- sites_stats %>% 
        filter(higherGeography==site)

    data$n_species <- stats_sel$unique_species

    data$n_fish <- stats_sel$unique_fish
    
    data$n_mammals <- stats_sel$unique_mammals
    
    data$n_iucn <- stats_sel$unique_iucn

    data$n_turtles <- stats_sel$unique_turtles

    data$n_sharks <- stats_sel$unique_sharks

    return(data)
}

