#Calculate base values based on site selection
#Calculate all 4 values in a list



n_species <- function(site, occurrence){
    
    data <- list()

    #site="Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems"

    sites_stats <- readRDS("data/supporting_data/sites_stats.rds")

    if (site == "") {
        # stats_sel <- sites_stats %>%
        #     summarise(across(2:ncol(.), sum))
        #temporary workaround
        # TODO: update sites_stats
        stats_sel <- data.frame(
            unique_species = 4436,
            unique_fish = 1995,
            unique_mammals = 30,
            unique_iucn = 128,
            unique_turtles = 3,
            unique_sharks = 86
        )
    } else {
        stats_sel <- sites_stats %>% 
            filter(higherGeography==site)
    }

    data$n_species <- stats_sel$unique_species

    data$n_fish <- stats_sel$unique_fish
    
    data$n_mammals <- stats_sel$unique_mammals
    
    data$n_iucn <- stats_sel$unique_iucn

    data$n_turtles <- stats_sel$unique_turtles

    data$n_sharks <- stats_sel$unique_sharks

    return(data)
}

