# Download additional information from WHC sites and save as txt
download_site_descriptions <- function(outfile = "data/sites_description.txt", force = FALSE) {

  if (force) {
    download <- TRUE
  } else if (file.exists(outfile)) {
    mod <- as.Date(file.info(outfile)$mtime)
    if (mod == Sys.Date()) download <- FALSE else download <- TRUE
  } else {
    download <- TRUE
  }
  
  if (download) {
    message("Downloading sites descriptions")
    
    sites <- jsonlite::read_json("https://raw.githubusercontent.com/iobis/edna-tracker-data/data/generated.json")
    sites_list <- sites$sites
    sites_samples <- sites$samples
    
    sites_info <- xml2::read_xml("https://whc.unesco.org/en/list/xml")
    sites_info <- xml2::as_list(sites_info)
    sites_info <- sites_info[[1]]
    sites_info <- lapply(sites_info, function(x){
      data.frame(url = unlist(x$http_url), description = unlist(x$short_description))
    })
    sites_info <- do.call("rbind", sites_info)
    
    sites_details <- do.call("rbind", lapply(sites_list, function(x) data.frame(name = x$name, url = x$url)))
    sites_info <- dplyr::left_join(sites_info, sites_details)
    sites_info <- sites_info[!is.na(sites_info$name),]
    sites_info$description <- gsub("<p>|</p>", "", sites_info$description)
    
    write.table(sites_info, outfile, row.names = F)
  } else {
    message("Skipping download of site descriptions - file already up-to-date.")
  }

  return(invisible(NULL))

}

# Calculate site stats and save as RDS
calculate_site_stats <- function(force = FALSE) {

  if (!file.exists("data/supporting_data/sites_stats.rds") || force) {
    message("Creating `data/supporting_data/sites_stats.rds` for front page stats")
    source("R/occurrence.R")
    occurrence <- read_occurrence_data()
    groups <- read.csv("data/supporting_data/groups.csv")
    redlist <- read.csv("data/supporting_data/redlist.csv")
    redlist_cat <- redlist %>% filter(category %in% c("EN", "CR", "VU")) 
    
    occurrence_species <- occurrence %>% 
      filter(taxonRank=="species")
    
    n_species <- occurrence_species %>%
      group_by(higherGeography) %>% 
      summarise(unique_species = n_distinct(scientificName)) %>% 
      ungroup()
    
    n_fish_species <- occurrence_species %>%
      group_by(higherGeography) %>% 
      filter(class %in% groups[groups$group=="fish", "taxon"]) %>%
      summarise(unique_fish = n_distinct(scientificName)) %>% 
      ungroup()
    
    n_mammals_species <- occurrence_species %>%
      group_by(higherGeography) %>% 
      filter(class %in% groups[groups$group=="mammals", "taxon"]) %>%
      summarise(unique_mammals = n_distinct(scientificName)) %>% 
      ungroup()
    
    n_turtles_species <- occurrence_species %>%
      group_by(higherGeography) %>% 
      filter(order %in% groups[groups$group=="turtles", "taxon"]) %>%
      summarise(unique_turtles = n_distinct(scientificName)) %>% 
      ungroup()
    
    n_sharks_species <- occurrence_species %>%
      group_by(higherGeography) %>% 
      filter(class %in% groups[groups$group=="sharks", "taxon"]) %>%
      summarise(unique_sharks = n_distinct(scientificName)) %>% 
      ungroup()
    
    n_iucn_species <- occurrence_species %>%
      group_by(higherGeography) %>% 
      filter(scientificName %in% redlist_cat$species) %>%
      summarise(unique_iucn = n_distinct(scientificName)) %>% 
      ungroup()
    
    full_data <- n_species %>%
      left_join(n_fish_species) %>%
      left_join(n_mammals_species) %>%
      left_join(n_turtles_species) %>%
      left_join(n_sharks_species) %>%
      left_join(n_iucn_species)
    
    full_data[is.na(full_data)] <- 0
    
    saveRDS(full_data, "data/supporting_data/sites_stats.rds")
  }

  return(invisible(NULL))

}
