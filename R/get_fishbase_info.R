get_fishbase_info <- function(force = FALSE) {

  if (!file.exists("data/fishbase.rds") || force) {
    message("Fetching fishbase data")

    # Load species list 
    suppressPackageStartupMessages(library(dplyr))
    source("R/occurrence.R")
    
    sites_info <- read.table("data/sites_description.txt", header=T)
    occurrence <- read_occurrence_data()
    
    sp_level <- occurrence[occurrence$taxonRank == "species",]
    
    # Get info from Fishbase, if available
    fish_base_info <- rfishbase::species(unique(na.omit(sp_level$scientificName)))
    fish_base_info$source <- "FishBase"
    fish_base_info <- fish_base_info %>%
      select(Species, SpecCode, Author, PicPreferredName, BodyShapeI, DemersPelag, 
             DepthRangeShallow, DepthRangeDeep, Length, Weight, Importance, UsedforAquaculture,
             Aquarium, Dangerous, Comments, source)
    
    # Get other marine species information 
    sp_sealifebase <- sp_level %>% filter(!scientificName %in% fish_base_info$Species)
    sealife_base_info <- rfishbase::species(unique(na.omit(sp_sealifebase$scientificName)), server="sealifebase")
    sealife_base_info$source <- "SeaLifeBase"
    sealife_base_info <- sealife_base_info %>%
      select(Species, SpecCode, Author, PicPreferredName, BodyShapeI, DemersPelag, 
             DepthRangeShallow, DepthRangeDeep, Length, Weight, Importance, UsedforAquaculture,
             Aquarium, Dangerous, Comments, source)
    
    #combine the two tables
    fish_base_info <- bind_rows(fish_base_info, sealife_base_info)
    
    fish_base_info$Comments <- gsub(" \\(Ref\\. [0-9,; ]+\\)", "", fish_base_info$Comments)
    #fish_base_info$Comments <- gsub(" \\(Ref\\. [0-9]+\\)", "", fish_base_info$Comments)
    
    # fish_base_commons <- rfishbase::common_names(unique(na.omit(sp_level$scientificName)))
    # sealife_base_commons <- rfishbase::common_names(unique(na.omit(sp_sealifebase$scientificName)), server="sealifebase")
    
    # fish_base_commons <- bind_rows(fish_base_commons, sealife_base_commons)
    
    # fish_base_commons <- fish_base_commons %>%
    #   group_by(Species) %>%
    #   slice_head(n = 3) %>% 
    #   ungroup()
    
    # fish_base_commons <- fish_base_commons[!is.na(fish_base_commons$ComName), ]
    
    # fish_base_commons <- fish_base_commons %>%
    #   filter(Language == "English")
    
    
    saveRDS(list(
      core_info = fish_base_info#,
      #common_names = fish_base_commons
    ), file = "data/fishbase.rds")
    
  }  else {
    message("Fishbase information already present")
  }
}
