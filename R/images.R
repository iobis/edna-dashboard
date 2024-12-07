library(rgbif)
library(purrr)
library(glue)
source("R/occurrence.R")

#' Download front gallery images
download_gallery_images <- function() {
  message("Downloading front gallery images from S3")
  system("aws s3 sync --no-sign-request s3://edna-dashboard/images/front_gallery images/front_gallery")
}

#' Generate list of GBIF image URLs, one for every species in the dataset
generate_image_list <- function(force = FALSE) {
  if (!file.exists("data/images.txt") || force) {
    message("Generating imnage list")
    occurrence <- read_occurrence_data()
    
    species_names <- occurrence %>% 
      filter(taxonRank == "species") %>% 
      pull(scientificName) %>% 
      unique()
    
    gbif_images <- map(species_names, function(species_name) {
      message(glue("Fetching GBIF image for species name {species_name}"))
      search_res <- try(occ_search(scientificName = species_name, mediaType = "StillImage", limit = 1, curlopts = list(timeout_ms = 10000))$media[[1]][[1]][[1]])
      if (!inherits(search_res, "try-error") && !is.null(search_res$identifier)) {
        data.frame(species = species_name,
          image_url = search_res$identifier,
          creator = ifelse(is.null(search_res$creator), NA, search_res$creator),
          publisher = ifelse(is.null(search_res$publisher), NA, search_res$publisher))
      } else {
         data.frame(species = species_name, image_url = NA, creator = NA, publisher = NA)
      }
    }) %>% 
      bind_rows()
    
    df <- as.data.frame(gbif_images)
    write.table(df, "data/images.txt", row.names = FALSE, quote = FALSE, sep = "\t", na = "")
  } else {
    message("Image list already exists")
  }
}

#' Create image database
create_images_database <- function() {

  fs::dir_create("images/gallery")
  
  all_sp <- read_occurrence_data()
  all_sp <- all_sp %>% distinct(scientificName, .keep_all = T) %>%
    select(species = scientificName, kingdom, phylum, class, order, family)
  
  images <- data.table::fread("data/images.txt")
  
  oldtm <- options("timeout")
  options(timeout = 15)
  
  for (i in 1:nrow(images)) {
    cat("Processing image", i, "out of", nrow(images), "\n")
    tf <- images[i,]
    if (is.na(tf$image_url)|| tf$image_url == "") next
    if (tools::file_ext(tf$image_url) %in% c("jpeg", "jpg", "png")) {
      tf_out <- paste0(gsub(" ", "_", tolower(tf$species)), ".webp")
    } else {
      tf_out <- paste0(gsub(" ", "_", tolower(tf$species)), ".", tools::file_ext(tf$image_url))
    }
    if (!file.exists(file.path("images/gallery", tf_out))) {
      if (tools::file_ext(tf$image_url) %in% c("jpeg", "jpg", "png")) {
        timg <- paste0("images/temp.", tools::file_ext(tf$image_url))
        dw <- try(download.file(tf$image_url, timg))
        if (!inherits(dw, "try-error")) {
          if (grepl("jp", tools::file_ext(tf$image_url))) {
            img <- try(jpeg::readJPEG(timg), silent = T)
          } else {
            img <- try(png::readPNG(timg), silent = T)
          }
          if (inherits(img, "try-error")) {
            file.copy(timg, gsub("webp", tools::file_ext(tf$image_url), file.path("images/gallery", tf_out)))
          } else {
            conv <- try(webp::write_webp(img, file.path("images/gallery", tf_out)), silent = T)
            if (inherits(conv, "try-error")) {
              file.copy(timg, gsub("webp", tools::file_ext(tf$image_url), file.path("images/gallery", tf_out)))
            }
          }
          file.remove(timg)
        }
      } else {
        try(download.file(tf$image_url, file.path("images/gallery", tf_out)))
      }
    }
  }
  
  options(timeout = oldtm$timeout)
  
  all_images <- list.files("images/gallery")
  
  all_images_table <- data.frame(
    species = stringr::str_to_sentence(gsub("_", " ", tools::file_path_sans_ext(all_images))),
    image_url = paste0("images/gallery/", all_images)
  )
  
  df_combined <- merge(images, all_images_table, by = "species", all.x = TRUE,
                       suffixes = c("_old", "_new"))
  
  df_combined$image_url <- ifelse(!is.na(df_combined$image_url_new), df_combined$image_url_new, df_combined$image_url_old)
  
  # Add groups and placeholders
  all_sp <- all_sp %>%
    mutate(alt_url = case_when(
      kingdom %in% c("Archaea", "Bacteria", "Fungi", "Protozoa", "Viruses") ~ "images/placeholders/bacteria.webp",
      kingdom %in% c("Chromista", "Plantae") ~ "images/placeholders/algae.webp",
      phylum == "Annelida" ~ "images/placeholders/worms.webp",
      phylum == "Cnidaria" ~ "images/placeholders/jelly.webp",
      phylum == "Arthropoda" ~ "images/placeholders/crustaceans.webp",
      phylum == "Echinodermata" ~ "images/placeholders/urchins.webp",
      phylum == "Mollusca" ~ "images/placeholders/molluscs.webp",
      class == "Mammalia" ~ "images/placeholders/mammals.webp",
      class == "Teleostei" | class == "Coelacanthi" | 
        class == "Petromyzonti" | class == "Elasmobranchii" | class == "Holocephali" ~ "images/placeholders/fishes.webp",
      .default = "images/placeholders/general.webp"
    ))
  
  df_combined <- merge(df_combined, all_sp[,c("species", "alt_url")], by = "species", all.x = TRUE)

  df_combined <- df_combined[, c("species", "image_url", "alt_url", "image_url_old", "creator", "publisher")]

  # Check species not available
  not_available <- all_sp %>%
    filter(!species %in% df_combined$species)

  not_available <- not_available %>%
    mutate(
      image_url = alt_url,
      image_url_old = "", creator = "", publisher = ""
    ) %>%
    select(species, image_url, alt_url, image_url_old, creator, publisher)

  final_table <- bind_rows(df_combined, not_available)
  
  write.table(final_table, "data/proc_images.txt", row.names = F)
  
}
