source("R/occurrence.R")
all_sp <- read_occurrence_data()
all_sp <- all_sp %>% distinct(scientificName, .keep_all = T) %>%
    select(species = scientificName, kingdom, phylum, class, order, family)

images <- data.table::fread("data/images.txt")

for (i in 1:nrow(images)) {
    cat("Processing image", i, "out of", nrow(images), "\n")
    tf <- images[i,]
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
                    img <- jpeg::readJPEG(timg)
                } else {
                    img <- png::readPNG(timg)
                }
                conv <- try(webp::write_webp(img, file.path("images/gallery", tf_out)))
                if (inherits(conv, "try-error")) {
                    file.copy(timg, gsub("webp", tools::file_ext(tf$image_url), file.path("images/gallery", tf_out)))
                }
                file.remove(timg)
            }
        } else {
            try(download.file(tf$image_url, file.path("images/gallery", tf_out)))
        }
    }
}


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

write.table(df_combined[, c("species", "image_url", "alt_url")], "data/proc_images.txt", row.names = F)
