sites_desc <- read.table("data/sites_description.txt", header = T)
sites <- sites_desc$name

figures <- list.files("images/front_gallery", full.names = T)

figures_table <- data.frame(site = NA, image_url = figures, caption = NA)

for (i in seq_len(nrow(figures_table))) {
    if (any(grepl(paste0(
        gsub("_.*", "", substr(basename(figures_table$image_url[i]), 1, 7))
    ), gsub(" |'", "", tolower(sites))))) {
        figures_table$site[i] <- sites[grepl(paste0(
            gsub("_.*", "", substr(basename(figures_table$image_url[i]), 1, 7))
        ), gsub(" |'", "", tolower(sites)))]
    }
}

figures_table$caption <- figures_table$site

write.table(figures_table, "data/front-images.txt", row.names = F)

system("aws s3 sync images/front_gallery/ s3://edna-dashboard/images/front_gallery/")
