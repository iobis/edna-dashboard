cat("Adjusting navbar")

file <- "index.html"

content <- readLines(file, warn = F)

navbar <- which(grepl("nav-link-text", content))

content_to_change <- content[navbar]

content_update <- gsub("Home", '<i class="bi bi-house"></i> Home', content_to_change)

content_update <- gsub("Taxonomy", '<i class="bi bi-bar-chart-steps"></i> Taxonomy', content_update)
content_update <- gsub("Diversity", '<i class="bi bi-boxes"></i> Diversity', content_update)
content_update <- gsub("Species", '<i class="bi bi-bug"></i> Species', content_update)
content_update <- gsub("Sample", '<i class="bi bi-droplet"></i> Sample', content_update)
content_update <- gsub("DNA", '<i class="bi bi-eyedropper"></i> DNA', content_update)
content_update <- gsub("Climate", '<i class="bi bi-thermometer-high"></i> Climate', content_update)

content[navbar] <- content_update

writeLines(content, file)