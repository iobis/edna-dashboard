cat("Adjusting navbar")

file <- "index.html"

content <- readLines(file, warn = F)

navbar <- which(grepl("nav-item", content))

content_to_change <- content[navbar]

content_update <- gsub("Home", '<i class="bi bi-house"></i> Home', content_to_change)

content_update <- gsub("Taxonomy", '<i class="bi bi-bar-chart-steps"></i> Taxonomy', content_update)
content_update <- gsub("Diversity", '<i class="bi bi-boxes"></i> Diversity', content_update)
content_update <- gsub("Species", '<i class="bi bi-bug"></i> Species', content_update)
content_update <- gsub("Sample", '<i class="material-symbols-outlined", style="font-size: 1.4em; 
    vertical-align: text-top;">labs</i> Sample', content_update)
content_update <- gsub("DNA", '<i class="bi bi-eyedropper"></i> DNA', content_update)
content_update <- gsub("Climate", '<i class="bi bi-thermometer-high"></i> Climate', content_update)

content_update <- gsub("Statistics", '<i class="bi bi-graph-up"></i> Statistics', content_update)
content_update <- gsub("Pictures", '<i class="bi bi-camera-fill"></i> Pictures', content_update)
content_update <- gsub("Data", '<i class="bi bi-database-fill-down"></i> Data', content_update)
#content_update <- gsub('href=""', 'href="javascript:void(0)"', content_update)

content[navbar] <- content_update

writeLines(content, file)
