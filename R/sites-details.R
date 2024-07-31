# Get additional information from WHC sites

get_sites_details <- function(outfile = "data/sites_description.txt", force = FALSE) {
  
  if (force) {
    download <- TRUE
  } else if (file.exists(outfile)) {
    mod <- as.Date(file.info(outfile)$mtime)
    if (mod == Sys.Date()) download <- FALSE else download <- TRUE
  } else {
    download <- TRUE
  }
  
  if (download) {
    cat("Downloading sites descriptions")
    
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
    cat("Skipping download of site descriptions - file already up-to-date.")
  }
  return(invisible(NULL))
}