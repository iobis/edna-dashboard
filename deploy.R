# Deploy the dashboard on the server
force <- FALSE

# Download occurrence data
if (!dir.exists("data/output") || force) {
    cat("Downloading occurrence data ======\n")
    system("rm -r output output.zip")
    download.file(
        "https://obis-edna-results.s3.amazonaws.com/output.zip",
        "output.zip"
    )
    unzip("output.zip", exdir = "data/")
    file.remove("output.zip")
} else {
    cat("Occurrence data already downloaded ======\n")
}

# Prepare images database
if (!dir.exists("images/gallery") || force) {
    cat("Preparing image database ======\n")
    source("R/create_images_database.R")
} else {
    cat("Image database done ======\n")
}
