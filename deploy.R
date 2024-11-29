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
cat("Preparing image database ======\n")
fs::dir_create("images/gallery")
source("R/create_images_database.R")

# Update krona plots
source("R/update_krona_plots.R")

# Download front gallery images
system("aws s3 sync --no-sign-request s3://edna-dashboard/images/front_gallery images/front_gallery")
