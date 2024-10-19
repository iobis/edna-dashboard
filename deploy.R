# Deploy the dashboard on the server

# Download occurrence data
cat("Downloading occurrence data ======\n")
system("rm -r output output.zip")
download.file("https://obis-edna-results.s3.amazonaws.com/output.zip",
              "output.zip")
unzip("output.zip", exdir = "data/")
file.remove("output.zip")

# Prepare images database
cat("Preparing image database ======\n")
source("R/create_images_database.R")