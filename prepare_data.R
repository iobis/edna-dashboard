# Prepare data that will be used on dashboard

source("R/site_details.R")
source("R/occurrence.R")
source("R/update_krona_plots.R")
source("R/images.R")
source("R/get_temperatures_climate.R")
source("R/get_fishbase_info.R")

download_site_descriptions(force = TRUE)
download_occurrence_data(force = TRUE)
calculate_site_stats(force = TRUE)
generate_image_list(force = FALSE)
create_images_database()
update_krona_plots()
download_gallery_images()
get_temperatures_climate(force = FALSE)
get_fishbase_info(force = FALSE)

# TODO: get_temperature_species still needs to be run manually
