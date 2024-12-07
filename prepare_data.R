# Prepare data that will be used on dashboard

source("R/site_details.R")
source("R/occurrence.R")
source("R/update_krona_plots.R")
source("R/images.R")
source("R/get_temperatures_climate.R")
source("R/get_temperatures_species.R")
source("R/get_fishbase_info.R")
source("R/get_taxonomy.R")

download_site_descriptions(force = FALSE)
download_occurrence_data(force = TRUE)
verify_occurrence_parquet()
calculate_site_stats(force = TRUE)
calculate_sample_stats(force = TRUE)
generate_image_list(force = TRUE)
create_images_database()
update_krona_plots()
download_gallery_images()
get_temperatures_climate(force = FALSE)
get_temperature_species(force = TRUE)
get_fishbase_info(force = TRUE)
taxonomy_by_site()
diversity_by_site()
diversity_metrics_by_site()
