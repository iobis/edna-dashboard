# Get temperatures at sites
if (!file.exists("data/climate_historical.txt")) {
  library(reticulate)
  library(dplyr)
  reticulate::source_python("py/download_glorys_py.py")
  cm <- import("copernicusmarine")

  if (Sys.getenv("COPERNICUS_USER") != "") {
    .user <- Sys.getenv("COPERNICUS_USER")
    .pwd <- Sys.getenv("COPERNICUS_PWD")
  } else {
    .user <- readline("Enter your user")
    .pwd <- readline("Enter your password")
  }

  sites <- jsonlite::read_json("https://raw.githubusercontent.com/iobis/edna-tracker-data/data/generated.json")
  sites_list <- sites$sites
  sites_samples <- sites$samples

  sites_names <- sapply(sites_list, function(x) x$name)

  localities <- sites_samples %>%
    bind_rows() %>%
    select(area_name, area_locality, parent_area_name, station, lon = area_longitude, lat = area_latitude) %>%
    group_by(area_name, area_locality, parent_area_name, station, lon, lat) %>%
    distinct(.keep_all = T)

  localities$temp_ID <- 1:nrow(localities)

  localities_wv <- localities %>%
    ungroup() %>%
    select(decimalLongitude = lon, decimalLatitude = lat, temp_ID) %>%
    mutate(depth_surface = 0)

  localities_wv <- localities_wv[!is.na(localities_wv$decimalLongitude), ]

  years <- 1993:2024
  months <- 1:12

  results <- list()

  for (y in 1:length(years)) {
    sel_year <- years[y]
    cat("Starting year", sel_year, "\n")
    for (m in 1:length(months)) {
      sel_month <- months[m]
      cat("Processing", sel_month, "\n")

      sel_date <- paste0(sel_year, "-", sprintf("%02d", sel_month), "-01")

      if (as.Date(sel_date) >=
        as.Date(paste0(lubridate::year(Sys.Date()), "-", sprintf("%02d", lubridate::month(Sys.Date())), "-01"))) {
        break
      } else {
        if (as.Date(sel_date) <= as.POSIXct("2021-06-01 02:00:00 CEST")) {
          dscode <- "cmems_mod_glo_phy_my_0.083deg_P1M-m"
        } else {
          dscode <- "cmems_mod_glo_phy_myint_0.083deg_P1M-m"
        }

        dataset <- cm$open_dataset(
          dataset_id = dscode,
          # variables = variables,
          username = .user,
          password = .pwd
        )

        part_result <- download_glorys_py(dataset, localities_wv, sel_date)

        results <- c(results, list(part_result))
      }
    }
  }

  results_b <- dplyr::bind_rows(results)

  results_b <- results_b %>%
    select(temp_ID, depth_type, value, requested_date) %>%
    tidyr::pivot_wider(names_from = depth_type, values_from = c(value))

  results_b$year <- lubridate::year(results_b$requested_date)
  results_b$month <- lubridate::month(results_b$requested_date)
  results_b <- results_b %>%
    select(-requested_date)

  results_final <- left_join(results_b, localities)

  write.table(results_final, "data/climate_historical.txt", row.names = F)

}
