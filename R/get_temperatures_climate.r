# Get temperatures at sites
if (!file.exists("data/climate_historical.txt")) {
  suppressPackageStartupMessages(library(reticulate))
  suppressPackageStartupMessages(library(dplyr))
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
  sites_samples <- sites$samples %>% bind_rows()

  sites_names <- sapply(sites_list, function(x) x$name)
  sites_names <- sites_names[sites_names != "Sanganeb Marine National Park and Dungonab Bay – Mukkawar Island Marine National Park"]
  sites_names <- sites_names[order(sites_names)]

  localities <- sites_samples %>% 
    filter(!blank) %>%
    select(area_name, area_locality, parent_area_name, station, lon = area_longitude, lat = area_latitude) %>%
    group_by(area_name, area_locality, parent_area_name, station, lon, lat) %>%
    distinct(.keep_all = T)

  localities$temp_ID <- 1:nrow(localities)

  localities_wv <- localities %>%
    ungroup() %>%
    select(decimalLongitude = lon, decimalLatitude = lat, temp_ID) %>%
    mutate(depth_surface = 0)

  localities_wv <- localities_wv[!is.na(localities_wv$decimalLongitude), ]

  # See if any are on land
  outf_temp_glorys <- cm$get(
          dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
          username = .user,
          password = .pwd,
          filter = paste0("*", 1993, sprintf("%02d", 01), "*"),
          output_directory = "",
          no_directories = T,
          force_download = T
  )
  outf_temp_glorys <- as.character(outf_temp_glorys[[1]])

  to_check <- terra::rast(outf_temp_glorys)
  to_check <- to_check$`thetao_depth=0.49402499`
  names(to_check) <- "thetao"

  loc_to_check <- terra::extract(to_check$thetao, localities_wv[,1:2])
  colnames(loc_to_check)[2] <- "value"
  
  na_locs <- localities_wv[is.na(loc_to_check$value),1:2]

  for (i in 1:nrow(na_locs)) {
    mm <- matrix(nrow = 5, ncol = 5)
    mm[] <- c(rep(1, 12), 0, rep(1, 12))
    nc <- terra::adjacent(to_check, terra::cellFromXY(to_check, as.data.frame(na_locs[i,1:2])), directions = mm)
    ncv <- terra::extract(to_check, as.vector(nc))
    if (any(!is.na(ncv$thetao))) {
      if (sum(!is.na(ncv$thetao)) == 1) {
        na_locs[i,1:2] <- terra::xyFromCell(to_check, 
                                            as.vector(nc)[!is.na(ncv$thetao)][1])
      } else {
        validp <- as.vector(nc)[!is.na(ncv$thetao)]
        validp <- terra::xyFromCell(to_check, validp)
        validp <- terra::vect(validp, crs = "EPSG:4326")
        ne <- terra::nearest(terra::vect(na_locs[i,1:2], crs = "EPSG:4326", geom = c("decimalLongitude", "decimalLatitude")), validp)
        na_locs[i,1:2] <- data.frame(x = ne$to_x[1], y = ne$to_y[1])
      }
    }
  }

  rm(to_check)
  fs::file_delete(outf_temp_glorys)

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
    select(temp_ID, depth_type, value, time, longitude, latitude) %>%
    group_by(temp_ID, depth_type, time, longitude, latitude) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = depth_type, values_from = value)

  results_b$year <- lubridate::year(results_b$time)
  results_b$month <- lubridate::month(results_b$time)
  results_b <- results_b %>%
    select(-time)

  results_final <- left_join(results_b, localities)

  write.table(results_final, "data/climate_historical.txt", row.names = F)

}
