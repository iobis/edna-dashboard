#################### eDNA expeditions - scientific analysis ####################
########################## Environmental data download #########################
# November of 2024
# Author: Silas C. Principe
# Contact: s.principe@unesco.org
#
############### Retrieve SST summaries for species and sites ###################

# Dependencies note
# Part of this code can be executed with Python, using Xarray and Dask
# You need to install those libraries
# using reticulate::py_install()
# To use that option, you can source the code "nearby_from_nc.R"
# Here we use the R terra package instead. The functions syntax are exactly the same


# Load packages -----
library(arrow)
library(dplyr)
library(tidyr)
library(biooracler)
library(obissdm)
library(h3jsr)
library(terra)
library(ggplot2)
library(patchwork)



# Settings -----
datafolder <- "climate_temp"
fs::dir_create(datafolder)
outfolder <- "results"



# Download data ------
# Species grids
gridf <- file.path(datafolder, 'speciesgrids')
fs::dir_create(gridf)
system(glue::glue("aws s3 cp --recursive s3://obis-products/speciesgrids/h3_7 {gridf} --no-sign-request"))

# Environmental data
datasets <- c(
  "thetao_baseline_2000_2019_depthsurf"
)

datasets <- c(datasets,
              gsub("depthsurf", "depthmean", datasets))

future_scenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

time_steps <- list(
  current = c("2000-01-01T00:00:00Z", "2010-01-01T00:00:00Z"), #2000-2010/2010-2020
  dec50 = c("2030-01-01", "2040-01-01"), #2030-2040/2040-2050
  dec100 = c("2080-01-01", "2090-01-01") #2080-2090/2090-2100
)

variables <- c("mean")

get_env_data(datasets = datasets, future_scenarios = future_scenarios,
             time_steps = time_steps, variables = variables,
             terrain_vars = NULL, average_time = T, outdir = datafolder)



# Prepare environmental layers -------
fpaths <- file.path(datafolder, paste0(
    "future/", future_scenarios, "/thetao_", future_scenarios, "_depthsurf_mean.tif"
))
all_paths <- c(
    file.path(datafolder, "current/thetao_baseline_depthsurf_mean.tif"),
    gsub("mean", "dec50_mean", fpaths),
    gsub("mean", "dec100_mean", fpaths)
)
surface <- rast(all_paths)
bottom <- rast(gsub("depthsurf", "depthmean", all_paths))

names(surface) <- gsub("thetao_", "", gsub(".tif", "", basename(sources(surface))))
names(bottom) <- gsub("thetao_", "", gsub(".tif", "", basename(sources(bottom))))

# Save as a single netcdf, as this will be used by the R or Python functions
writeCDF(sds(surface[[1]], bottom[[1]]), file.path(datafolder, "biooracle_data.nc"),
         overwrite = T)


# Get list of species ------
source("R/occurrence.R")

occ <- read_occurrence_data()

occ <- occ %>%
    filter(taxonRank == "species") %>%
    select(scientificName, higherGeography) %>%
    group_by(scientificName, higherGeography) %>%
    distinct(scientificName, .keep_all = T) %>%
    ungroup()

species_list <- occ %>% distinct(scientificName)



# Extract data and get nearby for those invalid -------
source("R/nearby_from_nc_terra.R")

# Uncomment and use lines below to use Python version
# library(reticulate)
# source("codes/nearby_from_nc.R")
# da <- import("dask")
# dd <- import("dask.distributed")
# client <- dd$Client()
# browseURL("http://localhost:8787/status")
# xr <- import("xarray")

target_nc <- file.path(datafolder, "biooracle_data.nc")

quantile_df <- function(x, probs = c(0.05, 0.95)) {
  res <- tibble(metric = c(paste0("q_", probs), "mean", "sd", "no_data", "with_data"), 
                value = c(quantile(x, probs, na.rm = T),
                          mean(x, na.rm = T),
                          sd(x, na.rm = T),
                          sum(is.na(x)),
                          sum(!is.na(x))))
  res
}

grids_ds <- open_dataset(gridf)

grids_ds <- grids_ds %>%
    select(species, AphiaID, min_year, cell)

number_recs <- grids_ds %>%
    filter(species %in% species_list$scientificName) %>%
    group_by(species) %>%
    count() %>%
    collect()

number_recs <- number_recs %>%
    ungroup() %>%
    mutate(cumulative_sum = cumsum(n),
         batch = cumsum(cumulative_sum >= lag(cumulative_sum, default = 0) + 100000)) %>%
    select(-cumulative_sum)

number_recs$batch <- number_recs$batch + 1

species_batch <- unique(number_recs$batch)
results_batch <- lapply(seq_along(species_batch), function(x) NULL)

for (spi in species_batch) {
    cat("\nRunning batch", spi, "out of", length(species_batch), "\n\n")
    sel_species <- number_recs$species[number_recs$batch == spi]

    sel_data <- grids_ds %>% 
        filter(species %in% sel_species) %>%
        collect()

    sel_data_crds <- as.data.frame(sf::st_coordinates(h3jsr::cell_to_point(sel_data$cell)))
    sel_data_crds <- bind_cols(sel_data_crds, sel_data)
    colnames(sel_data_crds)[1:2] <- c("decimalLongitude", "decimalLatitude")

    grids_result <- extract_from_nc(target_nc, sel_data_crds[,1:2])
    var_col <- which(grepl("thetao", colnames(grids_result)))[1]

    grids_result <- bind_cols(grids_result, sel_data)

    coords_na <- which(is.na(grids_result[, var_col]))

    if (length(coords_na) > 1) {
        new_coords <- get_nearby(target_nc, names(grids_result)[var_col],
                                grids_result[coords_na, 1:2], mode = "25")
        na_approx <- which(!is.na(new_coords[, "value"]))
        new_coords$decimalLongitude[na_approx] <- new_coords$new_lon[na_approx]
        new_coords$decimalLatitude[na_approx] <- new_coords$new_lat[na_approx]

        new_info <- grids_result[coords_na, colnames(sel_data)]
        grids_result <- grids_result[-coords_na,]
        new_result <- extract_from_nc(target_nc, new_coords[,1:2])
        grids_result <- bind_rows(grids_result, bind_cols(new_result, new_info))
    }

    grids_result <- grids_result %>%
        select(cell, species, starts_with("thetao")) %>%
        pivot_longer(starts_with("thetao"), names_to = "variable", values_to = "values") %>%
        group_by(variable, species) %>%
        distinct(cell, .keep_all = T) %>%
        reframe(quantile_df(values)) %>%
        separate_wider_delim(cols = "variable",
                            names = c("variable", "baseline", "depth", "variant"),
                            delim = "_") %>%
        select(-variable, -variant, scenario = baseline)

    results_batch[[spi]] <- grids_result
}

results_batch <- do.call("rbind", results_batch)

write_parquet(results_batch, file.path(datafolder, "species_thermal_lims.parquet"))



# Get temperature on sites on all scenarios ------
# sites <- jsonlite::read_json("https://raw.githubusercontent.com/iobis/edna-tracker-data/data/generated.json")
# sites_samples <- sites$samples %>% bind_rows()
sites_shape <- sf::read_sf("https://samples.ednaexpeditions.org/sites.geojson")
sites_shape <- terra::vect(sites_shape)

sites_values <- terra::extract(c(surface, bottom), sites_shape, fun = mean, na.rm = T)
sites_values$higherGeography <- sites_shape$name
sites_values <- sites_values %>% filter(!is.na(baseline_depthsurf_mean))

sites_values$higherGeography[sites_values$higherGeography == "Península Valdés"] <- "Peninsula Valdès"
sites_values$higherGeography[sites_values$higherGeography == "The Wadden Sea"] <- "Wadden Sea"

write_parquet(sites_values, file.path(datafolder, "sites_sst_all.parquet"))

species_site_temp <- left_join(occ, sites_values) %>% select(-ID)

species_lims <- results_batch %>% 
    select(species, metric, value, scenario, depth) %>%
    pivot_wider(names_from = metric, values_from = value, id_cols = c("species", "scenario", "depth"))

species_lims_dsurf <- species_lims %>%
    filter(depth == "depthsurf") %>%
    rename(scientificName = species)
species_lims_dmean <- species_lims %>%
    filter(depth == "depthmean") %>%
    rename(scientificName = species)

colnames(species_lims_dsurf)[4:9] <- paste0("dsurf_", colnames(species_lims_dsurf)[4:9])
colnames(species_lims_dmean)[4:9] <- paste0("dmean_", colnames(species_lims_dmean)[4:9])

species_site_temp <- left_join(species_site_temp, species_lims_dsurf[,c("scientificName", "dsurf_q_0.95")])
species_site_temp <- left_join(species_site_temp, species_lims_dmean[,c("scientificName", "dmean_q_0.95")])

sites_species_risk <- species_site_temp %>%
    mutate(across(contains("depthsurf"), ~ .x - dsurf_q_0.95)) %>%
    mutate(across(contains("depthmean"), ~ .x - dmean_q_0.95)) %>%
    rename(limit_dsurf = dsurf_q_0.95, limit_dmean = dmean_q_0.95) %>%
    mutate(across(3:length(.), ~round(.x, 2)))

# View results
sites_species_risk %>%
    filter(higherGeography == 'Aldabra Atoll') %>%
    select(scientificName, 
     current = baseline_depthsurf_mean,
     ssp1 = ssp126_depthsurf_dec100_mean,
     ssp2 = ssp245_depthsurf_dec100_mean,
     ssp3 = ssp370_depthsurf_dec100_mean,
     ssp5 = ssp585_depthsurf_dec100_mean)

write_parquet(sites_species_risk, file.path(datafolder, "species_sites_risk.parquet"))

# Add IUCN and groups information
groups <- read.csv("data/supporting_data/groups.csv")
group_lookup <- setNames(groups$group, paste(groups$rank, groups$taxon, sep = "_"))

# Add a new column for group
occ <- read_occurrence_data()

occ <- occ %>%
    filter(taxonRank == "species") %>%
    distinct(scientificName, .keep_all = T) %>%
    ungroup()

occurrence <- occ %>%
    mutate(group = coalesce(
        group_lookup[paste("class", class, sep = "_")],
        group_lookup[paste("order", order, sep = "_")],
        group_lookup[paste("phylum", phylum, sep = "_")]
    ))
  

redlist <- read.csv("data/supporting_data/redlist.csv")
colnames(redlist)[1] <- "scientificName"

occurrence <- occurrence %>% left_join(redlist)
occurrence <- occurrence %>%
    select(scientificName, group, category)

sites_species_risk_f <- left_join(sites_species_risk, occurrence)

# Get ecological information
fishbase <- rfishbase::species(species_list$scientificName)
sealifebase <- rfishbase::species(species_list$scientificName, server  = "sealifebase")

fishbase <- fishbase %>%
    select(scientificName = Species, max_depth = DepthRangeDeep, type = DemersPelag)

sealifebase <- sealifebase %>%
    select(scientificName = Species, max_depth = DepthRangeDeep, type = DemersPelag)

funcinfo <- bind_rows(fishbase, sealifebase) %>% distinct(scientificName, .keep_all = T)

funcinfo$max_depth[is.na(funcinfo$max_depth)] <- ifelse(
    funcinfo$type[is.na(funcinfo$max_depth)] %in% c("reef-associated", "pelagic-neritic"),
    0, 300
)

write_parquet(funcinfo, file.path(datafolder, "functional_info.parquet"))

funcinfo$habitat <- ifelse(
    funcinfo$max_depth <= 200, "Shallow", "Not shallow"
)

sites_species_risk_f <- left_join(sites_species_risk_f, funcinfo[,c("scientificName", "habitat")])
sites_species_risk_f$habitat[is.na(sites_species_risk_f$habitat)] <- "Not available"

shallow_sp <- sites_species_risk_f %>%
    filter(habitat %in% c("Shallow", "Not available")) %>%
    select(scientificName, higherGeography, contains("depthsurf"), sp_limit = limit_dsurf,
           group, category, habitat)
colnames(shallow_sp) <- gsub("_mean", "", gsub("_depthsurf", "", colnames(shallow_sp)))

deep_sp <- sites_species_risk_f %>%
    filter(!habitat %in% c("Shallow", "Not available")) %>%
    select(scientificName, higherGeography, contains("depthmean"), sp_limit = limit_dmean,
           group, category, habitat)
colnames(deep_sp) <- gsub("_mean", "", gsub("_depthmean", "", colnames(deep_sp)))

sites_species_risk_f <- bind_rows(shallow_sp, deep_sp)

# Save final file to be used in the app
write_parquet(sites_species_risk_f, file.path("data", "species_sites_risk_full.parquet"))

# Example application
sites_species_risk %>%
    filter(higherGeography == 'Aldabra Atoll') %>%
    select(scientificName, 
     current = baseline_depthsurf_mean,
     ssp1 = ssp126_depthsurf_dec100_mean,
     ssp2 = ssp245_depthsurf_dec100_mean,
     ssp3 = ssp370_depthsurf_dec100_mean) %>%
     mutate(across(2:length(.), ~ ifelse(.x <= 0, 0, 1))) %>%
     summarise(current = sum(current, na.rm = T), ssp1 = sum(ssp1, na.rm = T), ssp2 = sum(ssp2, na.rm = T), ssp3 = sum(ssp3, na.rm = T)) %>%
     pivot_longer(1:4, names_to = "scenario", values_to = "species") %>%
     ggplot() +
        geom_bar(aes(x = scenario, y = species), stat = "identity")

# Save an example of how the metrics work
example_sp <- grids_ds %>%
    select(species, AphiaID, min_year, cell) %>%
    filter(species == "Trachinotus baillonii") %>%
    collect()

example_sp <- terra::vect(h3jsr::cell_to_point(example_sp$cell))

example_data <- terra::extract(surface[[1]], example_sp, ID = FALSE)
example_data <- example_data[!is.na(example_data$baseline_depthsurf_mean),]
example_data <- cbind(example_data, y = 1)
colnames(example_data)[1] <- "SST"
example_data <- as.data.frame(example_data)

wrld <- rnaturalearth::ne_countries(returnclass = "sf")

sst <- terra::aggregate(surface[[1]], 10)
sst <- as.data.frame(sst, xy = T)
colnames(sst)[3] <- "SST"
#sst <- sf::st_as_sf(terra::vect(sst, geom = c("x", "y"), crs = "EPSG:4326"))

p1 <- ggplot() +
    geom_sf(data = wrld, fill = "grey90", color = "grey90") +
    geom_raster(data = sst, aes(x = x, y = y, fill = SST)) +
    scale_fill_distiller(palette = 4) +
    geom_sf(data = sf::st_as_sf(example_sp), color = "#003fbe") +
    ggtitle("Data are extracted from occurrence records",
    "Occurrence data comes from OBIS and GBIF, and SST from Bio-ORACLE") +
    theme_void() +
    theme(legend.position = "none")

v95 <- quantile(example_data$SST, .95)

example_data$status <- ifelse(example_data$SST < v95, "Within limit", "Out of limit")

p2 <- ggplot(example_data) +
    geom_jitter(aes(x = SST, y = 0.5, color = status), height = 0.1) +
    scale_color_manual(values = c("#ff8800", "#0045bc")) +
    geom_density(aes(x = SST), fill = "grey80", color = "grey80") +
    geom_text(label = "95% percent of values are here", x = 20, y = 0.35, color = "#0045bc") +
    geom_text(label = "Thermal limit", x = 29, y = 0.2, color = "#ff8800", angle = 90) +
    geom_vline(xintercept = v95, color = "#ff8800", linewidth = 2) +
    theme_light() +
    ggtitle("Values are then used to calculate the thermal limit",
    "We consider the thermal limit of the species the 95% quantile") +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
    )


example_data <- terra::extract(surface[[c(1,9)]], example_sp, ID = FALSE)
example_data <- cbind(example_data, geom(example_sp)[,c("x", "y")])
example_data <- example_data[!is.na(example_data$baseline_depthsurf_mean),]
colnames(example_data)[1:2] <- c("Current period", "SSP5 (2100)")
example_data <- as.data.frame(example_data)

example_data$`Current period` <- ifelse(
    example_data$`Current period` <= v95,
    "Normal conditions", "Thermal risk"
)

example_data$`SSP5 (2100)` <- ifelse(
    example_data$`SSP5 (2100)` <= v95,
    "Normal conditions", "Thermal risk"
)
example_data <- example_data %>%
    pivot_longer(1:2, names_to = "Scenario", values_to = "Status")

example_data <- vect(example_data, geom = c("x", "y"), crs = "EPSG:4326")

p3 <- ggplot() +
    geom_sf(data = wrld, fill = "grey90", color = "grey90") +
    geom_sf(data = sf::st_as_sf(example_data), aes(color = Status)) +
    scale_color_manual(values = rev(c("#ff8800", "#0045bc"))) +
    ggtitle("We then extract the temperature in futures scenarios\n to assess thermal risk",
    "Records with temperatures higher than the thermal limit\n are considered to be at risk") +
    theme_light() +
    theme(legend.position = "bottom") +
    facet_wrap(~ Scenario, ncol = 1)

design <- "A
           A
           B
           C
           C"

p1/(p2+ theme(plot.margin = unit(c(5,0,5,0), "pt")))/p3 +
    plot_layout(design = design)

ggsave("images/climate_data_example.jpg",
quality = 90, width = 5.5, height = 13)
