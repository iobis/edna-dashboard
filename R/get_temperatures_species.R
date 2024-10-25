# Prepare thermal limits for species

library(arrow)
library(dplyr)
library(biooracler)
library(obissdm)
library(h3jsr)
library(terra)
library(sfarrow)

tempfolder <- "climate_temp"
fs::dir_create(tempfolder)
outfolder <- "data"

gridf <- file.path(tempfolder, 'speciesgrids')
fs::dir_create(gridf)
system(glue::glue("aws s3 cp --recursive s3://obis-products/speciesgrids/h3_7 {gridf} --no-sign-request"))

# Download environmental data
datasets <- c(
  "thetao_baseline_2000_2019_depthsurf"
)

datasets <- c(datasets,
              gsub("depthsurf", "depthmean", datasets))

future_scenarios <- c("ssp126", "ssp245", "ssp370")

time_steps <- list(
  current = c("2000-01-01T00:00:00Z", "2010-01-01T00:00:00Z"), #2000-2010/2010-2020
  dec50 = c("2030-01-01", "2040-01-01"), #2030-2040/2040-2050
  dec100 = c("2080-01-01", "2090-01-01") #2080-2090/2090-2100
)

variables <- c("mean")

get_env_data(datasets = datasets, future_scenarios = future_scenarios,
             time_steps = time_steps, variables = variables,
             terrain_vars = NULL, average_time = T, outdir = tempfolder)


# Load environmental layers
fpaths <- file.path(tempfolder, paste0(
    "future/", future_scenarios, "/thetao_", future_scenarios, "_depthsurf_mean.tif"
))
all_paths <- c(
    file.path(tempfolder, "current/thetao_baseline_depthsurf_mean.tif"),
    gsub("mean", "dec50_mean", fpaths),
    gsub("mean", "dec100_mean", fpaths)
)
surface <- rast(all_paths)
bottom <- rast(gsub("depthsurf", "depthmean", all_paths))

names(surface) <- gsub("thetao_", "", gsub(".tif", "", basename(sources(surface))))
names(bottom) <- gsub("thetao_", "", gsub(".tif", "", basename(sources(bottom))))

# Get list of species
source("R/occurrence.R")

occ <- read_occurrence_data()

occ <- occ %>%
    filter(taxonRank == "species") %>%
    select(scientificName, higherGeography) %>%
    group_by(scientificName, higherGeography) %>%
    distinct(scientificName, .keep_all = T) %>%
    ungroup()

species_list <- occ %>% distinct(scientificName)

gridp <- open_dataset(gridf)

get_nearby <- function(cells, layer, mode = "25") {

    if (mode == "queen") {
      adj_m <- "queen"
    } else {
      adj_m <- matrix(c(rep(1, 12), 0, rep(1, 12)), 5, 5)
    }

    adj <- adjacent(layer, cells = cells, adj_m)

    bcell <- lapply(1:nrow(adj), function(x) {
        e <- terra::extract(layer, adj[x,])
        e <- e[,1]
        if (any(!is.na(e))) {
            if (sum(!is.na(e)) > 1) {
                dists <- xyFromCell(layer, adj[x,][!is.na(e)])
                main_point <- xyFromCell(layer, cells[x])
                ne <- nearest(vect(main_point, crs = "EPSG:4326"), vect(dists, crs = "EPSG:4326"))
                tr <- adj[x,][!is.na(e)][ne$to_id]
            } else {
                tr <- adj[x,][!is.na(e)]
            }
        } else {
            tr <- NA
        }
        return(tr)
    })
}

quantile_df <- function(x, probs = c(0.05, 0.95)) {
  res <- tibble(metric = c(paste0("q_", probs), "mean", "sd", "no_data", "with_data"), 
                value = c(quantile(x, probs, na.rm = T),
                          mean(x, na.rm = T),
                          sd(x, na.rm = T),
                          sum(is.na(x)),
                          sum(!is.na(x))))
  res
}

species_tables <- lapply(seq_along(species_list$scientificName), function(x) NULL)

for (i in seq_along(species_list$scientificName)) {

    cat(i, "out of", nrow(species_list), "\n")

    species_data <- gridp %>%
        select(species, AphiaID, geometry) %>%
        filter(species == species_list$scientificName[i])

    species_data <- sfarrow::read_sf_dataset(species_data)

    vc <- terra::extract(surface[[1]], species_data, cell = T)

    na_cells <- which(is.na(vc[,2]))

    if (length(na_cells) > 0) {
        valid <- get_nearby(vc$cell[na_cells], surface[[1]])
        valid <- unlist(valid)

        vc$cell[na_cells] <- valid

        vc <- vc$cell[!is.na(vc$cell)]
    }

    s_vals <- terra::extract(surface, vc)
    b_vals <- terra::extract(bottom, vc)

    a_vals <- cbind(s_vals, b_vals)

    a_vals <- tidyr::pivot_longer(a_vals, names_to = "layer", values_to = "sst", cols = 1:ncol(a_vals))

    a_vals <- a_vals %>%
        group_by(layer) %>%
        summarise(mean = mean(sst, na.rm = T), upper_lim = quantile(sst, probs = 0.95, na.rm = T))
    a_vals <- a_vals %>%
        mutate(layer = ifelse(grepl("baseline", layer), gsub("_mean", "_current_mean", layer), layer)) %>%
        tidyr::separate(
            layer,
            into = c("scenario", "depth", "period", "variant"),
            sep = "_",
            fill = "right"
        )
    a_vals$species <- species_list$scientificName[i]

    species_tables[[i]] <- a_vals

}





# Load one first raster to get indices vs h3
if (!file.exists(file.path(tempfolder, "indices.rds"))) {
    r <- rast(file.path(tempfolder, "current/thetao_baseline_depthmean_mean.tif"))
    r <- as.data.frame(r, xy = T, cell = T)

    batches <- split(seq_len(nrow(r)), ceiling(seq_len(nrow(r)) / 10000))

    results <- lapply(seq_along(batches), function(x) NULL)

    for (i in seq_along(batches)) {
        cat("Batch", i, "out of", length(batches))
        results[[i]] <- h3jsr::point_to_cell(r[batches[[i]], c("x", "y")], res = 7)
    }

    id_df <- data.frame(cell = r$cell, h3 = unlist(results))
    saveRDS(id_df, file.path(tempfolder, "indices.rds"))

    rm(results, batches, r)
} else {
    id_df <- readRDS(file.path(tempfolder, "indices.rds"))
}

# Get list of species
source("R/occurrence.R")

occ <- read_occurrence_data()

occ <- occ %>%
    filter(taxonRank == "species") %>%
    select(scientificName, higherGeography) %>%
    group_by(scientificName, higherGeography) %>%
    distinct(scientificName, .keep_all = T) %>%
    ungroup()

species <- occ %>% distinct(scientificName)

con <- dbConnect(duckdb())
colnames(id_df)[1] <- "rcell"
duckdb_register(con, "raster", id_df)

species_data <- dbGetQuery(con, glue::glue("
  select species, h3, rcell
  from read_parquet('{gridf}/*') grid
  left join raster on grid.cell = raster.h3
  where species == '{species$scientificName[i]}'
  group by species, h3, rcell
"))

for (i in seq_along(species)) {

}