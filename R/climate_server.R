climate_data <- readRDS("data/climate.rds")

climate_vals <- reactiveValues()

observe({
    climate_filt <- climate_species %>%
        select(
            Species = scientificName, higherGeography, Depth = habitat, `Upper limit` = sp_limit,
            Current = baseline, SSP1 = ssp126_dec100,
            SSP2 = ssp245_dec100, SSP3 = ssp370_dec100, group, category
        ) %>%
        filter(higherGeography == input$higherGeography) %>%
        collect() %>% ungroup() %>%
        filter(!is.na(Current))

    if (input$groupClimate != "all species") {
        climate_filt <- climate_filt %>%
            filter(group == input$groupClimate)
    }
    if (input$iucnClimate != "all species") {
        climate_filt <- climate_filt %>%
            filter(category == input$iucnClimate)
    }

    climate_vals$main <- climate_filt %>%
        select(-group, -category)

    n_max <- function(x) {
        sum(x > 0, na.rm = T)
    }

    climate_status <- climate_filt %>%
        summarise(across(5:8, n_max)) %>%
        tidyr::pivot_longer(1:4, names_to = "Scenario", values_to = "Number of species")

    climate_vals$status <- list(data = climate_status, total = length(unique(climate_filt$Species)))

    # sites_temperatures <- climate_data %>%
    #     select(
    #         Species = species, higherGeography,
    #         Current = site_current, SSP1 = site_ssp126_dec100,
    #         SSP2 = site_ssp245_dec100, SSP3 = site_ssp370_dec100
    #     ) %>%
    #     filter(grepl(tolower(substr(input$higherGeography, 1, 3)), higherGeography)) %>%
    #     slice_head(n = 1) %>%
    #     ungroup() %>% select(-Species, -higherGeography) %>%
    #     tidyr::pivot_longer(1:4, names_to = "Scenario", values_to = "Temperature")

    # climate_vals$temperatures <- sites_temperatures

})

output$climate_thermal_risk <- reactable::renderReactable({
    dat <- climate_vals$main
    dat[, c("Current", "SSP1", "SSP2", "SSP3")] <- round(dat[, c("Current", "SSP1", "SSP2", "SSP3")], 2)

    colfun <- function(value) {
        if (value < 0) {
            color <- "#7CD59B"
        } else if (value > 0) {
            color <- "#E8AEAE"
        } else {
            color <- "#777"
        }
        list(background = color, fontWeight = "bold")
    }

    dat$Depth <- ifelse(dat$Depth == "Shallow", "S",
        ifelse(dat$Depth == "Not shallow", "NS", "NA"))

    colfun2 <- function(value) {
        if (value == "S") {
            color <- "#1866b3"
        } else if (value == "NS") {
            color <- "#b69354"
        } else {
            color <- "#868686"
        }
        list(color = color, fontWeight = "bold")
    }

    reactable(dat, columns = list(
        Depth = colDef(style = colfun2, width = 60),
        SSP1 = colDef(style = colfun),
        SSP2 = colDef(style = colfun),
        SSP3 = colDef(style = colfun),
        Current = colDef(style = colfun)
    ))
})

# require(ggplot2)
# ggplot(climate_vals$status) +
#     geom_bar(aes(x = Scenario, fill = Scenario, y = `Number of species`), stat = "identity") +
#     scale_fill_manual(values = c("#11b1aa", "#3f45c7", "#ee8114", "#db3c81")) +
#     theme_light() + theme(legend.position = "none")

# ndat <- climate_vals$temperatures %>%
#     mutate(`Temperature Increase` = Temperature - climate_vals$temperatures$Temperature[1])
# ndat[2:4,] %>%
# ggplot() +
#     geom_bar(aes(x = Scenario, fill = Scenario, y = `Temperature Increase`), stat = "identity") +
#     scale_fill_manual(values = c("#11b1aa", "#067570", "#07413e")) +
#     theme_light() + theme(legend.position = "none")

output$climate_number_species <- shiny::renderPlot({
    require(ggplot2)
ggplot(climate_vals$status$data) +
    geom_bar(aes(x = Scenario, fill = Scenario, y = `Number of species`), stat = "identity") +
    scale_y_continuous(limits = c(0, climate_vals$status$total)) +
    scale_fill_manual(values = c("#11b1aa", "#3f45c7", "#ee8114", "#db3c81")) +
   theme_light() + theme(legend.position = "none")
})

output$climate_temperature_sites <- dygraphs::renderDygraph({

    dat <- data.table::fread("data/climate_historical.txt")

    if (input$spi_clim_surf) {
        dat <- dat %>%
            filter(grepl(tolower(substr(input$higherGeography, 1, 3)), parent_area_name, ignore.case = T)) %>%
            mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) %>%
            group_by(area_name, year) %>%
            summarise(depth_surface = mean(depth_bottom))
    } else {
        dat <- dat %>%
            filter(grepl(tolower(substr(input$higherGeography, 1, 3)), parent_area_name, ignore.case = T)) %>%
            mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) %>%
            group_by(area_name, year) %>%
            summarise(depth_surface = mean(depth_surface))
    }

    if (input$spi_clim_anomaly) {
        dat <- dat %>%
            group_by(area_name) %>%
            mutate(depth_surface = depth_surface - mean(depth_surface, na.rm = T))

        dat_surface <- dat %>%
            filter(!is.na(depth_surface)) %>%
            mutate(date = as.Date(paste0(year, "-01-01"))) %>%
            ungroup() %>%
            select(-year) %>%
            tidyr::pivot_wider(values_from = depth_surface, names_from = area_name)

        dat_each <- lapply(2:ncol(dat_surface), function(x) {
            xts::xts(x = dat_surface[, x], order.by = dat_surface$date)
        })

        dat_each <- do.call("cbind", dat_each)
        colnames(dat_each) <- gsub("\\.", " ", colnames(dat_each))

        dygraphs::dygraph(dat_each) %>%
            dygraphs::dyRangeSelector(height = 20) %>%
            dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(ncol(dat_each), "Set2")) %>%
            dygraphs::dyLegend(width = 500) %>%
            dygraphs::dyAxis(
                name = "x", axisLabelFormatter = "function(d){ return d.getFullYear() }"
            ) %>%
            dygraphs::dyLimit(
                limit = 0, label = paste(length(unique(dat_surface$date)), "years average"), labelLoc = c("right"),
                color = "#4161ff", strokePattern = "dashed"
            )
    } else {
        dat_surface <- dat %>%
            filter(!is.na(depth_surface)) %>%
            mutate(date = as.Date(paste0(year, "-01-01"))) %>%
            ungroup() %>%
            select(-year) %>%
            tidyr::pivot_wider(values_from = depth_surface, names_from = area_name)

        dat_each <- lapply(2:ncol(dat_surface), function(x) {
            xts::xts(x = dat_surface[, x], order.by = dat_surface$date)
        })
        dat_each <- do.call("cbind", dat_each)
        colnames(dat_each) <- gsub("\\.", " ", colnames(dat_each))

        dygraphs::dygraph(dat_each) %>%
            dygraphs::dyRangeSelector(height = 20) %>%
            dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(ncol(dat_each), "Set2")) %>%
            dygraphs::dyLegend(width = 500) %>%
            dygraphs::dyAxis(
                name = "x", axisLabelFormatter = "function(d){ return d.getFullYear() }"
            )
    }
})