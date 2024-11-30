#climate_data <- readRDS("data/climate.rds")

climate_vals <- reactiveValues()

observe({
    req(input$higherGeography != "")

    climate_filt <- climate_species %>%
        select(
            Species = scientificName, higherGeography, Phylum = phylum, Class = class,
            Depth = habitat, `Upper limit` = sp_limit,
            Current = baseline, SSP1 = ssp126_dec100,
            SSP2 = ssp245_dec100, SSP3 = ssp370_dec100, group, category
        ) %>%
        filter(higherGeography == input$higherGeography) %>%
        select(-higherGeography) %>%
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
    req(input$higherGeography != "")

    dat <- climate_vals$main
    dat[, c("Current", "SSP1", "SSP2", "SSP3")] <- round(dat[, c("Current", "SSP1", "SSP2", "SSP3")], 2)

    colfun <- function(value) {
        if (value < 0) {
            color <- "#c5ddf2"
        } else if (value > 0) {
            color <- "#e8c9b4"
        } else {
            color <- "#777"
        }
        list(background = color, fontWeight = "bold")
    }

    dat$Depth <- ifelse(dat$Depth == "Shallow", "S",
        ifelse(dat$Depth == "Not shallow", "NS", "NA"))

    colfun2 <- function(value) {
        if (value == "S") {
            color <- "#675292"
        } else if (value == "NS") {
            color <- "#b69354"
        } else {
            color <- "#868686"
        }
        list(color = color, fontWeight = "bold")
    }

    reactable(dat, columns = list(
        Species = colDef(style = list(fontStyle = "italic"), searchable = T),
        `Upper limit` = colDef(minWidth = 95, maxWidth = 105),
        Depth = colDef(style = colfun2, minWidth = 70, maxWidth = 80, searchable = T),
        SSP1 = colDef(style = colfun, minWidth = 70, maxWidth = 80),
        SSP2 = colDef(style = colfun, minWidth = 70, maxWidth = 80),
        SSP3 = colDef(style = colfun, minWidth = 70, maxWidth = 80),
        Current = colDef(style = colfun, minWidth = 80, maxWidth = 90)
    ), filterable = TRUE)
}) %>%
    bindEvent(climate_vals$main)

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
    req(input$higherGeography != "")
    
    require(ggplot2)
    ggplot(climate_vals$status$data) +
        geom_bar(aes(x = Scenario, fill = Scenario, y = `Number of species`), stat = "identity") +
        scale_y_continuous(limits = c(0, climate_vals$status$total)) +
        #scale_fill_manual(values = c("#675292", "#a13c93", "#f47f4e", "#f15e6b")) +
        scale_fill_manual(values = c("#FDA638", "#459395", "#EB7C69", "#866f85")) +
        #scale_fill_manual(values = c("#11b1aa", "#3f45c7", "#ee8114", "#db3c81")) +
    theme_light() + theme(legend.position = "none")
}) %>%
    bindEvent(climate_vals$status)

output$climate_temperature_sites <- dygraphs::renderDygraph({

    req(input$higherGeography != "")

    dat <- suppressWarnings(data.table::fread("data/climate_historical.txt"))

    # if (input$spi_clim_surf) {
    #     dat <- dat %>%
    #         #filter(grepl(tolower(substr(input$higherGeography, 1, 3)), parent_area_name, ignore.case = T)) %>%
    #         filter(parent_area_name == input$higherGeography) %>%
    #         mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) %>%
    #         group_by(station, year) %>%
    #         summarise(depth_surface = mean(depth_bottom)) %>% 
    #         ungroup()
    # } else {
    #     dat <- dat %>%
    #         #filter(grepl(tolower(substr(input$higherGeography, 1, 3)), parent_area_name, ignore.case = T)) %>%
    #         filter(parent_area_name == input$higherGeography) %>%
    #         mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) %>%
    #         group_by(station, year) %>%
    #         summarise(depth_surface = mean(depth_surface)) %>% 
    #         ungroup()
    # }
    dat <- dat %>%
            #filter(grepl(tolower(substr(input$higherGeography, 1, 3)), parent_area_name, ignore.case = T)) %>%
            filter(parent_area_name == input$higherGeography) %>%
            mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))) %>%
            group_by(station, year) %>%
            summarise(depth_surface = mean(depth_surface)) %>% 
            ungroup()

    dat <- dat %>% filter(year < 2024) # Remove this year while not complete

    if (input$spi_clim_anomaly) {
        dat <- dat %>%
            group_by(station) %>%
            mutate(depth_surface = depth_surface - mean(depth_surface, na.rm = T)) %>% 
            ungroup()

        dat_surface <- dat %>%
            filter(!is.na(depth_surface)) %>%
            mutate(date = as.Date(paste0(year, "-01-01"))) %>%
            ungroup() %>%
            select(-year) %>%
            tidyr::pivot_wider(values_from = depth_surface, names_from = station)

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
            tidyr::pivot_wider(values_from = depth_surface, names_from = station)

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
                name = "x", axisLabelFormatter = "function(d){ return d.getFullYear() }",
                label = "Time (monthly)"
            ) %>%
            dygraphs::dyAxis(name = "y", label = "Temperature (°C)")
    }
}) %>%
    bindEvent(input$higherGeography, input$spi_clim_anomaly, input$spi_clim_surf)