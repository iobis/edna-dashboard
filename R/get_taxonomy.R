cleanst <- function(string) {
        gsub(
            " ", "_",
            stringi::stri_trans_general(
                gsub(
                    "[[:punct:]]", "",
                    tolower(string)
                ), "Latin-ASCII"
            )
        )
    }

taxonomy_by_site <- function() {

    cat("\033[36mGetting taxonomy by site\033[39m\n")

    require(arrow)
    require(dplyr)

    outf <- "data/sites_data"
    fs::dir_create(outf)

    if (!file.exists("data/output/occurrence.parquet")) {
        stop("`occurrence.parquet` not available. Run occurrence code first.")
    }

    occurrence <- open_dataset("data/output/occurrence.parquet")

    un_sites <- occurrence %>% select(higherGeography) %>% distinct() %>% collect()
    un_sites <- unlist(unique(as.vector(un_sites)))

    clean_sites <- cleanst(un_sites)

    taxonLevel <- "species"

    for (i in seq_along(un_sites)) {
        cat("Processing site", i, "out of", length(un_sites), "\n")

        occurrence_site <- occurrence %>%
            filter(higherGeography == un_sites[i]) %>%
            collect() %>%
            mutate(species = ifelse(taxonRank == "species", scientificName, NA))

        suppressMessages({
            table_data <- occurrence_site %>%
                filter(!is.na(!!sym(taxonLevel))) %>%
                group_by(phylum, class, !!sym(taxonLevel)) %>%
                summarize(
                    samples = paste(unique(materialSampleID), collapse = ","),
                    localities = paste(unique(locationID), collapse = ","),
                    target_gene = paste(unique(target_gene), collapse = ","),
                    reads = sum(organismQuantity)
                ) %>%
                ungroup()
        })

        write_parquet(table_data, file.path(outf, paste0(clean_sites[i], ".parquet")))
    }
    return(invisible(NULL))
}


diversity_by_site <- function() {

    cat("\033[36mGetting diversity by site\033[39m\n")

    require(arrow)
    require(dplyr)

    outf <- "data/sites_data"
    fs::dir_create(outf)

    if (!file.exists("data/output/occurrence.parquet")) {
        stop("`occurrence.parquet` not available. Run occurrence code first.")
    }

    occurrence <- open_dataset("data/output/occurrence.parquet")

    un_sites <- occurrence %>% select(higherGeography) %>% distinct() %>% collect()
    un_sites <- unlist(unique(as.vector(un_sites)))

    cleanst <- function(string) {
        gsub(
            " ", "_",
            stringi::stri_trans_general(
                gsub(
                    "[[:punct:]]", "",
                    tolower(string)
                ), "Latin-ASCII"
            )
        )
    }

    clean_sites <- cleanst(un_sites)

    site_diversity <- function(occurrence, site, taxonLevel, plot_type) {
        occurrence_site <- occurrence %>%
            filter(higherGeography == site) %>%
            collect() %>%
            mutate(species = ifelse(taxonRank == "species", scientificName, NA))

        # Filter out samples with very low read counts (failed samples)
        reads_site <- occurrence_site %>%
            group_by(materialSampleID) %>%
            summarize(read_sum = sum(organismQuantity))
        reads_site <- reads_site %>% filter(read_sum > 50000)

        occurrence_site <- occurrence_site %>% filter(materialSampleID %in% reads_site$materialSampleID)

        if (plot_type == "reads") {
            stats_phylum <- occurrence_site %>%
                group_by(!!sym(taxonLevel), locationID, materialSampleID, pcr_primer_name_forward) %>%
                summarize(reads = sum(organismQuantity)) %>%
                ungroup()
        } else {
            # relative abundance (if we want to 'original' relative abundance calculate like this):
            # original: we remove the contaminants, so the relative abundance is not 1
            # occurrence_site = occurrence_site %>%
            #                        group_by(materialSampleID) %>%
            #                        mutate(totalSampleSizeValue = sum(unique(sampleSizeValue)))

            # occurrence_site = occurrence_site %>% mutate(relative_abundance = organismQuantity/totalSampleSizeValue)

            # Relative abundance, calculate based on the remaining reads after removal of contaminants:
            occurrence_site <- occurrence_site %>%
                group_by(materialSampleID) %>%
                mutate(totalSampleSizeValue = sum(organismQuantity)) %>%
                ungroup()

            occurrence_site <- occurrence_site %>% mutate(relative_abundance = organismQuantity / totalSampleSizeValue)

            stats_phylum <- occurrence_site %>%
                group_by(!!sym(taxonLevel), locationID, materialSampleID, pcr_primer_name_forward) %>%
                summarize(reads = sum(relative_abundance)) %>%
                ungroup()
        }

        # select only the 10 most common (the rest are others (and unknown))
        class_summary <- stats_phylum %>%
            group_by(!!sym(taxonLevel)) %>%
            summarize(total_reads = sum(reads)) %>%
            ungroup()

        # Step 2: Identify the top 10 most common classes
        top_10_classes <- class_summary %>%
            arrange(desc(total_reads)) %>%
            head(12) %>%
            pull(!!sym(taxonLevel))

        # Step 3: Rename all other classes to "other"
        df_modified <- stats_phylum %>%
            mutate(taxonLevel = ifelse(!!sym(taxonLevel) %in% top_10_classes, !!sym(taxonLevel), "other")) %>%
            group_by(taxonLevel, locationID, materialSampleID, pcr_primer_name_forward) %>%
            summarize(reads = sum(reads)) %>%
            ungroup()

        # For plots separated by markers add and input for show_primers (but to do: calculate new relative abundance also)
        # if (show_primers) {

        # ggplot(data=df_modified, aes(y=reads, x=materialSampleID, fill=taxonLevel), color="gray") +
        #    geom_bar(stat="identity",color="gray") +
        #    facet_grid(pcr_primer_name_forward~locality, scales="free_x", space = "free")+
        #    theme_minimal() +
        #    guides(fill=guide_legend(title=taxonLevel))+
        #    ggtitle("The ten most common taxa")+
        #    scale_fill_viridis(discrete=T,na.value = "gray")

        # } else {

        df_modified <- df_modified %>%
            group_by(taxonLevel, locationID, materialSampleID) %>%
            summarize(reads = sum(reads)) %>%
            ungroup()

        return(df_modified)
    }

    comb_matrix <- expand.grid(
        rank = c("phylum", "order", "class", "family", "genus", "species"),
        ptype = c("reads", "abundance"),
        stringsAsFactors = F
    )

    cat(nrow(comb_matrix), "combinations per site. \n")

    for (i in seq_along(un_sites)) {
        cat("Processing site", i, "out of", length(un_sites), "\n")

        site_full <- lapply(seq_len(nrow(comb_matrix)), function(x){
            cat(" > ", x)
            site_div <- suppressMessages(site_diversity(occurrence, un_sites[i], comb_matrix$rank[x], comb_matrix$ptype[x]))
            site_div$rank <- comb_matrix$rank[x]
            site_div$plot_type <- comb_matrix$ptype[x]
            site_div
        })

        site_full <- dplyr::bind_rows(site_full)

        write_parquet(site_full, file.path(outf, paste0(clean_sites[i], "_barplot.parquet")))

        cat("\n")
    }

    return(invisible(NULL))
}


diversity_metrics_by_site <- function() {

    cat("\033[36mGetting diversity metrics (Alpha/Beta) by site\033[39m\n")

    suppressPackageStartupMessages(require(vegan))
    suppressPackageStartupMessages(require(dplyr))
    suppressPackageStartupMessages(require(ape))
    suppressPackageStartupMessages(require(arrow))
    suppressPackageStartupMessages(require(dplyr))

    outf <- "data/sites_data"
    fs::dir_create(outf)

    if (!file.exists("data/output/occurrence.parquet")) {
        stop("`occurrence.parquet` not available. Run occurrence code first.")
    }

    occurrence <- open_dataset("data/output/occurrence.parquet")

    un_sites <- occurrence %>% select(higherGeography) %>% distinct() %>% collect()
    un_sites <- unlist(unique(as.vector(un_sites)))

    clean_sites <- cleanst(un_sites)

    metrics_by_site <- function(occurrence, site, taxonLevel, beta_div_measures = c("jaccard", "bray")) {

        message('\r', strrep(" ", 200), '\r', 
            paste0("\033[33m--> Site '", site, "' with taxonLevel '", taxonLevel, "'\033[39m"),
            appendLF = FALSE)

        occurrence_site <- occurrence %>%
            filter(higherGeography == site) %>%
            filter(!is.na(locationID)) %>%
            collect()

        # Filter out samples with very low read counts (failed samples)
        reads_site <- occurrence_site %>%
            group_by(materialSampleID) %>%
            summarize(read_sum = sum(organismQuantity))
        reads_site <- reads_site %>% filter(read_sum > 50000)

        occurrence_site <- occurrence_site %>% filter(materialSampleID %in% reads_site$materialSampleID)

        suppressMessages({
            if (taxonLevel == "ASV") {
                stats_mds <- occurrence_site %>%
                    group_by(materialSampleID, DNA_sequence) %>%
                    summarize(reads = as.numeric(sum(organismQuantity))) %>%
                    ungroup() %>%
                    pivot_wider(names_from = DNA_sequence, values_from = reads, values_fill = 0)
            } else if (taxonLevel != "species") {
                stats_mds <- occurrence_site %>%
                    filter(!is.na(!!sym(taxonLevel))) %>%
                    group_by(materialSampleID, !!sym(taxonLevel)) %>%
                    summarize(reads = as.numeric(sum(organismQuantity))) %>%
                    ungroup() %>%
                    pivot_wider(names_from = !!sym(taxonLevel), values_from = reads, values_fill = 0)
            } else {
                stats_mds <- occurrence_site %>%
                    filter(taxonRank == "species") %>%
                    group_by(materialSampleID, scientificName) %>%
                    summarize(reads = as.numeric(sum(organismQuantity))) %>%
                    ungroup() %>%
                    pivot_wider(names_from = scientificName, values_from = reads, values_fill = 0)
            }
        })

        stats_mds <- as.data.frame(stats_mds)
        rownames(stats_mds) <- stats_mds$materialSampleID
        stats_mds <- stats_mds[, -1]
        OTU <- as.matrix(stats_mds)


        ##### Plot 1
        estR <- as.data.frame(t(vegan::estimateR(OTU)))
        estR$shannon <- vegan::diversity(OTU, index = "shannon")
        estR$simpson <- vegan::diversity(OTU, index = "simpson")
        colnames(estR) <- c("Observed", "Chao1", "SE.Chao1", "ACE", "SE.ACE", "Shannon", "Simpson")
        estR$materialSampleID <- rownames(estR)

        suppressMessages({
            estR <- estR %>%
                left_join(occurrence %>% select(materialSampleID, locationID, decimalLatitude, decimalLongitude) %>% distinct() %>% collect()) %>%
                select(-decimalLatitude, -decimalLongitude)
        })

        estR$taxonLevel <- taxonLevel

        #### Plot 2
        beta_stats <- list()
        for (bd in seq_along(beta_div_measures)) {
            divR <- vegdist(OTU, method = beta_div_measures[bd])

            PCOA <- pcoa(divR)
            PCOAaxes <- as.data.frame(PCOA$vectors[, c(1, 2)])
            val1 <- paste0("PCoA1 (", round(PCOA$values$Relative_eig[1] * 100, 2), "%)")
            val2 <- paste0("PCoA2 (", round(PCOA$values$Relative_eig[2] * 100, 2), "%)")
            colnames(PCOAaxes) <- c(val1, val2)
            PCOAaxes$materialSampleID <- rownames(PCOAaxes)


            suppressMessages({
                PCOAaxes <- PCOAaxes %>%
                    left_join(occurrence %>% select(materialSampleID, locationID, decimalLatitude, decimalLongitude) %>% distinct() %>% collect())
            })

            PCOAaxes$betaMeasure <- beta_div_measures[bd]
            PCOAaxes$taxonLevel <- taxonLevel

            beta_stats[[bd]] <- PCOAaxes
        }

        names(beta_stats) <- beta_div_measures

        return(list(
            alpha = estR,
            beta = beta_stats
        ))
    }

    ranks <- c("ASV", "kingdom", "phylum", "order", "class", "family", "genus", "species")

    cat("Processing", length(ranks), "ranks for", length(un_sites), " sites.\n")

    for (sit in seq_along(un_sites)) {
        int_res <- lapply(ranks, function(rn) {
            metrics_by_site(occurrence, site = un_sites[sit], taxonLevel = rn)
        })
        names(int_res) <- ranks

        saveRDS(int_res, file.path(outf, paste0(clean_sites[sit], "_divmetrics.rds")))

        rm(int_res)
    }

    cat("\n")

    return(invisible(NULL))
}

# Output explanation:
# One RDS object per site
# Each RDS is a list of lists
# The first level of the list is the rank
# For each rank, a list containing alpha and beta diversity
# For beta diversity, it is another list with two levels
# One for each beta diversity stat