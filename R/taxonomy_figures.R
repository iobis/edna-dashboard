suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(eulerr))
suppressPackageStartupMessages(library(reactable))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(data.tree))
suppressPackageStartupMessages(library(ggtree))
suppressPackageStartupMessages(library(ggtreeExtra))
suppressPackageStartupMessages(library(phyloseq))
suppressPackageStartupMessages(library(psadd))

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

make_image_taxonomy <- function(site, taxonLevel, plot_type) {

  site <- cleanst(site)
  sel_type <- ifelse(
    plot_type == "reads", "reads", "abundance"
  )
  taxon_rank <- taxonLevel

  site_data <- arrow::read_parquet(
    file.path("data/sites_data", paste0(site, "_barplot.parquet"))
  )

  df_modified <- site_data %>%
    filter(rank == taxon_rank) %>%
    filter(plot_type == sel_type)
  
  ggplot(data = df_modified, aes(y = reads, x = materialSampleID, fill = taxonLevel), color = "gray") +
      geom_bar(stat = "identity", color = "gray", linewidth = 0.2) + 
      facet_grid(~locationID, scales = 'free_x', space = "free")+
      theme_minimal() + 
      guides(fill = guide_legend(title=taxonLevel))+
      ggtitle("The ten most common taxa")+
      scale_fill_viridis(discrete = T,na.value = "gray")

}

# make searchable table for browsing. 
# Possible to add taxonLevel choice, but now phylum, class, species information only.

make_table_taxonomy <- function(occurrence, site) {

  site <- cleanst(site)
  table_data <- arrow::read_parquet(file.path(
    "data/sites_data", paste0(site, ".parquet")
  ))

  species_def <- colDef(
    minWidth = 200,
    style = list("font-size" = "0.9em", "font-style" = "italic")
  )
  
  reads_def <- colDef(
    style = list("font-size" = "0.9em")
  )
  
  text_def_narrow <- colDef(
    maxWidth = 130,
    style = list("font-size" = "0.9em")
  )
  
  phylum_def_narrow <- colDef(
    maxWidth = 130,
    style = list("font-size" = "0.9em"),
    cell = function(value, index, name) {
      value <- dplyr::case_when(
        value == "Chlorophyta" ~ '<img src="images/icons/taxon_Chlorophyta.svg" alt="Chlorophyta" width = "15" height = "15"></i> Chlorophyta',
        value == "Chordata" ~ '<img src="images/icons/taxon_Chordata.svg" alt="Chordata" width = "15" height = "15"></i> Chordata',
        .default = paste('<img src="images/icons/taxon_unknown.svg" alt="Others" width = "15" height = "15"></i>', value)
      )
      htmltools::HTML(value)
    },
    html = TRUE
  )

  rtable <- table_data %>%
    reactable(pagination = TRUE, wrap = FALSE, highlight = TRUE, outlined = TRUE, striped = TRUE, defaultPageSize = 10, filterable = TRUE, defaultSortOrder = "desc", columns = list(
      "phylum" = phylum_def_narrow,
      "class" = text_def_narrow,
      "species" = species_def,
      "reads" = reads_def
    ),
    defaultSorted = "reads")

  return(rtable)
}

make_krona_plot <- function(occurrence) {

  # Add simplified site name for file headers because otherwise kronatools does not work
  sites <- jsonlite::read_json("https://raw.githubusercontent.com/iobis/edna-tracker-data/data/generated.json")
  sites_list <- sites$sites
  sites_samples <- sites$samples
  
  sites_names <- sapply(sites_list, function(x) x$name)
  sites_names_simple <- sapply(sites_list, function(x) x$simplified_name)
  
  site_naming <- data.frame(sites_names, sites_names_simple)
  
  unique_geography <- unique(occurrence$higherGeography)

  for (i in 1:length(unique_geography)) {
    site <- unique_geography[i]
    simple_site_name <- site_naming %>% filter(sites_names == site) %>% select(sites_names_simple)
  
    #occurrence_site <- occurrence %>% filter(higherGeography==unique(occurrence$higherGeography)[i]) %>%
    occurrence_site <- occurrence %>% filter(higherGeography == site) %>%
                            mutate(species = ifelse(taxonRank == 'species', scientificName, NA)) %>%
                            mutate(simple_site_name = simple_site_name$sites_names_simple)

    # Make a phyloseq object for ease of access
    
    otu_table <- occurrence_site %>% 
      select(DNA_sequence, organismQuantity, materialSampleID) %>%
      pivot_wider(names_from = materialSampleID, values_from = organismQuantity, values_fn = sum)
    
    otu_table <- as.data.frame(otu_table)
    rownames(otu_table) <- otu_table$DNA_sequence
    otu_table <- otu_table[,-1]
    otu_table[is.na(otu_table)] <- 0
    
    tax_table= occurrence_site %>% 
      select(DNA_sequence, kingdom, phylum, class, order, family, genus, species) %>%
      distinct(DNA_sequence, .keep_all = T)
    tax_table <- as.data.frame(tax_table)
    rownames(tax_table) <- tax_table$DNA_sequence
    tax_table <- tax_table[,-1]
    colnames(tax_table) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    taxmat <- as.matrix(tax_table, rownames.force = T)
    
    sample_data <- occurrence_site %>% 
      select(materialSampleID, eventRemarks, locationID, decimalLongitude, decimalLatitude, sampleSize, higherGeography, locationID, simple_site_name) %>%
      distinct()
    
    sample_data <- as.data.frame(sample_data)
    rownames(sample_data) <- sample_data$materialSampleID
    sample_data <- sample_data[,-1]
    
    # sequences <- Biostrings::DNAStringSet(rownames(otu_table))
    
    otutab <- phyloseq::otu_table(otu_table, taxa_are_rows = T)
    taxtab <- phyloseq::tax_table(taxmat)
    sampledat <- phyloseq::sample_data(sample_data)
    
    ps <- phyloseq::phyloseq(otutab, taxtab, sampledat)
    
    # plot_name=paste0("./www/krona_plots/",gsub(" ", "_", unique(occurrence$higherGeography)[i]))
    plot_name <- paste0("./www/krona_plots/",gsub(" |\\(|\\)","", simple_site_name$sites_names_simple))
    
    # Special characters in higherGeography are ruining this run:
    psadd::plot_krona(ps, plot_name, "simple_site_name", trim = TRUE)
  
  }
}

make_taxonomic_tree <- function(occurrence, site) {

  occurrence_site <- occurrence %>% filter(higherGeography == site) %>%
                          collect() %>%
                          mutate(species = ifelse(taxonRank == 'species', scientificName, NA))
  
  df <- occurrence_site %>%
    dplyr::group_by(phylum, class, order, family, pcr_primer_name_forward) %>%
    summarize(species = n(), reads = sum(organismQuantity)) %>%
    mutate(domain = "Eukaryota") %>%
    ungroup() %>%
    filter(!is.na(family))
  
  top_phyla <- df %>%
    group_by(phylum) %>%
    summarize(species = sum(species)) %>%
    arrange(desc(species)) %>%
    ungroup() %>% 
    head(10) %>%
    pull(phylum)
  
  df_subset <- df %>%
    filter(phylum %in% top_phyla)
  
  paths <- df_subset %>%
    tidyr::unite(path, c(domain, phylum, class, order, family), sep = ";")
  nwk <- ToNewick(as.Node(paths, pathName = "path", mode = "table", pathDelimiter = ";"))
  writeLines(nwk, "tree.nwk")
  
  tree <- read.tree("tree.nwk")

  ggtree(tree, layout = "circular") +
    geom_nodepoint() +
    geom_fruit(data = df_subset %>% filter(!is.na(family)), aes(x = species, y = family, fill = phylum), geom = geom_bar, stat = "identity") +
    #geom_fruit(data = json_full$stats$markers_family %>% filter(!is.na(family)), aes(y = family, x = pcr_primer_name_forward, size = reads, color = pcr_primer_name_forward), geom = geom_point, name = "marker") +
    geom_fruit(data = df_subset %>% filter(!is.na(family)), aes(y = family, x = pcr_primer_name_forward, size = reads, color = pcr_primer_name_forward), geom = geom_point, name = "marker") +
    geom_tiplab(size = 3, offset = 60) +
    scale_fill_brewer(palette = "Spectral") +
    scale_color_brewer(palette = "Paired") +
    guides(color = guide_legend(title = "marker")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.box = "vertical")

}