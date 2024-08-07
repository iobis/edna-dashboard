library(ggplot2)
library(viridis)
library(eulerr)
library(reactable)
library(dplyr)
library(tidyr)
library(data.tree)
library(ggtree)
library(ggtreeExtra)



make_image_taxonomy <- function(occurrence, site, taxonLevel, plot_type, show_primers){

#taxonLevel="phylum"
#plot_type="reads"
#show_primers=FALSE

occurrence_site <- occurrence %>%
                        filter(higherGeography==site) %>%
                        mutate(species = ifelse(taxonRank == 'species', scientificName, NA))


if (plot_type == "reads") {

stats_phylum <- occurrence_site %>%
  group_by(!!sym(taxonLevel), locality, materialSampleID, pcr_primer_name_forward) %>%
  summarize(reads = sum(organismQuantity)) %>%
  ungroup()

} else {

#relative abundance:
occurrence_site = occurrence_site %>% 
                        group_by(materialSampleID) %>% 
                        mutate(totalSampleSizeValue = sum(unique(sampleSizeValue)))

occurrence_site = occurrence_site %>% mutate(relative_abundance = organismQuantity/totalSampleSizeValue)

stats_phylum <- occurrence_site %>%
  group_by(!!sym(taxonLevel), locality, materialSampleID, pcr_primer_name_forward) %>%
  summarize(reads = sum(relative_abundance)) %>%
  ungroup()

}

#select only the 10 most common (the rest are others (and unknown))
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
df_modified <- stats_phylum  %>%
  mutate(taxonLevel = ifelse(!!sym(taxonLevel) %in% top_10_classes, !!sym(taxonLevel), "other")) %>% 
  group_by(taxonLevel, locality, materialSampleID, pcr_primer_name_forward) %>%
  summarize(reads = sum(reads)) %>%
  ungroup()


if (show_primers) {

ggplot(data=df_modified, aes(y=reads, x=materialSampleID, fill=taxonLevel), color="gray") +
    geom_bar(stat="identity",color="gray") + 
    facet_grid(pcr_primer_name_forward~locality, scales="free_x", space = "free")+
    theme_minimal() + 
    guides(fill=guide_legend(title=taxonLevel))+
    ggtitle("The ten most common taxa")+
    scale_fill_viridis(discrete=T,na.value = "gray")

} else {

df_modified <- df_modified %>%  group_by(taxonLevel, locality, materialSampleID) %>%
  summarize(reads = sum(reads)) %>%
  ungroup()

ggplot(data=df_modified, aes(y=reads, x=materialSampleID, fill=taxonLevel), color="gray") +
    geom_bar(stat="identity",color="gray") + 
    facet_grid(~locality, scales='free_x', space = "free")+
    theme_minimal() + 
    guides(fill=guide_legend(title=taxonLevel))+
    ggtitle("The ten most common taxa")+
    scale_fill_viridis(discrete=T,na.value = "gray")

}

}


#make searchable table for browsing. 
#Possible to add taxonLevel choice, but now phylum, class, species information only.
make_table_taxonomy <- function(occurrence, site){

taxonLevel="species"

occurrence_site <- occurrence %>%
                        filter(higherGeography==site) %>%
                        mutate(species = ifelse(taxonRank == 'species', scientificName, NA))

table_data <- occurrence_site %>% 
                filter(!is.na(!!sym(taxonLevel))) %>% 
                group_by(phylum, class, !!sym(taxonLevel)) %>% 
                summarize(samples = paste(unique(materialSampleID), collapse = ","), 
                        localities = paste(unique(locality), collapse = ","), 
                        target_gene = paste(unique(target_gene), collapse = ","),
                        reads = sum(organismQuantity))


species_def <- colDef(
  minWidth = 200,
  style = list("font-size" = "0.9em")
)

reads_def <- colDef(
  style = list("font-size" = "0.9em")
)

text_def_narrow <- colDef(
  maxWidth = 130,
  style = list("font-size" = "0.9em")
)


rtable<-table_data %>%
  reactable(pagination = TRUE, wrap = FALSE, highlight = TRUE, outlined = TRUE, striped = TRUE, defaultPageSize = 20, filterable = TRUE, columns = list(
    "phylum" = text_def_narrow,
    "class" = text_def_narrow,
    "species" = species_def,
    "reads" = reads_def
  ))


#return(table_data)
return(rtable)
}



make_venn_diagram_species_primers <- function(occurrence, site){

occurrence_site <- occurrence %>% filter(higherGeography==site) %>% filter(taxonRank=="species")

primers<-unique(occurrence_site$pcr_primer_name_forward)

species_by_primer <- list(
  MifishUE = unique(occurrence_site$scientificName[occurrence_site$pcr_primer_name_forward=="MiFish-UE-F"]),
  MiMammalUEB = unique(occurrence_site$scientificName[occurrence_site$pcr_primer_name_forward=="MiMammal-UEB-F"]),
  Teleo = unique(occurrence_site$scientificName[occurrence_site$pcr_primer_name_forward=="teleo_F_L1848"]),
  Vert16S = unique(occurrence_site$scientificName[occurrence_site$pcr_primer_name_forward=="Vert-16S-eDNA-F1"]),
  COI = unique(occurrence_site$scientificName[occurrence_site$pcr_primer_name_forward=="mlCOIintF"])
)
set.seed(1)
return(plot(euler(species_by_primer[1:4], shape = "ellipse"), quantities = TRUE))

#ggplot(occurrence_site %>% group_by(pcr_primer_name_forward) %>% summarize(n=sum(organismQuantity)), aes(x=pcr_primer_name_forward, y=n)) + geom_bar(stat="identity")
#ggplot(occurrence_site %>% group_by(pcr_primer_name_forward) %>% summarize(n=length(unique(scientificName))), aes(x=pcr_primer_name_forward, y=n)) + geom_bar(stat="identity")
}

#site="Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems"




make_taxonomic_tree <- function(occurrence, site){

occurrence_site <- occurrence %>% filter(higherGeography==site) %>%
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