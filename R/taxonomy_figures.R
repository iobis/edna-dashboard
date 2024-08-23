library(ggplot2)
library(viridis)
library(eulerr)
library(reactable)
library(dplyr)
library(tidyr)
library(data.tree)
library(ggtree)
library(ggtreeExtra)
library(phyloseq)
library(psadd)



make_image_taxonomy <- function(occurrence, site, taxonLevel, plot_type){


occurrence_site <- occurrence %>%
                        filter(higherGeography==site) %>%
                        mutate(species = ifelse(taxonRank == 'species', scientificName, NA))


if (plot_type == "reads") {

stats_phylum <- occurrence_site %>%
  group_by(!!sym(taxonLevel), locality, materialSampleID, pcr_primer_name_forward) %>%
  summarize(reads = sum(organismQuantity)) %>%
  ungroup()

} else {

#relative abundance (if we want to 'original' relative abundance calculate like this):
#original: we remove the contaminants, so the relative abundance is not 1
#occurrence_site = occurrence_site %>% 
#                        group_by(materialSampleID) %>% 
#                        mutate(totalSampleSizeValue = sum(unique(sampleSizeValue)))

#occurrence_site = occurrence_site %>% mutate(relative_abundance = organismQuantity/totalSampleSizeValue)

#Relative abundance, calculate based on the remaining reads after removal of contaminants:
occurrence_site = occurrence_site %>% 
                        group_by(materialSampleID) %>% 
                        mutate(totalSampleSizeValue = sum(organismQuantity))

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

#For plots separated by markers add and input for show_primers (but to do: calculate new relative abundance also)
#if (show_primers) {

#ggplot(data=df_modified, aes(y=reads, x=materialSampleID, fill=taxonLevel), color="gray") +
#    geom_bar(stat="identity",color="gray") + 
#    facet_grid(pcr_primer_name_forward~locality, scales="free_x", space = "free")+
#    theme_minimal() + 
#    guides(fill=guide_legend(title=taxonLevel))+
#    ggtitle("The ten most common taxa")+
#    scale_fill_viridis(discrete=T,na.value = "gray")

#} else {

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

#}


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
  reactable(pagination = TRUE, wrap = FALSE, highlight = TRUE, outlined = TRUE, striped = TRUE, defaultPageSize = 10, filterable = TRUE, defaultSortOrder = "desc", columns = list(
    "phylum" = text_def_narrow,
    "class" = text_def_narrow,
    "species" = species_def,
    "reads" = reads_def
  ),
  defaultSorted ="reads")


#return(table_data)
return(rtable)
}

make_krona_plot <- function(){

#Add simplified site name for file headers because otherwise kronatools does not work
sites <- jsonlite::read_json("https://raw.githubusercontent.com/iobis/edna-tracker-data/data/generated.json")
sites_list <- sites$sites
sites_samples <- sites$samples

sites_names <- sapply(sites_list, function(x) x$name)
sites_names_simple <- sapply(sites_list, function(x) x$simplified_name)

site_naming <- data.frame(sites_names, sites_names_simple)

for (i in 1:20){
  site=unique(occurrence$higherGeography)[i]
  simple_site_name <- site_naming %>% filter(sites_names==site) %>% select(sites_names_simple)

#occurrence_site <- occurrence %>% filter(higherGeography==unique(occurrence$higherGeography)[i]) %>%
occurrence_site <- occurrence %>% filter(higherGeography==site) %>%
                        mutate(species = ifelse(taxonRank == 'species', scientificName, NA))%>%
                        mutate(simple_site_name = simple_site_name$sites_names_simple)


#Make a phyloseq object for ease of access

otu_table <- occurrence_site %>% 
  select(DNA_sequence, organismQuantity, materialSampleID) %>%
  pivot_wider(names_from = materialSampleID, values_from = organismQuantity, values_fn=sum)

otu_table=as.data.frame(otu_table)
rownames(otu_table)=otu_table$DNA_sequence
otu_table=otu_table[,-1]
otu_table[is.na(otu_table)]=0

tax_table= occurrence_site %>% 
  select(DNA_sequence, kingdom, phylum, class, order, family, genus, species) %>% distinct(DNA_sequence, .keep_all = T)
rownames(tax_table)=tax_table$DNA_sequence
tax_table=tax_table[,-1]
colnames(tax_table) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxmat=as.matrix(tax_table, rownames.force = T)

sample_data = occurrence_site %>% 
  select(materialSampleID, eventRemarks, locality, decimalLongitude, decimalLatitude, sampleSize, higherGeography, locationID, simple_site_name) %>% distinct()

rownames(sample_data)=sample_data$materialSampleID
sample_data=sample_data[,-1]

#sequences <- Biostrings::DNAStringSet(rownames(otu_table))

otutab=phyloseq::otu_table(otu_table, taxa_are_rows = T)
taxtab=phyloseq::tax_table(taxmat)
sampledat=phyloseq::sample_data(sample_data)

ps=phyloseq::phyloseq(otutab, taxtab, sampledat)

#plot_name=paste0("./www/krona_plots/",gsub(" ", "_", unique(occurrence$higherGeography)[i]))
plot_name=paste0("./www/krona_plots/",gsub(" |\\(|\\)","", simple_site_name$sites_names_simple))

#Special characters in higherGeography are ruining this run:
psadd::plot_krona(ps, plot_name, "simple_site_name", trim=TRUE)

}
}

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