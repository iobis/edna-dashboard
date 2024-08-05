library(ggplot2)
library(viridis)
library(eulerr)

make_image_phylum <- function(occurrence, site){

stats_phylum <- occurrence %>%
filter(higherGeography==site) %>%
  filter(!is.na(phylum)) %>%
  group_by(phylum) %>%
  summarize(reads = sum(organismQuantity), species = length(unique(na.omit(species)))) %>%
  ungroup()

ggplot(data = stats_phylum) +
  geom_bar(aes(y = phylum, x = reads, fill = phylum), stat = "identity") +
  scale_x_continuous(trans = "log10") +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.2, end = 0.8) +
  scale_y_discrete(limits = rev) +
  #facet_grid(eventType~locationID) +
  theme(legend.position = "none") +
  ggtitle("Number of reads by location, sample type, and phylum")

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

site="Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems"
