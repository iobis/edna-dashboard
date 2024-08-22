
library(dplyr)
library(eulerr)

get_dna_statistics <- function(occurrence, site){

#site="Aldabra Atoll"
occurrence_site <- occurrence %>% filter(higherGeography == site)

## Stats for all markers:

total_sequences  <- sum(occurrence_site$organismQuantity)

n_sequence_sample <- occurrence_site %>% group_by(materialSampleID) %>% summarize( reads= sum(organismQuantity))
avg_sequences_per_sample <- mean(n_sequence_sample$reads)
sd_sequences_per_sample <- sd(n_sequence_sample$reads)

total_asvs <- length(unique(occurrence_site$DNA_sequence))

n_asvs_samples <- occurrence_site %>% group_by(materialSampleID) %>% summarize( asvs= n_distinct(DNA_sequence))
avg_asvs_per_sample <- mean(n_asvs_samples$asvs)
sd_asvs_per_sample  <- sd(n_asvs_samples$asvs)


total_species <- nrow(occurrence_site %>% filter(taxonRank=='species')%>% select(scientificName) %>% distinct())

n_species_samples <- occurrence_site %>% filter(taxonRank=='species') %>% group_by(materialSampleID) %>% summarize( n_species= n_distinct(scientificName))
avg_species_per_sample <- mean(n_species_samples$n_species)
sd_species_per_sample  <- sd(n_species_samples$n_species)

 card_content <- sprintf(
    "## Summary Statistics\n\n
    - **Total number of sequences:** %d\n
    - **Average sequences by sample:** %.2f (SD: %.2f)\n
    - **Total number of ASVs:** %d\n
    - **Average ASVs by sample:** %.2f (SD: %.2f)\n
    - **Number of species:** %d\n
    - **Average species by sample:** %.2f (SD: %.2f)\n",
    total_sequences, avg_sequences_per_sample, sd_sequences_per_sample,
    total_asvs, avg_asvs_per_sample, sd_asvs_per_sample,
    total_species, avg_species_per_sample, sd_species_per_sample
  )

# Print the card with multiple values
cat(card_content)
}



make_venn_diagram_species_primers <- function(occurrence, site, taxonLevel){
    
  occurrence_site <- occurrence %>% filter(higherGeography==site) %>% 
  mutate(species = ifelse(taxonRank == 'species', scientificName, NA))

if (taxonLevel=="ASV") {

MifishUE = occurrence_site %>% filter(pcr_primer_name_forward=="MiFish-UE-F") %>% select(DNA_sequence) %>% distinct() 
MiMammalUEB = occurrence_site %>% filter(pcr_primer_name_forward=="MiMammal-UEB-F")%>% select(DNA_sequence) %>% distinct()
Teleo = occurrence_site %>% filter(pcr_primer_name_forward=="teleo_F_L1848")%>% select(DNA_sequence) %>% distinct()
Vert16S = occurrence_site %>% filter(pcr_primer_name_forward=="Vert-16S-eDNA-F1")%>% select(DNA_sequence) %>% distinct()
COI = occurrence_site %>% filter(pcr_primer_name_forward=="mlCOIintF")%>% select(DNA_sequence) %>% distinct()

taxa_by_primer <- list(
    MifishUE = MifishUE$DNA_sequence,
    MiMammalUEB = MiMammalUEB$DNA_sequence,
    Teleo = Teleo$DNA_sequence, 
    Vert16S  = Vert16S$DNA_sequence,
    COI = COI$DNA_sequence
)


} else {
    
MifishUE = occurrence_site %>% filter(pcr_primer_name_forward=="MiFish-UE-F") %>% select(!!(sym(taxonLevel))) %>% distinct() 
MiMammalUEB = occurrence_site %>% filter(pcr_primer_name_forward=="MiMammal-UEB-F")%>% select(!!(sym(taxonLevel))) %>% distinct()
Teleo = occurrence_site %>% filter(pcr_primer_name_forward=="teleo_F_L1848")%>% select(!!(sym(taxonLevel))) %>% distinct()
Vert16S = occurrence_site %>% filter(pcr_primer_name_forward=="Vert-16S-eDNA-F1")%>% select(!!(sym(taxonLevel))) %>% distinct()
COI = occurrence_site %>% filter(pcr_primer_name_forward=="mlCOIintF")%>% select(!!(sym(taxonLevel))) %>% distinct()


taxa_by_primer <- list(
    MifishUE = MifishUE[,1],
    MiMammalUEB = MiMammalUEB[,1],
    Teleo = Teleo[,1], 
    Vert16S  = Vert16S[,1],
    COI = COI[,1]
)

}

set.seed(1)

return(plot(euler(taxa_by_primer[1:4], shape = "ellipse"), quantities = TRUE))
#ggplot(occurrence_site %>% group_by(pcr_primer_name_forward) %>% summarize(n=sum(organismQuantity)), aes(x=pcr_primer_name_forward, y=n)) + geom_bar(stat="identity")
#ggplot(occurrence_site %>% group_by(pcr_primer_name_forward) %>% summarize(n=length(unique(scientificName))), aes(x=pcr_primer_name_forward, y=n)) + geom_bar(stat="identity")
}