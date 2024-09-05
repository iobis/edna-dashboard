library(vegan)
library(dplyr)
library(ape)

#This script calculates and plots different diversity measures.
#In the future, calculate and plot separately, so the calculation is only done once? 
#To add also calculation of statistical significance of differences between localities?


get_alpha_diversity<-function(occurrence, site, taxonLevel, alpha_div_measure, beta_div_measure){
 
 occurrence_site <- occurrence %>%
        filter(higherGeography==site) %>%
        filter(!is.na(locationID))


#Filter out samples with very low read counts (failed samples)
reads_site <- occurrence_site %>% group_by(materialSampleID) %>% summarize(read_sum =sum(organismQuantity))
reads_site <- reads_site %>% filter(read_sum > 50000)

occurrence_site <- occurrence_site %>% filter(materialSampleID %in% reads_site$materialSampleID)

if(taxonLevel == "ASV"){

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
        filter(taxonRank=="species") %>%
        group_by(materialSampleID, scientificName) %>%
        summarize(reads = as.numeric(sum(organismQuantity))) %>%
        ungroup() %>%
        pivot_wider(names_from = scientificName, values_from = reads, values_fill = 0)

}

    stats_mds <- as.data.frame(stats_mds)
    rownames(stats_mds)<- stats_mds$materialSampleID
    stats_mds <- stats_mds[,-1]
    OTU <- as.matrix(stats_mds)

    estR <- as.data.frame(t(vegan::estimateR(OTU)))
    estR$shannon <- vegan::diversity(OTU, index="shannon")
    estR$simpson <- vegan::diversity(OTU, index="simpson")
    colnames(estR)<- c("Observed", "Chao1", "SE.Chao1", "ACE", "SE.ACE", "Shannon", "Simpson")
    estR$materialSampleID <- rownames(estR)

    estR <- estR %>% 
        left_join(occurrence %>% select(materialSampleID, locationID, decimalLatitude, decimalLongitude) %>% distinct())

    p1 <- ggplot(estR, aes(x=locationID, y=!!sym(alpha_div_measure), fill=locationID)) + 
                    geom_boxplot()+ 
                    geom_point() + 
                    theme_classic() +
                    #scale_fill_viridis(discrete=T, option="mako")
                    theme(axis.text.x = element_blank())+ 
                    scale_fill_brewer(palette = "Blues")

    #plots$alpha <- p1

    divR <- vegdist(OTU, method=beta_div_measure)

    PCOA <- pcoa(divR)
    PCOAaxes <- as.data.frame(PCOA$vectors[,c(1,2)])
    val1 <- paste0("PCoA1 (", round(PCOA$values$Relative_eig[1]*100, 2), "%)")
    val2 <- paste0("PCoA2 (", round(PCOA$values$Relative_eig[2]*100, 2), "%)")
    colnames(PCOAaxes) <- c(val1, val2)
    PCOAaxes$materialSampleID <- rownames(PCOAaxes)
    

    PCOAaxes <- PCOAaxes %>%
        left_join(occurrence %>% select(materialSampleID, locationID, decimalLatitude, decimalLongitude) %>% distinct())

    p2 <- ggplot(PCOAaxes, aes(x=!!sym(val1), y=!!sym(val2), fill=locationID, shape=locationID))+
                    geom_point(size=5)+
                    theme_classic() +
                    scale_shape_manual(values=c(21:26))+
                    scale_fill_brewer(palette = "Blues")

    list(p1, p2)

}



