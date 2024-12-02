# suppressPackageStartupMessages(library(vegan))
# suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(ape))

#This script calculates and plots different diversity measures.
#In the future, calculate and plot separately, so the calculation is only done once? 
#To add also calculation of statistical significance of differences between localities?
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

get_alpha_diversity<-function(site, taxonLevel, alpha_div_measure, beta_div_measure){

    site <- cleanst(site)

    site_data <- readRDS(file.path("data/sites_data", paste0(site, "_divmetrics.rds")))

    site_data <- site_data[taxonLevel]

    estR <- site_data[[1]]$alpha

    PCOAaxes <- site_data[[1]]$beta
    PCOAaxes <- PCOAaxes[[beta_div_measure]]

    val1 <- colnames(PCOAaxes)[grep("PCoA1", colnames(PCOAaxes))]
    val2 <- colnames(PCOAaxes)[grep("PCoA2", colnames(PCOAaxes))]
 
    p1 <- ggplot(estR, aes(x = locationID, y = !!sym(alpha_div_measure), fill = locationID)) +
                geom_boxplot() +
                geom_point() +
                theme_classic() +
                # scale_fill_viridis(discrete=T, option="mako")
                theme(axis.text.x = element_blank()) +
                scale_fill_brewer(palette = "Blues")

    p2 <- ggplot(PCOAaxes, aes(x=!!sym(val1), y=!!sym(val2), fill=locationID, shape=locationID))+
                    geom_point(size=5)+
                    theme_classic() +
                    scale_shape_manual(values=c(21:26))+
                    scale_fill_brewer(palette = "Blues")

    list(p1, p2)

}



