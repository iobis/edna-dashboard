source("R/taxonomy_figures.R")
source("R/occurrence.R")

read_occurrence_data() |> make_krona_plot()
