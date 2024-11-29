source("R/taxonomy_figures.R")
source("R/occurrence.R")

message("Updating Krona plots...")

read_occurrence_data() |>
  make_krona_plot()
