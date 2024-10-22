List of inputs and outputs used on the Shiny application. Please keep this list updated for easier maintenaince of the app.

# Inputs

*home.qmd*

higherGeography - sites names

*taxonomy.qmd*

higherGeography - sites names
taxonLevel - species rank
plotType - type of plot
showPrimers - show or not primers

*species.qmd*

higherGeography - sites names
group - species group

*diversity.qmd*

higherGeography
taxonLevel - taxonomic rank
alpha_measure - alpha measurements
beta_measure - beta measurements



# Outputs

*home.qmd*

mainMap - `leaflet` output (top map)
higherGeography - `text` output (site title)
siteDescription - `text` output (site description)
eventDate - `text` output (date of sampling)
eventSamples - `text` output (number of events)
value_box_species - `text` output (value box)
value_box_fish - `text` output (value box)
value_box_mammals - `text` output (value box)
value_box_iucn - `text` output (value box)
imageGallery - `html` output (image gallery)

*taxonomy.qmd*

tax_bar_plot - `plot` output (taxons)
rtable - `react table` output (taxon)
tree_plot - `plot` output (taxonomic tree)
markers_plot - `plot` output (markers plot)

*species.qmd*

species_name - `text` output (name of species)
species_info - `data table` output (info of species)
species_gallery - `html` output (species image gallery)

*diversity.qmd*

alpha_plot - `plot` output (alpha diversity plot)
beta_plot - `plot` output (beta diversity plot)
