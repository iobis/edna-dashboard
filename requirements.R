################################ eDNA dashboard ################################
# September of 2024
# Authors: Saara Suominen, Pieter Provoost, Silas C. Principe
# Contact: s.principe@unesco.org
############################ Project Requirements ##############################

# Needed packages on CRAN
req_packages <- c(
  "devtools",
  "tidyr",
  "arrow",
  "ggplot2",
  "dplyr",
  "terra",
  "plotly",
  "dygraphs",
  "waiter",
  "reactable",
  "DT",
  "eulerr",
  "data.tree",
  "viridisLite",
  "BiocManager"
)

bioconductor_packages <- c("ggtree", "ggtreeExtra", "phyloseq")

# Needed packages on GitHub
git_packages <- c("psadd")
git_packages_source <- c(
  "cpauvert/psadd"
)

# Create a function to check if is installed
is_package_installed <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# Check which ones are not installed and install if needed:
for (i in 1:length(req_packages)) {
  if (!is_package_installed(req_packages[i])) {
    install.packages(req_packages[i])
  }
}

# Check github packages
for (i in 1:length(git_packages)) {
  if (!is_package_installed(git_packages[i])) {
    devtools::install_github(git_packages_source[i])
  }
}

# Check bioconductor packages
for (i in 1:length(bioconductor_packages)) {
  if (!is_package_installed(bioconductor_packages[i])) {
    BiocManager::install(bioconductor_packages[i])
  }
}
