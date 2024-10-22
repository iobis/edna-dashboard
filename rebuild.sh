cd /srv/shiny-server/edna-dashboard
git pull
quarto render index.qmd
Rscript deploy.R