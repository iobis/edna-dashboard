cd /srv/shiny-server/edna-dashboard
git checkout main
git pull
quarto render index.qmd
Rscript deploy.R