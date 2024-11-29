cd /srv/shiny-server/edna-dashboard
git checkout main
git pull
quarto render index.qmd
chmod -R 755 index_files
Rscript deploy.R
