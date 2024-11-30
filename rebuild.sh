cd /srv/shiny-server/edna-dashboard
git checkout main
git pull
quarto render index.qmd
chmod -R 755 index_files
ls -al index_files
Rscript prepare_data.R
