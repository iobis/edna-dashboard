cd /srv/shiny-server/edna-dashboard
git checkout main
git fetch
git reset --hard origin/main
quarto render index.qmd
chmod -R 755 index_files
ls -al index_files
Rscript prepare_data.R
