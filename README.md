# eDNA dashboard
 
From RStudio: with `index.qmd` open you can simply click render or from the Build panel click 'Render Project'

Using the package: `quarto::quarto_render()` or `quarto::quarto_preview()`

From the command line: `quarto render` and then `quarto preview`

## Post-render code

After rendering the page, the code `post-render.R` is executed. This code adds the icons on the navbar. To change the icons, change this file.