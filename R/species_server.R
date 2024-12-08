# Species list gallery ----
species_list <- reactive({
    generate_ordered_species_list_bySite(input$higherGeography, occurrence_ds, input$group, input$iucn)
  })

w <- waiter::Waiter$new(
  id = "species_gallery",
  color = "white",
  html = htmltools::div(
    "Loading image gallery...",
    htmltools::br(), htmltools::br(),
    waiter::spin_1(), style = "color: #2780e3; font-size: 24px; font-weight: 900"
  )
)

wb <- waiter::Waiter$new(
  id = "species_gallery_b",
  color = "white",
  html = htmltools::div(
    "Loading more images...",
    # htmltools::br(), htmltools::br(),
    # waiter::spin_1(),
    style = "color: #2780e3; font-size: 24px; font-weight: 900"
  )
)

images_size <- reactive({
  width <- input$dimension[1]
  if (is.null(width)) {
    width <- 1100
  }

  container_width <- width - 115
  min_width <- 150
  max_width <- 230

  gap_width <- 0.1 * 16 

  min_images <- floor(container_width / (max_width + gap_width))
  max_images <- floor(container_width / (min_width + gap_width))

  best_size <- 0

  for (n in max_images:min_images) {
    image_width <- round(container_width / n)
    if (image_width >= min_width && image_width <= max_width) {
      best_size <- image_width
      break
    }
  }
  if (best_size == 0) {
    best_size <- 230
  }

  best_size - 2
})

sg_limit <- reactive({
  width <- input$dimension[1]
  if (is.null(width)) {
    width <- 1100
  }
  width <- width - 115
  maxw <- floor(width/images_size())
  maxw * 5
}) %>% bindEvent(images_size())

gallery_sp_list <- reactive({
  image_table <- data.table::fread("data/proc_images.txt")

  sel_species <- species_list()$scientificName
  existing_species <- image_table[match(sel_species, image_table$species, nomatch = 0),]

  existing_species$species
}) %>%
  bindEvent(species_list()$scientificName)

output$species_gallery <- renderUI({
    #req(length(species_list()$scientificName) > 0 && length(sg_limit()) > 0)

    if (input$higherGeography == "") {
      session$sendCustomMessage("second_gallery", "")
      htmltools::tags$br()
    } else if (length(species_list()$scientificName) > 0 && length(sg_limit()) > 0) {
      w$show()
      on.exit({
        w$hide()
      })

      if (length(gallery_sp_list()) > sg_limit()) {
        last_name <- "placeholder-tag"

        placeholders <- lapply(1:(sg_limit()/5), function(x) {
          if (x == 1) {
            card(height = "100%", full_screen = F, id = "placeholder-tag",
                card_header("Loading...", style = "font-style: italic;"),
                card_body(tags$img(src = "images/placeholders/general.webp",
                                    class = "gallery-img"), class= "p-0"),
                max_height = "250px", class = "loading"
                )
          } else {
            card(height = "100%", full_screen = F,
                card_header("Loading...", style = "font-style: italic;"),
                card_body(tags$img(src = "images/placeholders/general.webp",
                                    class = "gallery-img"), class= "p-0"),
                max_height = "250px", class = "loading"
                )
          }
        })

        htmltools::div(
          generate_gallery(gallery_sp_list()[1:sg_limit()], "data/proc_images.txt", max_image_height = "250px", max_image_width = paste0(images_size(), "px"), image_gap = "0.1em"),
          layout_column_wrap(id = "gallery-placeholder", width = paste0(images_size(), "px"), gap = "0.1em", fixed_width = TRUE, heights_equal = "row",
        !!!placeholders),
          htmltools::tags$script(
            htmltools::HTML(
              glue::glue(
                '
  $(window).on("scroll.checkElement", { passive: true }, function() {
    const element = $("#[last_name]");

    if (element.length) {
      const elementTop = element.offset().top;
      const elementBottom = elementTop + element.outerHeight();
      const viewportTop = $(window).scrollTop();
      const viewportBottom = viewportTop + $(window).height();

      if (elementBottom > viewportTop && elementTop < viewportBottom) {
        console.log("#your-element-id is in view!");
        
        var message = {id: "id1", nonce: Math.random()};
        Shiny.onInputChange("secondGallery", message);
        
        $(window).off("scroll.checkElement");
      }
    }
  });
              '
                , .open = "[", .close = "]"
              )
            )
          )
        )
      } else {
        session$sendCustomMessage("second_gallery", "")
        generate_gallery(gallery_sp_list(), "data/proc_images.txt", max_image_height = "250px",max_image_width = paste0(images_size(), "px"), image_gap = "0.1em")
      }
    } else {
      session$sendCustomMessage("second_gallery", "")
      htmltools::tags$br()
    }
}) %>%
  bindEvent(species_list()$scientificName)

observe({
  if (length(gallery_sp_list()) <= sg_limit()) {
    output$species_gallery_b <- renderUI({htmltools::tags$br()})
  } else {
    wb$show()
    on.exit(wb$hide())
    gallery_b <- generate_gallery(gallery_sp_list()[sg_limit():length(gallery_sp_list())], "data/proc_images.txt", max_image_height = "250px",max_image_width = paste0(images_size(), "px"), image_gap = "0.1em")

    output$species_gallery_b <- renderUI({gallery_b})

    removeUI(
      selector = "#gallery-placeholder"
    )
  }
}) %>%
  bindEvent(input$secondGallery, ignoreInit = TRUE, ignoreNULL = TRUE)



observe({
  if (!is.null(input$clicked_image_id)) {

    shiny::showModal(shiny::modalDialog(
      htmltools::tags$span("Loading species information...", style = "color: #0277d4; font-size: large;"),
      footer = NULL, easyClose = FALSE))

    species <- gsub("_", " ", gsub("link_", "", input$clicked_image_id))
    species_name <- species
    
    sel_aphiaid <- unique_sp_codes %>%
      ungroup() %>%
      filter(scientificName == species) %>%
      select(scientificNameID) %>%
      pull()

    sel_aphiaid <- sel_aphiaid[1] # To handle few cases with multiple, e.g. "Kyphosus vaigiensis"
    
    fb_content <- species_context_info$core_info %>% filter(Species == species)
    #fb_commons <- species_context_info$common_names %>% filter(Species == species)
    fb_commons <- vernacular_names[taxonID %in% sel_aphiaid, vernacularName]
    fb_commons <- as.vector(fb_commons)
    if (length(fb_commons) > 10) { # avoid very long list
      fb_commons <- fb_commons[1:10]
    }
    
    
    species_link_1 <- glue::glue('<a href="{paste0("https://obis.org/taxon/", sel_aphiaid)}", target="_blank">OBIS: {sel_aphiaid}</a>')
    species_link_2 <- glue::glue('<a href="{paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", sel_aphiaid)}", target="_blank">WoRMS: {sel_aphiaid}</a>')
    if (nrow(fb_content) > 0) {
      if (fb_content$source == "SeaLifeBase") {
      species_link_3 <- glue::glue('<a href="{paste0("https://www.sealifebase.org/summary/", gsub(" ", "-", species), ".html")}", target="_blank">SeaLifeBase: {fb_content$SpecCode[1]}</a>')
      } else if (fb_content$source == "FishBase") {
        species_link_3 <- glue::glue('<a href="{paste0("https://www.fishbase.org/summary/", gsub(" ", "-", species), ".html")}", target="_blank">FishBase: {fb_content$SpecCode[1]}</a>')
      } else {
        species_link_3 <- ""
      }
    } else {
      species_link_3 <- ""
    }
    
    sp_fb_common_names <- ifelse(length(fb_commons) > 0,
                                 paste(paste(fb_commons, collapse = ", "), "(Source: WoRMS)"), "not found")
    sp_fb_content <- ifelse(nrow(fb_content) < 1 || is.na(fb_content$Comments), "Additional information not available.",
                            paste(fb_content$Comments, glue::glue("(Source: {fb_content$source})")))

    species_info <- get_species_information(species, occurrence_ds, input$higherGeography)
  } else {
    species_name <- "Select a species to start."
    species_info <- ""
  }
  shiny::removeModal()
  shiny::showModal(
    species_gallery_modal(species_name, species_link_1, species_link_2, species_link_3, sp_fb_common_names, sp_fb_content)
  )

  ts <- max(sapply(species_info$DNA_sequence, nchar))

  output$species_info <- reactable::renderReactable({
    reactable::reactable(species_info, defaultPageSize = 7,
    resizable = TRUE, wrap = FALSE, bordered = TRUE, highlight = TRUE,
    striped = TRUE,
    columns = list(
     samples = colDef(name = "Samples"),
     localities = colDef(name = "Localities"), 
     target_gene = colDef(name = "Target gene", minWidth = 80),
     reads = colDef(name = "Reads", minWidth = 80),
     rdp_confidences = colDef(name = "RDP confidences", minWidth = 80),
     vsearch_identity = colDef(name = "VSEARCH identity", minWidth = 80),
     NCBI_ID = colDef(name = "NCBI ID", minWidth = 80),
     DNA_sequence = colDef(name = "DNA sequence",
       cell = function(value, index, name) {
        if (input$spi_dna_col & !input$spi_dna_back) {
          value <- gsub("A", '<span style="color: #5d1af6"; font-weight: bold;>A</span>', value)
          value <- gsub("T", '<span style="color: #f6d11a"; font-weight: bold;>T</span>', value)
          value <- gsub("G", '<span style="color: #0eab72"; font-weight: bold;>G</span>', value)
          value <- gsub("C", '<span style="color: #c50936"; font-weight: bold;>C</span>', value)
        } else if (input$spi_dna_back) {
          value <- gsub("A", '<span style="color: white; background-color: #5d1af6"; font-weight: bold;>A</span>', value)
          value <- gsub("T", '<span style="color: white; background-color: #f6d11a"; font-weight: bold;>T</span>', value)
          value <- gsub("G", '<span style="color: white; background-color: #0eab72"; font-weight: bold;>G</span>', value)
          value <- gsub("C", '<span style="color: white; background-color: #c50936"; font-weight: bold;>C</span>', value)
        }
             htmltools::HTML(value)
  }, minWidth = (ts*10), html = TRUE)),
  height = 260, theme = reactableTheme(
    backgroundColor = "rgba(0, 0, 0, 0)" # remove this line if want the table white
    ))
  })

}) %>%
  bindEvent(input$clicked_image_id, ignoreInit = T)