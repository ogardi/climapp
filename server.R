###############################################################################
# CARITAS-CDA: Shiny-App Server
# =============================================================================
# 
# Author: oliver.gardi@bfh.ch
# Date:   February 2022
# 
# rsconnect::deployApp()

source("functions.R")
source("ui.R")

options(shiny.maxRequestSize=60*1024^2)

server <- function(input, output, session) {
  
  # Get data for location =====================================================

  loc <- reactiveValues(name = NULL,
                        lat  = NULL,
                        lon  = NULL,
  #                     crops = NULL,
                        clim = NULL)

  # load location
  observeEvent(input$loc.load, {
    showModal(modalDialog(
      h3("Load a previously saved location:"), br(),
      fileInput("loc.load.file", "Location File (.zip)",
                multiple = FALSE,
                accept = ".zip"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("loc.load.go", "Load", class="btn btn-primary")
      )
    ))
  })

  observeEvent(input$loc.load.go, {
    if (is.null(input$loc.load.file)) return()
    print(input$loc.load.file$datapath)
    loc[["dir"]] <- tempfile()
    print(loc[["dir"]])
    unzip(input$loc.load.file$datapath, exdir = loc[["dir"]])
    location <- readRDS(paste0(loc[["dir"]], "/location.rds"))
    loc[["name"]] <- location[["name"]]
    loc[["lat"]] <- location[["lat"]]
    loc[["lon"]] <- location[["lon"]]
  #  loc[["crops"]] <- location[["crops"]]# readRDS(paste0(loc[["dir"]], "/crops.rds"))
    id <- notify(paste0("Loading ", loc$name, " ..."))
    on.exit(removeNotification(id), add = TRUE)
    loc[["clim"]] <- normalize.data(load.data(loc[["dir"]], loc[["lat"]], loc[["lon"]], id))
    removeModal()
  })

  # new location
  observeEvent(input$loc.new, {
    showModal(modalDialog(
      h3("Create a new location:"),
      fluidRow(
        column(6, textInput("loc.new.name", "Location", placeholder = "Name")),
        column(3, textInput("loc.new.lat", "Lat", placeholder = "Lat")),
        column(3, textInput("loc.new.lon", "Lon", placeholder = "Lon"))
      ),
      div("Extracting and downloading data from KNMI Explorer can take up to 30 minutes"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("loc.new.go", "Create", class="btn btn-primary")
      )
    ))
  })

  observeEvent(input$loc.new.go, {
    loc[["name"]] <- input$loc.new.name
    loc[["lat"]] <- input$loc.new.lat
    loc[["lon"]] <- input$loc.new.lon
    loc[["dir"]] <- tempfile()
    id <- notify(paste0("Creating ", loc$name, " ..."))
    on.exit(removeNotification(id), add = TRUE)
    loc[["clim"]] <- normalize.data(load.data(loc[["dir"]], loc$lat, loc$lon, id))
 #   loc[["crops"]] <- list()
    removeModal()
  })

  # save location
  output$loc.save <- downloadHandler(
    filename = function() {
      paste(loc[["name"]], ".zip", sep = "")
    },
    content = function(file) {
      saveRDS(list("name" = loc[["name"]], "lat" = loc[["lat"]], "lon" = loc[["lon"]]), paste0(loc[["dir"]], "/location.rds"))
  #   saveRDS(loc[["crops"]], paste0(loc[["dir"]], "/crops.rds"))
      wd <- getwd()
      setwd(loc[["dir"]])
      zip(file, list.files(recursive=TRUE, full.names = TRUE))
      setwd(wd)
    }
  )


  # put location name in title
  output$location <- renderText({
    if(is.null(loc$name)) {
      "None - please specify a location"
    } else {
      paste0(loc$name, " (", loc$lon, "E, ", loc$lat, "N)")
    }
  })



  # Climographs ===============================================================

  # Left Panel
  output$clim.1.plot <- renderPlot({
    req(loc[["clim"]])
    plot.climograph(loc[["clim"]], var=input$clim.1.var, set=strsplit(input$clim.1.set_sce, "_")[[1]][1], sce=strsplit(input$clim.1.set_sce, "_")[[1]][2],
                    title=paste0(input$clim.1.var, "_", input$clim.1.set_sce, " / ", loc[["name"]], " (", loc[["lon"]], "E ", loc[["lat"]], "N)"))
  })

  # Right Panel
  output$clim.2.plot <- renderPlot({
    req(loc[["clim"]])
    plot.climograph(loc[["clim"]], var=input$clim.2.var, set=strsplit(input$clim.2.set_sce, "_")[[1]][1], sce=strsplit(input$clim.2.set_sce, "_")[[1]][2],
                    title=paste0(input$clim.1.var, "_", input$clim.1.set_sce, " / ", loc[["name"]], " (", loc[["lon"]], "E ", loc[["lat"]], "N)"))
  })

  # Climate diagram ===============================================================

  # Left Panel
  output$diag.1.plot <- renderPlot({
    req(loc[["clim"]])
    plot.walterlieth(loc[["clim"]], set=strsplit(input$diag.1.set_sce, "_")[[1]][1], sce=strsplit(input$diag.1.set_sce, "_")[[1]][2],
                    title=paste0(input$diag.1.var, "_", input$diag.1.set_sce, " / ", loc[["name"]], " (", loc[["lon"]], "E ", loc[["lat"]], "N)"))
  })

  # Right Panel
  output$diag.2.plot <- renderPlot({
    req(loc[["clim"]])
    plot.walterlieth(loc[["clim"]], set=strsplit(input$diag.2.set_sce, "_")[[1]][1], sce=strsplit(input$diag.2.set_sce, "_")[[1]][2],
                    title=paste0(input$diag.1.var, "_", input$diag.1.set_sce, " / ", loc[["name"]], " (", loc[["lon"]], "E ", loc[["lat"]], "N)"))
  })


  # Time series  ==============================================================

  tser.1.period <- reactiveValues(start = "Jan", end = "Dec")

  # Left Panel
  observeEvent(input$tser.1.update, {
    tser.1.period$start <- input$tser.1.start
    tser.1.period$end <- input$tser.1.end
  })

  output$tser.1.plot <- renderPlot({
    req(loc[["clim"]])
    plot.timeseries(loc[["clim"]], var=input$tser.1.var, set.obs="cru4", set.mod="cmip6", norm=TRUE,
                    period=month.seq(tser.1.period$start, tser.1.period$end),
                    title=paste0("cmip6/cru4_", input$tser.1.var, " - ", loc$name, " - ", tser.1.period$start, ":", tser.1.period$end))
  })

  # Right Panel
  tser.2.period <- reactiveValues(start = "Jan", end = "Dec")

  # Left Panel
  observeEvent(input$tser.2.update, {
    tser.2.period$start <- input$tser.2.start
    tser.2.period$end <- input$tser.2.end
  })

  output$tser.2.plot <- renderPlot({
    req(loc[["clim"]])
    plot.timeseries(loc[["clim"]], var=input$tser.2.var, set.obs="cru4", set.mod="cmip6", norm=TRUE,
                    period=month.seq(tser.2.period$start, tser.2.period$end),
                    title=paste0("cmip6/cru4_", input$tser.2.var, " - ", loc$name, " - ", tser.2.period$start, ":", tser.2.period$end))
  })


  # Crop specification ========================================================

  crops <- reactiveValues(list = list())

  # get ECOCROP from input
  crop <- reactive({
    cr <- new("ECOCROPcrop")
    cr@code <- input$crop.profit      # misuse of slot code for storing profit!
    cr@GMIN <- input$crop.gdur[1]
    cr@GMAX <- input$crop.gdur[2]
    cr@KTMP <- 0
    # cr@TMIN <- input$crop.tmin[1]
    # cr@TOPMN <- input$crop.tmin[2]
    # cr@TOPMX <- input$crop.tmax[1]
    # cr@TMAX <- input$crop.tmax[2]
    # cr@RMIN <- input$crop.pmin[1]
    # cr@ROPMN <- input$crop.pmin[2]
    # cr@ROPMX <- input$crop.pmax[1]
    # cr@RMAX <- input$crop.pmax[2]
    cr@TMIN <- input$crop.tlim[1]
    cr@TOPMN <- input$crop.topt[1]
    cr@TOPMX <- input$crop.topt[2]
    cr@TMAX <- input$crop.tlim[2]
    cr@RMIN <- input$crop.plim[1]
    cr@ROPMN <- input$crop.popt[1]
    cr@ROPMX <- input$crop.popt[2]
    cr@RMAX <- input$crop.plim[2]
    cr
  })

  crop.name <- reactiveVal("Maize (Default)")
  output$crop.name = renderText(crop.name())

   # Loading crop list ----------------------------
  observeEvent(input$crops.load, {
    showModal(modalDialog(
     h3("Load a previously saved crop definitions:"), br(),
      fileInput("crops.load.file", "Crop list (.rds)",
                multiple = FALSE,
                accept = ".rds"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("crops.load.go", "Load", class="btn btn-primary")
      )
    ))
  })

  observeEvent(input$crops.load.go, {
    crps <- readRDS(input$crops.load.file$datapath)
    for(cr in crps) {
      crops$list[[cr@name]] <- cr
    }
    updateSelectInput(session, "crops.list", choices=names(crops$list))
    updateSelectInput(session, "ccor.1.crop", choices=names(crops$list))
    updateSelectInput(session, "ccor.2.crop", choices=names(crops$list))
    updateSelectInput(session, "ecind.1.crop", choices=names(crops$list))
    updateSelectInput(session, "ecind.2.crop", choices=names(crops$list))
    removeModal()
  })

   # save crop list -----------------------
  output$crops.save <- downloadHandler(
    filename = function() {
      paste0(loc[["name"]], "_crops.rds")
    },
    content = function(file) {
      saveRDS(crops$list, file)
    }
  )

  # load selected crop
  observeEvent(input$crops.list, {
    cr <- crops$list[[input$crops.list]]
    crop.name(cr@name)
    updateNumericInput(session, "crop.profit", value=cr@code)
    updateSliderInput(session, "crop.gdur", value=c(cr@GMIN, cr@GMAX))
    # updateSliderInput(session, "crop.tmin", value=c(cr@TMIN, cr@TOPMN))
    # updateSliderInput(session, "crop.tmax", value=c(cr@TOPMX, cr@TMAX))
    # updateSliderInput(session, "crop.pmin", value=c(cr@RMIN, cr@ROPMN))
    # updateSliderInput(session, "crop.pmax", value=c(cr@ROPMX, cr@RMAX))
    updateSliderInput(session, "crop.topt", value=c(cr@TOPMN, cr@TOPMX))
    updateSliderInput(session, "crop.tlim", value=c(cr@TMIN, cr@TMAX))
    updateSliderInput(session, "crop.popt", value=c(cr@ROPMN, cr@ROPMX))
    updateSliderInput(session, "crop.plim", value=c(cr@RMIN, cr@RMAX))
  }, ignoreInit = TRUE)

  # Adding crop to crops list
  observeEvent(input$crop.add, {
    showModal(modalDialog(
      h3("Save crop as:"),
      textInput("crop.add.name", NULL, placeholder = "Crop Name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("crop.add.go", "Add", class="btn btn-primary")
      )
    ))
  })

  observeEvent(input$crop.add.go, {
    cr <- crop()
    cr@name <- input$crop.add.name
    crops$list[[cr@name]] <- cr
    updateSelectInput(session, "crops.list", choices=names(crops$list), selected=cr@name)
    updateSelectInput(session, "ccor.1.crop", choices=names(crops$list), selected=cr@name)
    updateSelectInput(session, "ccor.2.crop", choices=names(crops$list), selected=cr@name)
    updateSelectInput(session, "ecind.1.crop", choices=names(crops$list), selected=cr@name)
    updateSelectInput(session, "ecind.2.crop", choices=names(crops$list), selected=cr@name)
    removeModal()
  })

  # Deleting current crop (if it exists in the list)
  observeEvent(input$crop.del, {
    crops$list[[crop.name()]] <- NULL
    updateSelectInput(session, "crops.list", choices=names(crops$list))
    updateSelectInput(session, "ccor.1.crop", choices=names(crops$list))
    updateSelectInput(session, "ccor.2.crop", choices=names(crops$list))
    updateSelectInput(session, "ecind.1.crop", choices=names(crops$list))
    updateSelectInput(session, "ecind.2.crop", choices=names(crops$list))
  })


  # Plotting crop suitability
  output$crop.specification <- renderPlot(plot.crop(crop(), crop.name()))


   # Ecocrop Parameters ========================================================

  output$ecocrop.params <- renderReactable({
    reactable(ecocrops,
              searchable=TRUE, pagination = FALSE, highlight = TRUE, height=600,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "12px"),
              selection = "single",
              onClick = "select",
              defaultColDef = colDef(minWidth = 60),
              columns = list(
                .selection = colDef(show = FALSE),
                NAME = colDef(minWidth = 140),
                FAMNAME = colDef(minWidth = 110),
                SCIENTNAME = colDef(minWidth = 180)
              )
    )
  })

  selectedRow <- reactive(reactable::getReactableState("ecocrop.params", "selected"))

  observeEvent(selectedRow(), {
    print(selectedRow())
    cr <- ecocrops[selectedRow(),]
    updateTextInput(session, "crop.name", value=paste(cr$NAME, "(Default)"))
    updateNumericInput(session, "crop.profit", value=0)
    updateSliderInput(session, "crop.gdur", value=c(cr$GMIN, cr$GMAX))
    # updateSliderInput(session, "crop.tmin", value=c(cr$TMIN, cr$TOPMN))
    # updateSliderInput(session, "crop.tmax", value=c(cr$TOPMX, cr$TMAX))
    # updateSliderInput(session, "crop.pmin", value=c(cr$RMIN, cr$ROPMN))
    # updateSliderInput(session, "crop.pmax", value=c(cr$ROPMX, cr$RMAX))
    updateSliderInput(session, "crop.topt", value=c(cr$TOPMN, cr$TOPMX))
    updateSliderInput(session, "crop.tlim", value=c(cr$TMIN, cr$TMAX))
    updateSliderInput(session, "crop.popt", value=c(cr$ROPMN, cr$ROPMX))
    updateSliderInput(session, "crop.plim", value=c(cr$RMIN, cr$RMAX))
  })
  
  # Ecocrop indices ==========================================================

  # Left Panel
  output$ecind.1.plot <- renderPlot({
    req(loc[["clim"]])
    plot.ecocrop(loc[["clim"]], crops$list[[input$ecind.1.crop]])
  })

  # Right Panel
  output$ecind.2.plot <- renderPlot({
    req(loc[["clim"]])
    plot.ecocrop(loc[["clim"]], crops$list[[input$ecind.2.crop]])
  })

  # Climate corridors ==========================================================

  # Left Panel
  output$ccor.1.plot <- renderPlot({
    req(loc[["clim"]])
    plot.ccor(loc[["clim"]], crops$list[[input$ccor.1.crop]], var=input$ccor.1.var, set.obs="cru4",
                    set.mod=strsplit(input$ccor.1.sce, "_")[[1]][1], sce=strsplit(input$ccor.1.sce, "_")[[1]][2])
  })

  # Right Panel
  output$ccor.2.plot <- renderPlot({
    req(loc[["clim"]])
    plot.ccor(loc[["clim"]], crops$list[[input$ccor.2.crop]], var=input$ccor.2.var, set.obs="cru4",
                    set.mod=strsplit(input$ccor.2.sce, "_")[[1]][1], sce=strsplit(input$ccor.2.sce, "_")[[1]][2])
  })

}
  

# shinyApp(ui, server)