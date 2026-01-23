###############################################################################
# CARITAS-ClimApp: Shiny-App
# =============================================================================
# 
# Author: oliver.gardi@gmail.com
# Date:   December 2023
# 
# Konfigurieren von rsconnect: https://shiny.posit.co/r/articles/share/shinyapps/
# rsconnect::deployApp(appFileManifest="appFileManifest.txt", appName="climapp")
# rsconnect::showLogs(streaming = TRUE)

# options(shiny.fullstacktrace = TRUE)

library(shiny)
options(shiny.maxRequestSize=10*1024^2) # enable file upload sizes of up to 10 MB
library(promises)          # for async tasks with multiple users
library(future)            # for async tasks with multiple users
plan("multicore", workers=4)
library(shinyjs)           # enabling/disabling inputfields
library(shinycssloaders)   # spinner
library(shinyBS)           # tooltips
library(shiny.i18n)        # multilingual
translator <- Translator$new(translation_json_path="translation.json")
library(shinydisconnect)   # Error message if shiny server is disconnected
library(cachem)            # for caching to disk
shinyOptions(cache = cachem::cache_disk("./cache/"))

library(shinyvalidate)

library(shinymanager)      # for login / authentication
credentials <- data.frame(
  user = c("CACH", "Guest", "Extern"),
  password = c("climApp", "climApp", "climApp"),
  admin = c(FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

library(reactable)
library(purrr)             # for set_names used for translation in UI
library(plotly)            # for interactive plots

load("./ECOcrops.RData")
ecocrops <- ECOcrops[order(ECOcrops$FAMNAME, ECOcrops$SCIENTNAME), c(1:3, 5:6, 8:11, 12:15)]

# Resolution for figures
default.res <- 96
  
# library(berryFunctions)  # climateGraph Walter-Lieth

# UI rendered on server-side (needed for translation)
ui <- secure_app(
      tags_top = tags$h1("CARITAS climApp", style="color:#DB001BFF;"),
        tags_bottom = tags$div(
          tags$p("To get access to the Caritas climApp, please contact ",
          tags$a(
            href = "mailto:mclaudelin@caritas.ch?Subject=ClimApp%20Access",
            target="_top", "mclaudelin@caritas.ch"
          ))
        ),
      # https://developer.mozilla.org/fr/docs/Web/CSS/background
      background  = "bottom/100% no-repeat url('background.jpg');",
      #           linear-gradient(rgba(219, 0, 27, 1), rgba(255, 255, 255, 1));",
      fluidPage(
        disconnectMessage(),
        useShinyjs(),
        uiOutput('page_content')
      )
)

server <- function(input, output, session) {
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # reactive translator, reacting to changes in language
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })
  
  iv <- InputValidator$new()
  iv$add_rule("loc.new.lon", compose_rules(sv_required("-180.0—180.0ºE"), sv_regex("^[-]?(([0-9]|[1-9][0-9]|1[0-7][0-9])([.][0-9]*)?|(180)([.]0*)?)$", message = "-180.0—180.0ºE")))
  iv$add_rule("loc.new.lat", compose_rules(sv_required("-90.0—90.0ºN"), sv_regex("^[-]?(([0-9]|[1-8][0-9])([.][0-9]*)?|(90)([.]0*)?)$", message = "-90.0—90.0ºN")))
  # compose_rules mit sv_required is workaround, da sonst: "Warning: Error in if: argument is of length zero" (ohne Konsequenzen)
  iv$enable()
  
  # source code for rendering UI: output$page_content <- renderUI(...)
  source("./renderUI.R", local=TRUE)
  
  # LOCATION =====================================================

  # initialize loc with default location
  default_location <- readRDS("default_location.rds")  # Lucerne
  loc <- reactiveValues(name = default_location$name,
                        lat  = default_location$lat,
                        lon  = default_location$lon,
                        clim = default_location$clim,
                        dir  = NULL)
  
  # create a new location based on coordinates ...
  observeEvent(input$loc.new, {
    showModal(modalDialog(
      h3(i18n()$t("loc:new:title")),
      fluidRow(
        column(6, textInput("loc.new.name", i18n()$t("loc:new:name"), placeholder = i18n()$t("gen:name:label"))),
        column(3, textInput("loc.new.lat", "Lat", placeholder = "Lat")),
        column(3, textInput("loc.new.lon", "Lon", placeholder = "Lon"))
      ),
      div(HTML(i18n()$t("loc:new:warning-1"))),
      footer = tagList(
        modalButton(i18n()$t("gen:cancel")),
        actionButton("loc.new.go", paste(i18n()$t("gen:create"), "..."), class="btn btn-primary")
      )
    ))
  })

  # ... and launch it in the background (future), once the GO button is pressed
  observeEvent(input$loc.new.go, {
    req(iv$is_valid())
    removeModal()
    showPageSpinner(caption=div(strong(paste0(i18n()$t("loc:new:spinner"), " ", input$loc.new.name, " (", round(as.numeric(input$loc.new.lat), 1), "N, ", round(as.numeric(input$loc.new.lon), 1), "E) ...")), br(), em(HTML(i18n()$t("loc:new:warning-2")))))
    future_promise({
      new.loc <- list()
      new.loc$name <- isolate(input$loc.new.name)   # isolate the provided value (not reactive anymore!)
      new.loc$lat  <- round(as.numeric(isolate(input$loc.new.lat)), 1)
      new.loc$lon  <- round(as.numeric(isolate(input$loc.new.lon)), 1)
      new.loc$dir  <- download.data(new.loc$lat, new.loc$lon, id)
      new.loc$clim <- normalize.data(load.data(new.loc$dir))
      new.loc
    }) %...>% (function(new.loc){             # update location once the new location is created
      loc$name <- new.loc$name
      loc$lat  <- new.loc$lat
      loc$lon  <- new.loc$lon
      loc$clim <- new.loc$clim
      loc$dir  <- new.loc$dir
      hidePageSpinner()
      showModal(modalDialog(
        h3(i18n()$t("loc:new:save:title")),
        fluidRow(column(12, textInput("loc.save.name", NULL, value = paste0(new.loc$name, ".zip")))),
        div(i18n()$t("loc:new:save:message")),
        footer = tagList(
          modalButton(i18n()$t("gen:cancel")),
          downloadButton("loc.new.save", paste(i18n()$t("gen:download"), "..."), class="btn btn-primary")
        )
      ))
    })
  })
    
  # save location (see here how automated download could be implemented when a new location is created)
  # https://stackoverflow.com/questions/54652364/r-shiny-automatically-start-download
  output$loc.new.save <- downloadHandler(
    filename = function() {
      input$loc.save.name
    },
    content = function(file) {
      removeModal()
      showPageSpinner(caption=div(strong(paste(i18n()$t("loc:new:download:spinner-1"), input$loc.save.name, i18n()$t("loc:new:download:spinner-2")))))
      saveRDS(list("name" = loc$name, "lat" = loc$lat, "lon" = loc$lon), paste0(loc$dir, "/location.rds"))
      wd <- getwd()
      setwd(loc$dir)
      zip(file, list.files(recursive=TRUE, full.names = TRUE))
      setwd(wd)
      hidePageSpinner()
    }
  )

  # upload file for loading a previously saved location ...
  observeEvent(input$loc.load, {
    showModal(modalDialog(
      h3(i18n()$t("loc:load:title")), br(),
      fileInput("loc.load.file", i18n()$t("loc:load:file"),
                buttonLabel = paste(i18n()$t("gen:browse"), "..."),
                multiple = FALSE,
                accept = ".zip"),
      footer = tagList(
        modalButton(i18n()$t("gen:cancel")),
      )
    ))
  })

  # ... and update loc, once the file has been uploaded
  observeEvent(input$loc.load.file, {
    removeModal()
    if (is.null(input$loc.load.file)) return()
    showPageSpinner(caption=div(strong(paste0(i18n()$t("loc:load:spinner"), " ", input$loc.load.file$name, " ..."))))
    tmpdir <- tempfile() # temporary directory for extracting data
    unzip(input$loc.load.file$datapath, exdir = tmpdir)
    new.loc <- readRDS(paste0(tmpdir, "/location.rds"))
    loc$name <- new.loc$name
    loc$lat  <- new.loc$lat
    loc$lon  <- new.loc$lon
    loc$clim <- normalize.data(load.data(tmpdir, id))
    loc$dir  <- tmpdir
    hidePageSpinner()
  })

  # put location name in title
  output$location <- renderText({
      paste0(loc$name, " (", loc$lat, "N, ", loc$lon, "E)")
  })


  # Tab 1: Time series  ==============================================================

  # Left Panel
  tser.1.period <- reactiveValues(start = "Jan", end = "Dec")

  observeEvent(input$tser.1.update, {
    tser.1.period$start <- input$tser.1.start
    tser.1.period$end <- input$tser.1.end
  })
  
  output$tser.1.plot <- renderCachedPlot({
    req(loc$clim)
    gp <- plot.timeseries(loc$clim, var=input$tser.1.var, set.obs="cru4", set.mod="cmip6", norm=TRUE, period=month.seq(tser.1.period$start, tser.1.period$end))
    if(input$tser.1.var == "tas") {
      gp <- gp + labs(title = paste0(i18n()$t("tser:fig:temp:title"), " — ", i18n()$t(tser.1.period$start), ":", i18n()$t(tser.1.period$end), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("tser:fig:temp:ylab"), x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6")
    } else {
      gp <- gp + labs(title = paste0(i18n()$t("tser:fig:prec:title"), " — ", i18n()$t(tser.1.period$start), ":", i18n()$t(tser.1.period$end), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("tser:fig:prec:ylab"), x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6")
    }
    gp
  },
  cacheKeyExpr = {
    list(loc$clim, input$tser.1.var, tser.1.period$start, tser.1.period$end, input$lang)
  }, res = default.res)

  # output$tser.1.plot <- renderPlotly({
  #   req(loc$clim)
  #   gp <- plot.timeseries(loc$clim, var=input$tser.1.var, set.obs="cru4", set.mod="cmip6", norm=TRUE, period=month.seq(tser.1.period$start, tser.1.period$end))
  #   if(input$tser.1.var == "tas") {
  #     gp <- gp + labs(title = paste0(i18n()$t("tser:fig:temp:title"), " — ", i18n()$t(tser.1.period$start), ":", i18n()$t(tser.1.period$end), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
  #                     y = i18n()$t("tser:fig:temp:ylab"), x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6")
  #   } else {
  #     gp <- gp + labs(title = paste0(i18n()$t("tser:fig:prec:title"), " — ", i18n()$t(tser.1.period$start), ":", i18n()$t(tser.1.period$end), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
  #                     y = i18n()$t("tser:fig:prec:ylab"), x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6")
  #   }
  #   
  #   gp <- ggplotly(gp) %>% layout(title =list(font=list(size=14)),
  #                          legend=list(title=list(font=list(size=12)), font=list(size=10)),
  #                          xaxis = list(title=list(font = list(size = 12)), tickfont=list(size = 10)),
  #                          yaxis = list(title=list(font = list(size = 12)), tickfont=list(size = 10)))
  #   gp$x$data[[1]]$showlegend <- FALSE
  #   gp$x$data[[2]]$showlegend <- FALSE
  #   gp$x$data[[3]]$showlegend <- FALSE
  #   gp$x$data[[4]]$showlegend <- FALSE
  #   gp$x$data[[5]]$showlegend <- TRUE
  #   gp$x$data[[6]]$showlegend <- TRUE
  #   gp$x$data[[7]]$showlegend <- TRUE
  #   gp$x$data[[8]]$showlegend <- TRUE
  #   
  #   gp %>% plotly::config(
  #     displaylogo = FALSE,
  #     modeBarButtons = list(list("toImage"), list(dl_button)),
  #     toImageButtonOptions = list(
  #       format = "png",
  #       width = 960,
  #       height = 540,
  #       scale = 2
  #     )
  #   )
  # }) %>% bindCache(loc$clim, input$tser.1.var, tser.1.period$start, tser.1.period$end, input$lang)
  

  # Right Panel
  tser.2.period <- reactiveValues(start = "Jan", end = "Dec")

  observeEvent(input$tser.2.update, {
    tser.2.period$start <- input$tser.2.start
    tser.2.period$end <- input$tser.2.end
  })

  output$tser.2.plot <- renderCachedPlot({
    req(loc$clim)
    gp <- plot.timeseries(loc$clim, var=input$tser.2.var, set.obs="cru4", set.mod="cmip6", norm=TRUE, period=month.seq(tser.2.period$start, tser.2.period$end))
    if(input$tser.2.var == "tas") {
      gp <- gp + labs(title = paste0(i18n()$t("tser:fig:temp:title"), " — ", i18n()$t(tser.1.period$start), ":", i18n()$t(tser.1.period$end), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("tser:fig:temp:ylab"), x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6")
    } else {
      gp <- gp + labs(title = paste0(i18n()$t("tser:fig:prec:title"), " — ", i18n()$t(tser.1.period$start), ":", i18n()$t(tser.1.period$end), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("tser:fig:prec:ylab"), x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6")
    }
    gp
  },
  cacheKeyExpr = {
    list(loc$clim, input$tser.2.var, tser.2.period$start, tser.2.period$end, input$lang)
  }, res = default.res)

  # TAB 2: Climographs ===============================================================

  # Left Panel
  output$clim.1.plot <- renderCachedPlot({
    req(loc$clim)
    gp <- plot.climograph(loc$clim, var=input$clim.1.var, set=strsplit(input$clim.1.set_sce, "_")[[1]][1], sce=strsplit(input$clim.1.set_sce, "_")[[1]][2])
    if(input$clim.1.var == "tas") {
      gp <- gp + labs(title = paste0(i18n()$t("clim:fig:temp:title"), " — ", i18n()$t(input$clim.1.set_sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("clim:fig:temp:ylab"), x = "", colour = i18n()$t("gen:year:label"), caption = "Ref: cru4/cmip6")
    } else {
      gp <- gp + labs(title = paste0(i18n()$t("tser:fig:prec:title"), " — ", i18n()$t(input$clim.1.set_sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("clim:fig:prec:ylab"), x = "", colour = i18n()$t("gen:year:label"), caption = "Ref: cru4/cmip6")
    }
    gp
  },
  cacheKeyExpr = {
    list(loc$clim, input$clim.1.var, input$clim.1.set_sce, input$lang)
  }, res = default.res)

  # Right Panel
  output$clim.2.plot <- renderCachedPlot({
    req(loc$clim)
    gp <- plot.climograph(loc$clim, var=input$clim.2.var, set=strsplit(input$clim.2.set_sce, "_")[[1]][1], sce=strsplit(input$clim.2.set_sce, "_")[[1]][2])
    if(input$clim.2.var == "tas") {
      gp <- gp + labs(title = paste0(i18n()$t("clim:fig:temp:title"), " — ", i18n()$t(input$clim.2.set_sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("clim:fig:temp:ylab"), x = "", colour = i18n()$t("gen:year:label"), caption = "Ref: cru4/cmip6")
    } else {
      gp <- gp + labs(title = paste0(i18n()$t("tser:fig:prec:title"), " — ", i18n()$t(input$clim.2.set_sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("clim:fig:prec:ylab"), x = "", colour = i18n()$t("gen:year:label"), caption = "Ref: cru4/cmip6")
    }
    gp
  },
  cacheKeyExpr = {
    list(loc$clim, input$clim.2.var, input$clim.2.set_sce, input$lang)
  }, res = default.res)

  # TAB 3: Climate diagram ===============================================================
# 
#   # Left Panel
#   
#   output$diag.1.plot <- renderCachedPlot({
#     req(loc$clim)
#     plot.walterlieth(loc$clim, set="cru4", period=input$diag.1.period:(as.numeric(input$diag.1.period)+29),
#                     location = loc$name, lat=loc$lat, lon=loc$lon)
#   },
#   cacheKeyExpr = {
#     list(loc$clim, input$diag.1.period, input$clim.1.set_sce)
#   }, res = default.res)
# 
#   # Right Panel
#   output$diag.2.plot <- renderCachedPlot({
#     req(loc$clim)
#     plot.walterlieth(loc$clim, set=strsplit(input$diag.2.set_sce, "_")[[1]][1], sce=strsplit(input$diag.2.set_sce, "_")[[1]][2], period=input$diag.2.period:(as.numeric(input$diag.2.period)+29),
#                     location = loc$name, lat=loc$lat, lon=loc$lon)
#   },
#   cacheKeyExpr = {
#     list(loc$clim, input$diag.2.set_sce, input$diag.2.period)
#   }, res = default.res)
  
  # TAB 4: Crop specification ========================================================

  # list of defined crops (with Maize as default crop) as reactive Values
  crops <- lapply(readRDS("default_crops.rds"), reactiveVal)

  # list with ecocrop indices for selected climate
  # reactive, so that indices are only recalculated when needed and invalidated
  clim.suits <- lapply(crops, function(cr) reactive(clim.suit(loc$clim, cr())))
  
  output$crop.gdur.months <- renderText(paste("<center><strong>[", round((input$crop.gdur[1] + input$crop.gdur[2])/60), i18n()$t("gen:months:label"), "]</strong></center>"))
  
  # helper function for switching between "view" and "edit" mode
  set.mode <- function(mode) {
    crop.parameters <- c("crop.name", "crop.gdur", "crop.topt", "crop.tlim", "crop.popt", "crop.plim")
    if(mode == "view") {
      for(element in crop.parameters) {
        shinyjs::disable(element)
      }
      shinyjs::show("crop.edit")
      shinyjs::show("crop.delete")
      shinyjs::show("crop.duplicate")
      shinyjs::hide("crop.defaults")
      shinyjs::hide("crop.save")
      shinyjs::hide("crop.cancel")
    }
    if(mode == "edit") {
      for(element in crop.parameters) {
        shinyjs::enable(element)
      }
      shinyjs::hide("crop.edit")
      shinyjs::hide("crop.delete")
      shinyjs::hide("crop.duplicate")
      shinyjs::show("crop.defaults")
      shinyjs::show("crop.save")
      shinyjs::show("crop.cancel")
    }
  }
  
  
  # Loading crop list ---
  observeEvent(input$crops.load, {
    showModal(modalDialog(
     h3("Load a previously saved crop definitions:"), br(),
      fileInput("crops.load.file", "Crop list (.rds)",
                buttonLabel = i18n()$t("Browse ..."),
                multiple = FALSE,
                accept = ".rds"),
      footer = tagList(
        modalButton(i18n()$t("Cancel"))
      )
    ))
  })

  observeEvent(input$crops.load.file, {
    removeModal()
    if (is.null(input$crops.load.file)) return()
    crops <<- lapply(readRDS(input$crops.load.file$datapath), reactiveVal)
    clim.suits <<- lapply(crops, function(cr) reactive(clim.suit(loc$clim, cr())))
    new.names <- names(crops)
    # update lists
    updateSelectInput(session, "crop.selected", choices=c(new.names, "new ..."="new_crop"), selected = new.names[1])
    updateSelectInput(session, "ecind.1.crop", choices=new.names, selected=new.names[1])
    updateSelectInput(session, "ecind.2.crop", choices=new.names, selected=ifelse(length(new.names) > 1, new.names[2], new.names[1]))
    updateSelectInput(session, "ccor.1.crop",  choices=new.names, selected=new.names[1])
    updateSelectInput(session, "ccor.2.crop",  choices=new.names, selected=new.names[1])
    updateSelectInput(session, "pdate.1.crop", choices=new.names, selected=new.names[1])
    updateSelectInput(session, "pdate.2.crop", choices=new.names, selected=ifelse(length(new.names) > 1, new.names[2], new.names[1]))
  })

  # saving crop list ---
  observeEvent(input$crops.save, {
    showModal(modalDialog(
      h3(i18n()$t("Save the crop list on your computer as:")),
      fluidRow(column(12, textInput("crops.save.name", NULL, value = paste0(loc$name, "_crops.rds")))),
      footer = tagList(
        modalButton(i18n()$t("Cancel")),
        downloadButton("crops.save.download", i18n()$t("Download ..."), class="btn btn-primary")
      )
    ))
  })
    
  output$crops.save.download <- downloadHandler(
    filename = function() {
      input$crops.save.name
    },
    content = function(file) {
      removeModal()
      saveRDS(lapply(crops, function(x) isolate(x())), file)
    }
  )
  
  
  # load selected crop ---
  observeEvent(input$crop.selected, {
    if(is.na(input$crop.selected)) {}
    else if(input$crop.selected=="new_crop") {
      set.mode("edit")
      updateTextInput(session, "crop.name", value=NA, placeholder="New Crop")
      updateSliderInput(session, "crop.gdur", value=c(0, 365))
      #updateNumericInput(session, "crop.yield", value=NA)
      updateSliderInput(session, "crop.topt", value=c(0, 50))
      updateSliderInput(session, "crop.tlim", value=c(0, 50))
      updateSliderInput(session, "crop.popt", value=c(0, 5000))
      updateSliderInput(session, "crop.plim", value=c(0, 5000))
    } else {
      set.mode("view")
      cr <- crops[[input$crop.selected]]()
      updateTextInput(session, "crop.name", value=cr@name)
      updateSliderInput(session, "crop.gdur", value=c(cr@GMIN, cr@GMAX))
      #updateNumericInput(session, "crop.yield", value=cr@code)
      updateSliderInput(session, "crop.topt", value=c(cr@TOPMN, cr@TOPMX))
      updateSliderInput(session, "crop.tlim", value=c(cr@TMIN, cr@TMAX))
      updateSliderInput(session, "crop.popt", value=c(cr@ROPMN, cr@ROPMX))
      updateSliderInput(session, "crop.plim", value=c(cr@RMIN, cr@RMAX))
    }
  })
  
  
  # duplicate selected crop ---
  observeEvent(input$crop.duplicate, {
    # copy crop and update name
    cr.iso <- isolate(crops[[input$crop.selected]]())
    cr.iso@name <- paste(input$crop.selected, "[copy]")
    # and insert it into the list
    idx <- which(input$crop.selected == names(crops)) + 1
    if(idx <= length(crops)) {
      crops <<- c(crops[1:idx-1], reactiveVal(cr.iso), crops[idx:length(crops)])
      clim.suits <<- c(clim.suits[1:idx-1], list(NULL), clim.suits[idx:length(clim.suits)])
    } else {
      crops <<- c(crops[1:idx-1], reactiveVal(cr.iso))
      clim.suits <<- c(clim.suits[1:idx-1], list(NULL))
    }
    names(crops)[idx] <<- cr.iso@name
    names(clim.suits)[idx] <<- cr.iso@name
    cr <- crops[[cr.iso@name]]
    clim.suits[[cr.iso@name]] <<- reactive(clim.suit(loc$clim, cr()))
    # and update lists (in ecind and ccor, select same name, or index-1)
    new.names <- names(crops)
    updateSelectInput(session, "crop.selected",choices=c(new.names, "new ..."="new_crop"), selected=cr.iso@name)
    updateSelectInput(session, "ecind.1.crop", choices=new.names, selected=input$ecind.1.crop)
    updateSelectInput(session, "ecind.2.crop", choices=new.names, selected=input$ecind.2.crop)
    updateSelectInput(session, "ccor.1.crop",  choices=new.names, selected=input$ccor.1.crop)
    updateSelectInput(session, "ccor.2.crop",  choices=new.names, selected=input$ccor.2.crop)
    updateSelectInput(session, "pdate.1.crop", choices=new.names, selected=input$pdate.1.crop)
    updateSelectInput(session, "pdate.2.crop", choices=new.names, selected=input$pdate.2.crop)
  })
  
  
  # delete selected crop ---
  observeEvent(input$crop.delete, {
    # delete crop from crops list
    old.names <- names(crops)
    crops <<- crops[names(crops) != input$crop.selected]
    clim.suits <<- clim.suits[names(clim.suits) != input$crop.selected]
    new.names <- names(crops)
    # and update lists (in ecind and ccor, select same name, or index-1)
    updateSelectInput(session, "crop.selected",choices=c(new.names, "new ..."="new_crop"), selected=ifelse(length(crops)>0, new.names[max(1, which(input$crop.selected == old.names) - 1)], "new_crop"))
    updateSelectInput(session, "ecind.1.crop", choices=new.names, selected=ifelse(input$ecind.1.crop %in% new.names, input$ecind.1.crop, new.names[max(1, which(input$ecind.1.crop == old.names) - 1)]))
    updateSelectInput(session, "ecind.2.crop", choices=new.names, selected=ifelse(input$ecind.2.crop %in% new.names, input$ecind.2.crop, new.names[max(1, which(input$ecind.2.crop == old.names) - 1)]))
    updateSelectInput(session, "ccor.1.crop",  choices=new.names, selected=ifelse(input$ccor.1.crop %in% new.names, input$ccor.1.crop, new.names[max(1, which(input$ccor.1.crop == old.names) - 1)]))
    updateSelectInput(session, "ccor.2.crop",  choices=new.names, selected=ifelse(input$ccor.2.crop %in% new.names, input$ccor.1.crop, new.names[max(1, which(input$ccor.1.crop == old.names) - 1)]))
    updateSelectInput(session, "pdate.1.crop", choices=new.names, selected=ifelse(input$pdate.1.crop %in% new.names, input$pdate.1.crop, new.names[max(1, which(input$pdate.1.crop == old.names) - 1)]))
    updateSelectInput(session, "pdate.2.crop", choices=new.names, selected=ifelse(input$pdate.2.crop %in% new.names, input$pdate.2.crop, new.names[max(1, which(input$pdate.2.crop == old.names) - 1)]))
  })
  
  
  # edit selected crop ---
  observeEvent(input$crop.edit, {
    set.mode("edit")
  })
  
  
  # load EcoCrop parameters ---
  observeEvent(input$crop.defaults, {
    showModal(modalDialog(size="l",
      h3("Load crop parameters from FAO's EcoCrop database:"), br(),
      reactableOutput("ecocrop.params"),
      footer = tagList(
        modalButton(i18n()$t("Cancel")),
        actionButton("crop.load.default", "Load", class="btn btn-primary")
      )
    ))
  })
  
  # EcoCrop Table
  output$ecocrop.params <- renderReactable({
    reactable(ecocrops,
              searchable=TRUE, defaultPageSize = 20, highlight = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "10px"),
              selection = "single",
              onClick = "select",
              defaultColDef = colDef(minWidth = 50),
              columns = list(
                .selection = colDef(show = FALSE),
                NAME = colDef(minWidth = 120),
                FAMNAME = colDef(minWidth = 90),
                SCIENTNAME = colDef(minWidth = 150)
              ),
              theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee",  boxShadow = "inset 2px 0 0 0 #0D6EFD"))
    )
  })
  
  # set EcoCrop parameters
  observeEvent(input$crop.load.default, {
    params <- ecocrops[getReactableState("ecocrop.params", "selected"),]
    updateTextInput(session, "crop.name", value=paste(params$NAME, "(default)"))
    updateSliderInput(session, "crop.gdur", value=c(params$GMIN, params$GMAX))
    updateSliderInput(session, "crop.topt", value=c(params$TOPMN, params$TOPMX))
    updateSliderInput(session, "crop.tlim", value=c(params$TMIN, params$TMAX))
    updateSliderInput(session, "crop.popt", value=c(params$ROPMN, params$ROPMX))
    updateSliderInput(session, "crop.plim", value=c(params$RMIN, params$RMAX))
    removeModal()
  })
  
  
  # cancel edits (reload selected crop) ---
  observeEvent(input$crop.cancel, {
    item <- input$crop.selected
    # switch first to NA
    updateSelectInput(session, "crop.selected", choices = c(names(crops), "new ..."="new_crop"), selected=NA)
    # and then reload crop
    updateSelectInput(session, "crop.selected", choices = c(names(crops), "new ..."="new_crop"), selected=item)
  })
  
  
  # save edits / add new crop to the list ---
  observeEvent(input$crop.save, {
    # create new crop from the parameters
    cr <- new("ECOCROPcrop", 
              name = input$crop.name,
              GMIN = input$crop.gdur[1], 
              GMAX = input$crop.gdur[2], 
              #code= as.numeric(input$crop.yield), 
              TOPMN= input$crop.topt[1],
              TOPMX= input$crop.topt[2],
              ROPMN= input$crop.popt[1],
              ROPMX= input$crop.popt[2],
              TMIN = input$crop.tlim[1],
              TMAX = input$crop.tlim[2], 
              RMIN = input$crop.plim[1], 
              RMAX = input$crop.plim[2])
    
    # get old names, for updating lists with index
    old.names <- names(crops)
    
    if(input$crop.selected=="new_crop") {
      # add new crop
      crops[[cr@name]] <<- reactiveVal(cr)
      clim.suits[[cr@name]] <<- reactive(clim.suit(loc$clim, crops[[cr@name]]()))
    } else {
      # or update name and crop, if crop already exists
      names(crops)[names(crops)==input$crop.selected] <<- cr@name
      crops[[cr@name]](cr)
      names(clim.suits)[names(clim.suits)==input$crop.selected] <<- cr@name
    } 
    # and add/update list with the new/modified crop
    
    new.names <- names(crops)
    
    # update lists (ecind and ccor lists remain with the same index selected)
    updateSelectInput(session, "crop.selected",choices=c(new.names, "new ..."="new_crop"), selected=cr@name)
    updateSelectInput(session, "ecind.1.crop", choices=new.names, selected=new.names[which(input$ecind.1.crop == old.names)])
    updateSelectInput(session, "ecind.2.crop", choices=new.names, selected=new.names[which(input$ecind.2.crop == old.names)])
    updateSelectInput(session, "ccor.1.crop",  choices=new.names, selected=new.names[which(input$ccor.1.crop == old.names)])
    updateSelectInput(session, "ccor.2.crop",  choices=new.names, selected=new.names[which(input$ccor.2.crop == old.names)])
    updateSelectInput(session, "pdate.1.crop", choices=new.names, selected=new.names[which(input$pdate.1.crop == old.names)])
    updateSelectInput(session, "pdate.2.crop", choices=new.names, selected=new.names[which(input$pdate.2.crop == old.names)])
    
    # disable editing
    set.mode("view")
  })

  
  # Plot crop suitability ---
  output$crop.specification <- renderCachedPlot({
    plot.crop(input$crop.name, input$crop.tlim, input$crop.topt, input$crop.plim, input$crop.popt) +
      labs(title=paste(i18n()$t("crops:fig:title"), input$crop.name), 
           x = i18n()$t("crops:fig:xlab"), y=i18n()$t("crops:fig:ylab"), fill=i18n()$t("crops:fig:legend"))
    
  },
  cacheKeyExpr = {
    list(input$crop.name, input$crop.tlim, input$crop.topt, input$crop.plim, input$crop.popt, input$lang)
  }, res = default.res)
  

  # TAB 5: Ecocrop indices ==========================================================

  # Left Panel
  output$ecind.1.plot <- renderCachedPlot({
    ylab <- ifelse(input$ecind.1.index == "temp", i18n()$t("ecind:fig:temp:ylab"), ifelse(input$ecind.1.index == "prec", i18n()$t("ecind:fig:prec:ylab"), i18n()$t("ecind:fig:comb:ylab")))
    gdur <- round((crops[[input$ecind.1.crop]]()@GMIN + crops[[input$ecind.1.crop]]()@GMAX)/60)
    gp <- plot.ecocrop(clim.suits[[input$ecind.1.crop]](), period=input$ecind.1.period, index=input$ecind.1.index) +
      labs(title = paste0("EcoCrop Index ", input$ecind.1.crop, " — ", i18n()$t(input$ecind.1.index)), 
           subtitle = paste0(i18n()$t(input$ecind.1.period), " — ", gdur, " ", i18n()$t("gen:months:label"), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
           y = ylab, x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6/ecocrop")
    gp
  },
  cacheKeyExpr = {
    list(clim.suits[[input$ecind.1.crop]](), input$ecind.1.period, input$ecind.1.index, input$lang)
  }, res = default.res)

  # Right Panel
  output$ecind.2.plot <- renderCachedPlot({
    ylab <- ifelse(input$ecind.2.index == "temp", i18n()$t("ecind:fig:temp:ylab"), ifelse(input$ecind.2.index == "prec", i18n()$t("ecind:fig:prec:ylab"), i18n()$t("ecind:fig:comb:ylab")))
    gdur <- round((crops[[input$ecind.2.crop]]()@GMIN + crops[[input$ecind.2.crop]]()@GMAX)/60)
    gp <- plot.ecocrop(clim.suits[[input$ecind.2.crop]](), period=input$ecind.2.period, index=input$ecind.2.index) +
      labs(title = paste0("EcoCrop Index ", input$ecind.2.crop, " — ", i18n()$t(input$ecind.2.index)), 
           subtitle = paste0(i18n()$t(input$ecind.2.period), " — ", gdur, " ", i18n()$t("gen:months:label"), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
           y = ylab, x = "", colour = i18n()$t("gen:scenario:label"), caption = "Ref: cru4/cmip6/ecocrop")
    gp
  },
  cacheKeyExpr = {
    list(clim.suits[[input$ecind.2.crop]](), input$ecind.2.period, input$ecind.2.index, input$lang)
  }, res = default.res)
  
  
  # TAB 6: Best planting dates ========================================================

  # Left Panel
  output$pdate.1.plot <- renderCachedPlot({
    gdur <- round((crops[[input$pdate.1.crop]]()@GMIN + crops[[input$pdate.1.crop]]()@GMAX)/60)
    gp <- plot.pdate(clim.suits[[input$pdate.1.crop]](), period=input$pdate.1.period, sce=strsplit(input$pdate.1.sce, "_")[[1]][2], suit=input$pdate.1.suit) +
      labs(title = paste0(i18n()$t("pdate:title"), " ", input$pdate.1.crop),
           subtitle = paste0(i18n()$t(input$pdate.1.period), " — ", gdur, " ", i18n()$t("gen:months:label"), " — ", i18n()$t(input$pdate.1.sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
           y = "", x = "", colour = i18n()$t("gen:data:label"), caption = "Ref: cru4/cmip6/ecocrop")
     
    gp
  },
  cacheKeyExpr = {
    list(clim.suits[[input$pdate.1.crop]](), input$pdate.1.period, input$pdate.1.sce, input$pdate.1.suit, input$lang)
  }, res = default.res)
  
  # Right Panel
  output$pdate.2.plot <- renderCachedPlot({
    gdur <- round((crops[[input$pdate.2.crop]]()@GMIN + crops[[input$pdate.2.crop]]()@GMAX)/60)
    gp <- plot.pdate(clim.suits[[input$pdate.2.crop]](), period=input$pdate.2.period, sce=strsplit(input$pdate.2.sce, "_")[[1]][2], suit=input$pdate.2.suit) +
      labs(title = paste0(i18n()$t("pdate:title"), " ", input$pdate.2.crop),
           subtitle = paste0(i18n()$t(input$pdate.2.period), " — ", gdur, " ", i18n()$t("gen:months:label"), " — ", i18n()$t(input$pdate.2.sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
           y = "", x = "", colour = i18n()$t("gen:data:label"), caption = "Ref: cru4/cmip6/ecocrop")
    gp
  },
  cacheKeyExpr = {
    list(clim.suits[[input$pdate.2.crop]](), input$pdate.2.period, input$pdate.2.sce, input$pdate.2.suit, input$lang)
  }, res = default.res)
  
  
  # TAB 7: Climate corridors ==========================================================

  # Left Panel
  output$ccor.1.plot <- renderCachedPlot({
    gdur <- round((crops[[input$ccor.1.crop]]()@GMIN + crops[[input$ccor.1.crop]]()@GMAX)/60)
    gp <- plot.ccor(clim.suits[[input$ccor.1.crop]](), crops[[input$ccor.1.crop]](), period=input$ccor.1.period, index=input$ccor.1.index, sce=strsplit(input$ccor.1.sce, "_")[[1]][2])
    if(input$ccor.1.index == "temp") {
      gp <- gp + labs(title = paste0(i18n()$t("ccor:fig:temp:title"), " ", input$ccor.1.crop),
                      subtitle = paste0(i18n()$t(input$ccor.1.period), " — ", gdur, " ", i18n()$t("gen:months:label"), " — ", i18n()$t(input$ccor.1.sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("ccor:fig:temp:ylab"), x = "", caption = "Ref: cru4/cmip6/ccor") + guides(fill = guide_legend(i18n()$t("ccor:fig:temp:legend")), colour = guide_legend(i18n()$t("ccor:fig:temp:legend")))
    } else {
      gp <- gp + labs(title = paste0(i18n()$t("ccor:fig:prec:title"), " ", input$ccor.1.crop),
                      subtitle = paste0(i18n()$t(input$ccor.1.period), " — ", gdur, " ", i18n()$t("gen:months:label"), " — ", i18n()$t(input$ccor.1.sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("ccor:fig:prec:ylab"), x = "", caption = "Ref: cru4/cmip6/ccor")
    }
    gp
  },
  cacheKeyExpr = {
    list(clim.suits[[input$ccor.1.crop]](), crops[[input$ccor.1.crop]](), input$ccor.1.period, input$ccor.1.index, input$ccor.1.sce, input$lang)
  }, res = default.res)

  # Right Panel
  output$ccor.2.plot <- renderCachedPlot({
    gdur <- round((crops[[input$ccor.2.crop]]()@GMIN + crops[[input$ccor.2.crop]]()@GMAX)/60)
    gp <- plot.ccor(clim.suits[[input$ccor.2.crop]](), crops[[input$ccor.2.crop]](), period=input$ccor.2.period, index=input$ccor.2.index, sce=strsplit(input$ccor.2.sce, "_")[[1]][2])
    if(input$ccor.2.index == "temp") {
      gp <- gp + labs(title = paste0(i18n()$t("ccor:fig:temp:title"), " ", input$ccor.2.crop),
                      subtitle = paste0(i18n()$t(input$ccor.2.period), " — ",  gdur, " ", i18n()$t("gen:months:label"), " — ", i18n()$t(input$ccor.2.sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("ccor:fig:temp:ylab"), x = "", caption = "Ref: cru4/cmip6/ccor") + guides(fill = guide_legend(i18n()$t("ccor:fig:temp:legend")), colour = guide_legend(i18n()$t("ccor:fig:temp:legend")))
    } else {
      gp <- gp + labs(title = paste0(i18n()$t("ccor:fig:prec:title"), " ", input$ccor.2.crop),
                      subtitle = paste0(i18n()$t(input$ccor.2.period), " — ",  gdur, " ", i18n()$t("gen:months:label"), " — ", i18n()$t(input$ccor.2.sce), " — ", loc$name, " (", loc$lat, "N/", loc$lon, "E)"),
                      y = i18n()$t("ccor:fig:prec:ylab"), x = "", caption = "Ref: cru4/cmip6/ccor") 
    }
    gp
  },
  cacheKeyExpr = {
    list(clim.suits[[input$ccor.2.crop]](), crops[[input$ccor.2.crop]](), input$ccor.2.period, input$ccor.2.index, input$ccor.2.sce, input$lang)
  }, res = default.res)
  
}
  
shinyApp(ui, server)