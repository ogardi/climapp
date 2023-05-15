###############################################################################
# CARITAS-CDA: Shiny-App User Interface
# =============================================================================
# 
# Author: oliver.gardi@bfh.ch
# Date:   February 2022
# 

ref.cruts   <- em("- Observed CRU TS4.05 monthly data at 0.5º spatial resolution ( Sources @ KNMI: ", a(href="https://climexp.knmi.nl/select.cgi?field=cru4_tmp", "cru4_tmp"), "and", a(href="https://climexp.knmi.nl/select.cgi?field=cru4_pre", "cru4_pre"), ")")
ref.cmip6  <- em("- CMIP6 model ensembles at 250km spatial resolution ( Sources @ KNMI: ", a(href="https://climexp.knmi.nl/select.cgi?id=someone@somewhere&field=cmip6_tas_mon_mod_ssp126", "cmip6_tas"), "and", a(href="https://climexp.knmi.nl/select.cgi?id=someone@somewhere&field=cmip6_pre_mon_mod_ssp126", "cmip6_pre"), "), downscaled to CRU TS4.05 (delta change to 1991:2020 period)")
ref.cmip5 <- em("- CMIP5 model ensembles, monthly data at 2.5º spatial resolution ( Source @ KNMI: ", a(href="https://climexp.knmi.nl/selectfield_cmip5.cgi", "cmip5"), ")")
ref.ccor <- em("- Climate corridors article by Orlowsky et al.(xxx)")
ref.ecocr <- em("- FAO ecocrop parameters available in the R-package ", a(href="https://cran.r-project.org/web/packages/dismo/index.html", "dismo"))
ref.ecind <-  em("- Ecocrop model of R. Hijmans implemented in the R-package ", a(href="https://cran.r-project.org/web/packages/dismo/index.html", "dismo"), ", ignoring monthly minimum and crop killing temperatures")


           
ui <- fluidPage(
  
  # Header ====================================================================
  tagList(tags$head(tags$title("CARITAS - Climate Data Analysis"))),
  br(),
    sidebarLayout(position="right",
      
      # Title (left Panel) ----------------------------------------------------
      mainPanel = mainPanel(width=6,
        h1("CARITAS", style="color:#DB001BFF; margin-bottom: 0px; margin-top: 5px"),
        h2("Climate Data Analysis", style="color:#DB001BFF; margin-top: 0px"),
        div("v0.2 / May 2022 / oliver.gardi@bfh.ch", style="margin-bottom: 30px")
      ),
      
      # Location (right Panel) -------------------------------------------------
      sidebarPanel = sidebarPanel(width=6,
        h4("Current Location:", style="margin-bottom: 5px; margin-top: 0px"), 
        fluidRow(
          column(7, h3(textOutput("location"), style="margin-bottom: 0px; margin-top: 5px")),
          column(5, 
            column(4, actionButton("loc.load", "load ...", class="btn btn-primary"), align="right"),
            column(4, actionButton("loc.new",  "new ...", class="btn btn-primary"), align="right"),
            column(4, downloadButton("loc.save",  "save ...", class="btn btn-primary"), align="right")
          )
        )
      )
    ),
  
  # Tabset ====================================================================
  tabsetPanel(
    
    # Timeseries --------------------------------------------------------------
    tabPanel("Time Series",
      br(),
      fluidRow(
        # Left Panel
        column(6, 
          fluidRow(
            column(5, selectInput("tser.1.var", "Variable", c("Mean monthly temperature (ºC)" = "tas", "Mean monthly precipitation (mm/day)" = "pre"), "tas")),
            column(2, selectInput("tser.1.start", label = "From", month.abb, "Jan")),
            column(2, selectInput("tser.1.end", label = "To", month.abb, "Dec")),
            column(2, actionButton("tser.1.update", "update", class="btn btn-primary"),  style="margin-top: 25px")
          ),
          plotOutput("tser.1.plot", height="500px")
        ),
        # Right Panel
        column(6, 
          fluidRow( 
            column(5, selectInput("tser.2.var", "Variable", c("Mean monthly temperature (ºC)" = "tas", "Mean monthly precipitation (mm/day)" = "pre"), "pre")),
            column(2, selectInput("tser.2.start", label = "From", month.abb, "Jan")),
            column(2, selectInput("tser.2.end", label = "To", month.abb, "Dec")),
            column(2, actionButton("tser.2.update", "update", class="btn btn-primary"),  style = "margin-top: 25px")
          ),
          plotOutput("tser.2.plot", height="500px")
        )
      ),
      br(),
      # Footer
      p(em("Data:"), br(), ref.cruts, br(), ref.cmip6)
    ),
    
    # Climographs -------------------------------------------------------------
    tabPanel("Climatology",
      br(),
      fluidRow(
        # Left Panel
        column(6, 
          fluidRow( 
            column(5, selectInput("clim.1.var", "Variable", c("Mean monthly temperature (ºC)" = "tas", "Mean monthly precipitation (mm/day)" = "pre"), "tas")),
            column(4, selectInput("clim.1.set_sce", "Set / Scenario", 
                                  c("Observed (CRU TS4.05)" = "cru4", "+1.8ºC (CMIP6 ssp126)" = "cmip6_ssp126", "+2.7ºC (CMIP6 ssp245)" = "cmip6_ssp245", 
                                                         "+3.6ºC (CMIP6 ssp370)" = "cmip6_ssp370", "+4.4ºC (CMIP6 ssp585)" = "cmip6_ssp585"), "cru4"))
          ),
          plotOutput("clim.1.plot", height="500px")
        ),
        # Right Panel
        column(6, 
          fluidRow( 
            column(5, selectInput("clim.2.var", "Variable", c("Mean monthly temperature (ºC)" = "tas", "Mean monthly precipitation (mm/day)" = "pre"), "pre")),
            column(4, selectInput("clim.2.set_sce", "Set / Scenario", 
                                  c("Observed (CRU TS4.05)" = "cru4", "+1.8ºC (CMIP6 ssp126)" = "cmip6_ssp126", "+2.7ºC (CMIP6 ssp245)" = "cmip6_ssp245", 
                                                         "+3.6ºC (CMIP6 ssp370)" = "cmip6_ssp370", "+4.4ºC (CMIP6 ssp585)" = "cmip6_ssp585"), "cru4"))
          ),
          plotOutput("clim.2.plot", height="500px")
        )
      ),
      br(),
      # Footer
      p(em("Data:"), br(), ref.cruts, br(), ref.cmip6)
    ),
    
    # Walther-Lieth Climate Diagram -------------------------------------------------------------
    tabPanel("Walter-Lieth",
      br(),
      fluidRow(
        # Left Panel
        column(6, 
          fluidRow( 
            column(4, selectInput("diag.1.set_sce", "Set / Scenario", 
                                  c("Observed (CRU TS4.05)" = "cru4", "+1.8ºC (CMIP6 ssp126)" = "cmip6_ssp126", "+2.7ºC (CMIP6 ssp245)" = "cmip6_ssp245", 
                                                         "+3.6ºC (CMIP6 ssp370)" = "cmip6_ssp370", "+4.4ºC (CMIP6 ssp585)" = "cmip6_ssp585"), "cru4"))
          ),
          plotOutput("diag.1.plot", height="500px")
        ),
        # Right Panel
        column(6, 
          fluidRow( 
            column(4, selectInput("diag.2.set_sce", "Set / Scenario", 
                                  c("Observed (CRU TS4.05)" = "cru4", "+1.8ºC (CMIP6 ssp126)" = "cmip6_ssp126", "+2.7ºC (CMIP6 ssp245)" = "cmip6_ssp245", 
                                                         "+3.6ºC (CMIP6 ssp370)" = "cmip6_ssp370", "+4.4ºC (CMIP6 ssp585)" = "cmip6_ssp585"), "cru4"))
          ),
          plotOutput("diag.2.plot", height="500px")
        )
      ),
      br(),
      # Footer
      p(em("Data:"), br(), ref.cruts, br(), ref.cmip6)
    ),
    
    # Crop Specification  ----------------------------------------------------------------
    tabPanel("Crop Specification",
      br(),
      fluidRow(
        column(12,
          sidebarLayout(
            # Crop Parameters
            sidebarPanel(width = 4,
              h3("Crop list:"),
              fluidRow(
                column(6, selectInput("crops.list", NULL, "", "")),
                column(1),
                column(2, actionButton("crops.load", "load ...", class="btn btn-primary"), align="right"),
                column(3, downloadButton("crops.save", "save ...", class="btn btn-primary"), align="right")
              ),
              hr(),
              h3(textOutput("crop.name")),
              fluidRow(
                column(6, numericInput("crop.profit", "Profit (USD/ha)", 10000, 0, 100000, step=1000)),
                column(6, sliderInput("crop.gdur", "GMIN/GMAX", 0, 365, c(65, 365), 5))
              ),
              hr(),
              fluidRow(
                column(6,
                  #sliderInput("crop.tmin", "TMIN/TOPMN", 0, 50,  c(10, 18),  1),
                  #sliderInput("crop.tmax", "TOPMX/TMAX", 0, 50,  c(33, 47),  1)
                  sliderInput("crop.topt", "TOPMN/TOPMX", 0, 50,  c(18, 33),  1),
                  sliderInput("crop.tlim", "TMIN/TMAX",   0, 50,  c(10, 47),  1)
                ),
                column(6,
                  #sliderInput("crop.pmin", "RMIN/ROPMN", 0, 5000, c(400, 600), 100),
                  #sliderInput("crop.pmax", "ROPMX/RMAX", 0, 5000, c(1200, 1800), 100)
                  sliderInput("crop.popt", "ROPMN/ROPMX", 0, 5000, c(600, 1200), 100),
                  sliderInput("crop.plim", "RMIN/RMAX",   0, 5000, c(400, 1800), 100)
                ),
              ),
              hr(),
              fluidRow(
                column(6),
                column(3, actionButton("crop.del", "delete crop", class="btn btn-primary") , align="right"),
                column(3, actionButton("crop.add", "add crop ...", class="btn btn-primary") , align="right")
              )
            ),
            # Crop Suitability
            mainPanel(width = 8,
              br(),
              plotOutput("crop.specification", height="470px")
            )
          )
        )
      ),
      br(),
      h2("Ecocrop Parameters"),
      textOutput("ecocrop.row"),
      reactable::reactableOutput("ecocrop.params"),
       br(),
       # Footer
       p(em("Data:"), br(), ref.ecocr)
    ),
     
    # Ecocrop indices  ------------------------------------------------------
    tabPanel("Ecocrop indices",
      br(),
      fluidRow(
        # Left Panel
        column(6,
          fluidRow(
            column(4, selectInput("ecind.1.crop", "Crop", choices = NULL))
          ),
          plotOutput("ecind.1.plot", height="500px")
        ),
        # Right Panel
        column(6,
          fluidRow(
            column(4, selectInput("ecind.2.crop", "Crop", choices = NULL))
          ),
          plotOutput("ecind.2.plot", height="500px")
        )
      ),
      br(),
      # Footer
      p(em("Data:"), br(), ref.cruts, br(), ref.cmip6, br(), ref.ecind)
    ),
    
    # Climate Corridors  ------------------------------------------------------
    tabPanel("Climate corridors",
      br(),
      fluidRow(
        # Left Panel
        column(6,
          fluidRow(
            column(4, selectInput("ccor.1.crop", "Crop", choices = NULL)),
            column(3, selectInput("ccor.1.var", "Variable", c("tas", "pre"), "tas")),
            column(3, selectInput("ccor.1.sce", "Scenario", 
                                  c("+1.8ºC (CMIP6 ssp126)" = "cmip6_ssp126", "+2.7ºC (CMIP6 ssp245)" = "cmip6_ssp245", 
                                    "+3.6ºC (CMIP6 ssp370)" = "cmip6_ssp370", "+4.4ºC (CMIP6 ssp585)" = "cmip6_ssp585"), "cmip6_ssp370"))
          ),
          plotOutput("ccor.1.plot", height="500px")
        ),
        # Right Panel
        column(6,
          fluidRow(
            column(4, selectInput("ccor.2.crop", "Crop", choices = NULL)),
            column(3, selectInput("ccor.2.var", "Variable", c("tas", "pre"), "pre")),
            column(3, selectInput("ccor.2.sce", "Scenario", 
                                  c("+1.8ºC (CMIP6 ssp126)" = "cmip6_ssp126", "+2.7ºC (CMIP6 ssp245)" = "cmip6_ssp245", 
                                    "+3.6ºC (CMIP6 ssp370)" = "cmip6_ssp370", "+4.4ºC (CMIP6 ssp585)" = "cmip6_ssp585"), "cmip6_ssp370"))
          ),
          plotOutput("ccor.2.plot", height="500px")
        )
      ),
      br(),
      # Footer
      p(em("Data:"), br(), ref.cruts, br(), ref.cmip6, br(), ref.ccor)
    )
  )
)
