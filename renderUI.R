###############################################################################
# CARITAS-CDA: Shiny-App User Interface
# =============================================================================
# 
# Author: oliver.gardi@gmail.com
# Date:   August 2023
# 


# to be rendered through Shiny-Server (because of i18n translation)
output$page_content <- renderUI({
  
tagList(
  
  # Header ====================================================================
  tagList(tags$head(tags$title("climApp")),
          tags$style(".popover{width: 500px; max-width: 500px;}"),
          tags$style(".selectize-input {white-space: nowrap;}"),
          tags$style(".selectize-dropdown {width: max-content !important; min-width: 100%;}"),
          ),
  br(),
  
  sidebarLayout(position="right",
      
    # Title (left Panel) ----------------------------------------------------
    mainPanel = mainPanel(width=6,
      column(8, 
        h1(i18n()$t("head:title"), style="color:#DB001BFF; margin-bottom: 0px; margin-top: 5px"),
        h2(i18n()$t("head:subtitle"), style="color:#DB001BFF; margin-top: 0px"),
        div(i18n()$t("head:credits"), style="margin-bottom: 30px"),
      ),
      column(3,
        selectInput("lang", i18n()$t("head:language"), c("English", "Français", "Deutsch"), input$lang)                           
      ),
      column(1)
    ),
      
    # Location (right Panel) -------------------------------------------------
    sidebarPanel = sidebarPanel(width=6,
      h4(i18n()$t("loc:current"), style="margin-bottom: 5px; margin-top: 0px"), 
      fluidRow(
        column(7, h3(textOutput("location"), style="margin-bottom: 0px; margin-top: 5px")),
        column(5, 
          column(5, actionButton("loc.new",  i18n()$t("loc:new:button"), class="btn btn-primary"), align="right"),
          column(7, actionButton("loc.load", i18n()$t("loc:load:button"), class="btn btn-primary"), align="right"),
        )
      )
    ),
  ),
  
  # Tabset ====================================================================
  tabsetPanel(
    
    # TAB 1: Timeseries --------------------------------------------------------------
    tabPanel(
      span(i18n()$t("tser:title"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("tser:title"), "</b>"), content=i18n()$t("tser:info"))),
      br(),
      fluidRow(
        # Left Panel
        column(6, 
          fluidRow(
            column(5, selectInput("tser.1.var", i18n()$t("tser:var:label"), VARS %>% set_names(i18n()$t(.)), "tas")),
            column(2, selectInput("tser.1.start", i18n()$t("tser:start:label"), month.abb %>% set_names(i18n()$t(.)), "Jan")),
            column(2, selectInput("tser.1.end", span(i18n()$t("tser:end:label"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("tser:period:info:title"), "</b>"), content=i18n()$t("tser:period:info"))), month.abb %>% set_names(i18n()$t(.)), "Dec")),
            column(2, actionButton("tser.1.update", i18n()$t("tser:update"), class="btn btn-primary"),  style="margin-top: 25px")
          ),
          #withSpinner(plotlyOutput("tser.1.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
          withSpinner(plotOutput("tser.1.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        ),
        # Right Panel
        column(6, 
          fluidRow( 
            column(5, selectInput("tser.2.var",     i18n()$t("tser:var:label"), VARS %>% set_names(i18n()$t(.)), "pre")),
            column(2, selectInput("tser.2.start",   i18n()$t("tser:start:label"), month.abb %>% set_names(i18n()$t(.)),   "Jan")),
            column(2, selectInput("tser.2.end",     i18n()$t("tser:end:label"),   month.abb %>% set_names(i18n()$t(.)),   "Dec")),
            column(2, actionButton("tser.2.update", i18n()$t("tser:update"), class="btn btn-primary"),  style = "margin-top: 25px")
          ),
          withSpinner(plotOutput("tser.2.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        )
      ),
      br(),
      # Footer
      p(em(i18n()$t("foot:ref:label")), br(), HTML(i18n()$t("foot:ref:cru4")), br(), HTML(i18n()$t("foot:ref:cmip6")), br(), br())
    ),
    
    # TAB 2: Climographs -------------------------------------------------------------
    tabPanel(
      span(i18n()$t("clim:title"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("clim:title"), "</b>"), content=i18n()$t("clim:info"))),
      br(),
      fluidRow(
        # Left Panel
        column(6, 
          fluidRow( 
            column(5, selectInput("clim.1.var",     i18n()$t("clim:var:label"), VARS %>% set_names(i18n()$t(.)),  "tas")),
            column(4, selectInput("clim.1.set_sce", i18n()$t("gen:scenario:label"),   SCENS %>% set_names(i18n()$t(.)), "cru4"))
          ),
          withSpinner(plotOutput("clim.1.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        ),
        # Right Panel
        column(6, 
          fluidRow( 
            column(5, selectInput("clim.2.var",     i18n()$t("clim:var:label"), VARS %>% set_names(i18n()$t(.)),  "pre")),
            column(4, selectInput("clim.2.set_sce", i18n()$t("gen:scenario:label"),   SCENS %>% set_names(i18n()$t(.)), "cru4"))
          ),
          withSpinner(plotOutput("clim.2.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        )
      ),
      br(),
      # Footer
      p(em(i18n()$t("foot:ref:label")), br(), HTML(i18n()$t("foot:ref:cru4")), br(), HTML(i18n()$t("foot:ref:cmip6")), br(), br())
    ),
    
    # # TAB 3: Walther-Lieth Climate Diagram -------------------------------------------------------------
    # tabPanel(
    #   span(i18n()$t("diag:title"), popify(icon("info-circle"), title=paste(icon("info-circle"), i18n()$t("diag:title")), content=i18n()$t("diag:info"))),
    #   br(),
    #   fluidRow(
    #     # Left Panel
    #     column(6, 
    #       fluidRow( 
    #         column(5, selectInput("diag.1.set_sce", i18n()$t("diag:set_sce-1:label"), SCENS[1] %>% set_names(i18n()$t(.)), "cru4")),
    #         column(3, selectInput("diag.1.period",  i18n()$t("diag:period:label"), c("1901-1930"=1901, "1931-1960"=1931, "1961-1990"=1961, "1981-2010"=1981, "1991-2020"=1991), 1991))
    #       ),
    #       withSpinner(plotOutput("diag.1.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
    #     ),
    #     # Right Panel
    #     column(6, 
    #       fluidRow( 
    #         column(5, selectInput("diag.2.set_sce", i18n()$t("diag:set_sce-2:label"), SCENS[-1] %>% set_names(i18n()$t(.)), "cmip6_ssp370")),
    #         column(3, selectInput("diag.2.period",  i18n()$t("diag:period:label"), c("1991-2020"=1991, "2001-2030"=2001, "2031-2060"=2031, "2061-2090"=2061, "2091-2100"=2091), 2091))
    #       ),
    #       withSpinner(plotOutput("diag.2.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
    #     )
    #   ),
    #   br(),
    #   # Footer
    #   p(em(i18n()$t("foot:ref:label")), br(), HTML(i18n()$t("foot:ref:cru4")), br(), HTML(i18n()$t("foot:ref:cmip6")), br(), br())
    # ),
    
    # TAB 4: Crop Specification  ----------------------------------------------------------------
    tabPanel(
      span(i18n()$t("crops:title"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("crops:title"), "</b>"), content=i18n()$t("crops:info"))),
      br(),
      fluidRow(
        # Crop list
        column(2,
          wellPanel(
            h3(i18n()$t("crops:list:label"), style="margin-left: 0; margin-top: 0px; margin-bottom: 5px;"),
            fluidRow(selectInput("crop.selected", NULL, setNames(c(names(crops),"new_crop"), c(names(crops), paste(i18n()$t("crops:new"), "..."))), names(crops)[1], size=17, selectize=FALSE)),
            fluidRow(align="right",
              actionButton("crops.load", paste(i18n()$t("gen:load"), "..."), class="btn btn-primary"),
              actionButton("crops.save", paste(i18n()$t("gen:save"), "..."), class="btn btn-primary"),
            )
          )
        ),
        # Crop specifications
        column(5,
          wellPanel(
            fluidRow(
              column(6,
                disabled(textInput("crop.name", i18n()$t("gen:name:label"), names(crops)[1]))     
              ),
              column(6, align="right",
                actionButton("crop.duplicate", i18n()$t("gen:duplicate"), class="btn btn-primary"),
                hidden(actionButton("crop.cancel", i18n()$t("gen:cancel"), class="btn btn-primary")),
                actionButton("crop.delete", i18n()$t("gen:delete"), class="btn btn-primary"),
                hidden(actionButton("crop.defaults", paste(i18n()$t("gen:load"), "..."), class="btn btn-primary")),
                actionButton("crop.edit", paste(i18n()$t("gen:edit"), "..."), class="btn btn-primary"),
                hidden(actionButton("crop.save", i18n()$t("gen:save"), class="btn btn-primary"))
              )
            ),
            hr(),
            fluidRow(
              column(8,
                disabled(sliderInput("crop.gdur", span(i18n()$t("crops:gdur:label"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("crops:gdur:info:title"), "</b>"), content=i18n()$t("crops:gdur:info"))), 0, 365, c(65, 365), 5))
              ),
              column(4,
                # disabled(numericInput("crop.yield", "Yield potential (t/ha)", NA, min=0, step=1))
                # tags$label(class="control-label", "value used:"),
                br(), br(),
                htmlOutput("crop.gdur.months")
              )
            ),
            hr(),
            fluidRow(
              column(6,
                disabled(sliderInput("crop.topt", i18n()$t("crops:topt:label"), 0, 50,  c(18, 33),  1)),
                disabled(sliderInput("crop.tlim", i18n()$t("crops:tlim:label"),   0, 50,  c(10, 47),  1))
              ),
              column(6,
                disabled(sliderInput("crop.popt", i18n()$t("crops:popt:label"), 0, 5000, c(600, 1200), 100)),
                disabled(sliderInput("crop.plim", i18n()$t("crops:plim:label"),   0, 5000, c(400, 1800), 100))
              )
            )
          )
        ),
        # Crop Suitability
        column(5,
          withSpinner(plotOutput("crop.specification", height="510px"), caption=i18n()$t("gen:fig:spinner"))     
        )
      ),
      p(p(em(HTML(i18n()$t("foot:ecocrop:disclaimer"))))),
      # Footer
      p(em(i18n()$t("foot:ref:label")), br(), HTML(i18n()$t("foot:ref:ecocrop:db")), br(), br(), br())
    ),

    # TAB 5: EcoCrop Index  ------------------------------------------------------
    tabPanel(
      span(i18n()$t("ecind:title"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("ecind:title"), "</b>"), content=i18n()$t("ecind:info"))),
      br(),
      fluidRow(
        # Left Panel
        column(6,
          fluidRow(
            column(3, selectInput("ecind.1.crop", i18n()$t("gen:crop:label"), choices = names(crops), selected=names(crops)[1])),
            column(3, selectInput("ecind.1.period", i18n()$t("gen:period:label"), choices = c(setNames(c("rainfed", "irrigated"), c(i18n()$t("gen:period:label:rnf"), i18n()$t("gen:period:label:irr"))), month.abb %>% set_names(i18n()$t(.))), selected="rainfed")),
            column(2, selectInput("ecind.1.index", i18n()$t("ecind:index:label"), choices = setNames(c("comb", "prec", "temp"), c(i18n()$t("ecind:index:label:comb"), i18n()$t("gen:index:label:prec"), i18n()$t("gen:index:label:temp"))), selected="comb"))
          ),
          withSpinner(plotOutput("ecind.1.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        ),
        # Right Panel
        column(6,
          fluidRow(
            column(3, selectInput("ecind.2.crop", i18n()$t("gen:crop:label"), choices = names(crops), selected=names(crops)[2])),
            column(3, selectInput("ecind.2.period", i18n()$t("gen:period:label"), choices = c(setNames(c("rainfed", "irrigated"), c(i18n()$t("gen:period:label:rnf"), i18n()$t("gen:period:label:irr"))), month.abb %>% set_names(i18n()$t(.))), selected="rainfed")),
            column(2, selectInput("ecind.2.index", i18n()$t("ecind:index:label"), choices = setNames(c("comb", "prec", "temp"), c(i18n()$t("ecind:index:label:comb"), i18n()$t("gen:index:label:prec"), i18n()$t("gen:index:label:temp"))), selected="comb"))
          ),
          withSpinner(plotOutput("ecind.2.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        )
      ),
      br(),
      # Footer
      p(em(i18n()$t("foot:ref:label")), br(), HTML(i18n()$t("foot:ref:cru4")), br(), HTML(i18n()$t("foot:ref:cmip6")), br(), HTML(i18n()$t("foot:ref:ecocrop:idx")))
    ),

    # TAB 6: Best planting date  ----------------------------------------------
    tabPanel(
      span(i18n()$t("pdate:title"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("pdate:title"), "</b>"), content=i18n()$t("pdate:info"))),
      br(),
      fluidRow(
        # Left Panel
        column(6,
          fluidRow(
            column(3, selectInput("pdate.1.crop", i18n()$t("gen:crop:label"), choices = names(crops), selected=names(crops)[1])),
            column(3, selectInput("pdate.1.period", i18n()$t("gen:period:label"), choices = setNames(c("rainfed", "irrigated"), c(i18n()$t("rainfed"), i18n()$t("irrigated"))), selected="rainfed")),
            column(3, selectInput("pdate.1.sce", i18n()$t("gen:scenario:label"), SCENS[-1], "cmip6_ssp370")),
            column(3, radioButtons("pdate.1.suit", span(i18n()$t("pdate:suit:label"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("pdate:suit:info:title"), "</b>"), content=i18n()$t("pdate:suit:info"))), setNames(c(FALSE, TRUE), c(i18n()$t("gen:no"), i18n()$t("gen:yes"))), FALSE, inline=TRUE))
          ),
          withSpinner(plotOutput("pdate.1.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        ),
        # Right Panel
        column(6,
          fluidRow(
            column(3, selectInput("pdate.2.crop", i18n()$t("gen:crop:label"), choices = names(crops), selected=names(crops)[2])),
            column(3, selectInput("pdate.2.period", i18n()$t("gen:period:label"), choices = setNames(c("rainfed", "irrigated"), c(i18n()$t("rainfed"), i18n()$t("irrigated"))), selected="rainfed")),
            column(3, selectInput("pdate.2.sce", i18n()$t("gen:scenario:label"), SCENS[-1], "cmip6_ssp370")),
            column(3, radioButtons("pdate.2.suit", i18n()$t("pdate:suit:label"), setNames(c(FALSE, TRUE), c(i18n()$t("gen:no"), i18n()$t("gen:yes"))), FALSE, inline=TRUE))
          ),
          withSpinner(plotOutput("pdate.2.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        )
      ),
      br(),
      # Footer
      p(em(i18n()$t("foot:ref:label")), br(), HTML(i18n()$t("foot:ref:cru4")), br(), HTML(i18n()$t("foot:ref:cmip6")), br(), HTML(i18n()$t("foot:ref:ecocrop:idx")))
    ), 
    
    # TAB 7: Climate Corridors  ------------------------------------------------------
    tabPanel(
      span(i18n()$t("ccor:title"), popify(icon("info-circle"), title=paste0(icon("info-circle"), " <b>", i18n()$t("ccor:title"), "</b>"), content=i18n()$t("ccor:info"))),
      br(),
      fluidRow(
        # Left Panel
        column(6,
          fluidRow(
            column(3, selectInput("ccor.1.crop", i18n()$t("gen:crop:label"), choices = names(crops), selected=names(crops)[1])),
            column(3, selectInput("ccor.1.period", i18n()$t("gen:period:label"), choices = c(setNames(c("rainfed", "irrigated"), c(i18n()$t("gen:period:label:rnf"), i18n()$t("gen:period:label:irr"))), month.abb %>% set_names(i18n()$t(.))), selected="rainfed")),
            column(2, selectInput("ccor.1.index", i18n()$t("ccor:index:label"), choices = setNames(c("prec", "temp"), c(i18n()$t("gen:index:label:prec"), i18n()$t("gen:index:label:temp"))), selected="temp")),
            column(3, selectInput("ccor.1.sce", i18n()$t("gen:scenario:label"), SCENS[-1], "cmip6_ssp370"))
          ),
          withSpinner(plotOutput("ccor.1.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        ),
        # Right Panel
        column(6,
          fluidRow(
            column(3, selectInput("ccor.2.crop", i18n()$t("gen:crop:label"), choices = names(crops), selected=names(crops)[1])),
            column(3, selectInput("ccor.2.period", i18n()$t("gen:period:label"), choices = c(setNames(c("rainfed", "irrigated"), c(i18n()$t("gen:period:label:rnf"), i18n()$t("gen:period:label:irr"))), month.abb %>% set_names(i18n()$t(.))), selected="rainfed")),
            column(2, selectInput("ccor.2.index", i18n()$t("ccor:index:label"), choices = setNames(c("prec", "temp"), c(i18n()$t("gen:index:label:prec"), i18n()$t("gen:index:label:temp"))), selected="prec")),
            column(3, selectInput("ccor.2.sce", i18n()$t("gen:scenario:label"), SCENS[-1], "cmip6_ssp370"))
          ),
          withSpinner(plotOutput("ccor.2.plot", height="500px"), caption=i18n()$t("gen:fig:spinner"))
        )
      ),
      br(),
      # Footer
      p(em(i18n()$t("foot:ref:label")), br(), HTML(i18n()$t("foot:ref:cru4")), br(), HTML(i18n()$t("foot:ref:cmip6")), br(), HTML(i18n()$t("foot:ref:ccor")))

    ), 
    
    # Disclaimer ====================
    footer=tagList(div(style="color:#DB001BFF; background-color: #e8e8e8; border-style: solid; border-width: 1px; border-radius: 3px; border-color: #ccc;", align="center", HTML(i18n()$t("foot:disclaimer"))))
    
  )
)
})
