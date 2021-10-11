require(shinydashboard)
require(tidyverse)
require(shiny)
require(plotly)
require(sf)
require(shinycssloaders)
require(dplyr)
require(DT)

source("Script/UtilityFunctions.R")

sidebar <- dashboardSidebar(sidebarMenu(
  id = 'sidebar',
  
  menuItem(
    "Italia",
    icon = icon("th"),
    tabName = "italy-tb",
    startExpanded = TRUE
  ),
  div(
    id = 'sidebar_it',
    conditionalPanel(
      "input.sidebar === 'italy-tb'",
      uiOutput(outputId = "selRefDate"),
      tags$div(
        class = "form-group shiny-input-container",
        tags$label(id = 'labelSelezionaRegioni', "Seleziona regioni:"),
        withSpinner(plotlyOutput(outputId = "selMap", height = 300),
                    size = 0.8),
        tags$label(
          id = 'infoMap',
          '(*) doppio clic su un\'area vuota della mappa per selezionare tutte le regioni'
        )
      ),
      selectInput(
        "selectVar",
        label = "Seleziona variabile:",
        choices = setNames(var_descrip$field_name, var_descrip$description)
      ),
      selectInput(
        "selectMapColrPal",
        label = "Color palette per mappa:",
        choices = list(
          "Tonalita singola" = 'SH',
          "Viridis" = 'V',
          'YlOrRd' = 'Y'
        ),
        selected = 'SH'
      ),
      selectInput(
        "selectChartTimeCurrDistr",
        label = "Tipo grafico distribuzione nel tempo:",
        choices = list("Stacked bar chart" = 'S', "Line chart" = 'L'),
        selected = 'L'
      ),
      checkboxInput("checkRisc", label = "Riscala mappa e serie storica per numero di residenti", value = F),
      uiOutput(outputId = 'numCas'),
      checkboxInput("checkbSerieReg", label = "Visualizza serie per regione", value = T),
      
    )
    
  ),
  
  menuItem("Info", tabName = "info", icon = icon("info")),
  
  div(id = 'sidebar_it',
      conditionalPanel("input.sidebar === 'info'"))
))


body <- dashboardBody(
  skin = "black",
  
  includeCSS("styles.css"),
  
  tabItems(
    tabItem(tabName = "info",
            div(style = "margin-top:-2em", includeMarkdown("info.md"))),
    
    tabItem(
      tabName = "italy-tb",
      fluidRow(
        box(
          width = 2,
          title = div(id = 'CumCasesTitle', var_descrip[var_descrip$field_name == 'totale_casi', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("CumCases"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarCumCases',
            withSpinner(uiOutput("varCumCases"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'CumHealedTitle', var_descrip[var_descrip$field_name == 'dimessi_guariti', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("CumHealed"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarCumHealed',
            withSpinner(uiOutput("varCumHealed"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'DeathsTitle', var_descrip[var_descrip$field_name == 'deceduti', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("Deaths"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarDeaths',
            withSpinner(uiOutput("varDeaths"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'NewPosTitle', var_descrip[var_descrip$field_name == 'nuovi_positivi', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("NewPos"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarNewPos',
            withSpinner(uiOutput("varNewPos"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'LethRateTitle', var_descrip[var_descrip$field_name == 'tasso_letalita', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("LethRate"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarLethRate',
            withSpinner(uiOutput("varLethRate"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'PosSwabsTitle', var_descrip[var_descrip$field_name == 'pos_tamponi', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("PosSwabs"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarPosSwabs',
            withSpinner(uiOutput("varPosSwabs"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'CurrCasesTitle', var_descrip[var_descrip$field_name == 'totale_positivi', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("CurrCases"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarCurrCases',
            withSpinner(uiOutput("varCurrCases"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'CurrHomeIsTitle', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("CurrHomeIs"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarCurrHomeIs',
            withSpinner(uiOutput("varCurrHomeIs"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'CurrHospSymptTitle', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("CurrHospSympt"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarCurrHospSympt',
            withSpinner(uiOutput("varCurrHospSympt"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'CurrIntCareTitle', var_descrip[var_descrip$field_name == 'terapia_intensiva', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("CurrIntCare"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarCurrIntCare',
            withSpinner(uiOutput("varCurrIntCare"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'CurrHospTitle', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("CurrHosp"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarCurrHosp',
            withSpinner(uiOutput("varCurrHosp"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'SwabsTitle', var_descrip[var_descrip$field_name == 'nuovi_tamponi', 'description']),
          fluidRow(class = 'first_row',
                   withSpinner(uiOutput("Swabs"), size = 0.7)),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarSwabs',
            withSpinner(uiOutput("varSwabs"), size = 0.4)
          )
        )
      )
      ,
      fluidRow(column(5,
                      fluidRow(
                        box(
                          width = 12,
                          title = textOutput('mapTitle'),
                          withSpinner(plotlyOutput(outputId = "mapRegDistr", height = 880), size = 1)
                        )
                      )),
               column(
                 7,
                 fluidRow(
                   box(
                     width = 6,
                     title = 'Distribuzione totale casi',
                     withSpinner(plotlyOutput(outputId = "CumCasesDistr"),
                                 size = 1)
                   ),
                   box(
                     width = 6,
                     title = 'Distribuzione attuali positivi',
                     withSpinner(plotlyOutput(outputId = "CurrPosDistr"),
                                 size = 1)
                   )
                 ),
                 fluidRow(tabBox(
                   width = 12,
                   id = "tabset",
                   tabPanel(
                     "Distribuzione totale casi",
                     withSpinner(plotlyOutput(outputId = "timeCumCasesDistr"),
                                 size = 1)
                   )
                   ,
                   tabPanel("Distribuzione positivi",
                            withSpinner(
                              plotlyOutput(outputId = "timePosDistrib"),
                              size = 1
                            ))
                   ,
                   tabPanel("Serie storica",
                            withSpinner(
                              plotlyOutput(outputId = "timeSeries"),
                              size = 1
                            ))
                   
                 ))
               )),
      fluidRow(box(
        width = 12,
        withSpinner(dataTableOutput(outputId = "deTable"),
                    size = 1)
      ))
      
      
    )
  ),
  tags$script(src = "myscript.js"),
)

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "Covid-19 Dashboard"),
                    sidebar,
                    body)

server <- function(input, output, session) {
  get_rend_ui <- function(idUi, data, variab, type, mod = '1') {
    switch(mod,
           '1' = {
             switch(type,
                    'val' = {
                      renderUI(div(
                        id = paste0(idUi, 'Value'),
                        class = 'valText',
                        data()[['adds']][[1]][[variab]][[1]]
                      ))
                    },
                    'var' = {
                      renderUI(div(
                        class = 'varText',
                        div(
                          id = paste0(idUi, 'Inc'),
                          data()[['adds']][[1]][[variab]][[3]],
                          style = data()[['adds']][[1]][[variab]][[8]]
                        ),
                        div(
                          id = paste0(idUi, 'PercInc'),
                          data()[['adds']][[1]][[variab]][[5]],
                          style = paste0(data()[['adds']][[1]][[variab]][[8]], ';display: none;')
                        )
                      ))
                    })
           },
           '2' = {
             switch(type,
                    'val' = {
                      renderUI(div(
                        id = paste0(idUi, 'Value'),
                        class = 'valText',
                        data()[['adds']][[1]][[variab]][[1]]
                      ))
                    },
                    'var' = {
                      renderUI(div(class = 'varText',
                                   div(
                                     id = paste0(idUi, 'PercInc'),
                                     data()[['adds']][[1]][[variab]][[5]],
                                     style = paste0(data()[['adds']][[1]][[variab]][[8]])
                                   )))
                    })
             
           })
    
    
    
  }
  
  
  # Totale casi
  output$CumCases <-
    get_rend_ui('CumCases', summary_it, 'totale_casi', 'val')
  output$varCumCases <-
    get_rend_ui('CumCases', summary_it, 'totale_casi', 'var')
  
  # Nuovi positivi
  output$NewPos <-
    get_rend_ui('NewPos', summary_it, 'nuovi_positivi', 'val')
  output$varNewPos <-
    get_rend_ui('NewPos', summary_it, 'nuovi_positivi', 'var')
  
  # Tasso di letalità
  output$LethRate <-
    get_rend_ui('LethRate', summary_it, 'tasso_letalita', 'val', '2')
  output$varLethRate <-
    get_rend_ui('LethRate', summary_it, 'tasso_letalita', 'var', '2')
  
  # Positivi tamponi
  output$PosSwabs <-
    get_rend_ui('PosSwabs', summary_it, 'pos_tamponi', 'val', '2')
  output$varPosSwabs <-
    get_rend_ui('PosSwabs', summary_it, 'pos_tamponi', 'var', '2')
  
  # Attuali positivi
  output$CurrCases <-
    get_rend_ui('CurrCases', summary_it, 'totale_positivi', 'val')
  output$varCurrCases <-
    get_rend_ui('CurrCases', summary_it, 'totale_positivi', 'var')
  
  # Isolamento domiciliare
  output$CurrHomeIs <-
    get_rend_ui('CurrHomeIs', summary_it, 'isolamento_domiciliare', 'val')
  output$varCurrHomeIs <-
    get_rend_ui('CurrHomeIs', summary_it, 'isolamento_domiciliare', 'var')
  
  # Ricoverati con sintomi
  output$CurrHospSympt <-
    get_rend_ui('CurrHospSympt',
                summary_it,
                'ricoverati_con_sintomi',
                'val')
  output$varCurrHospSympt <-
    get_rend_ui('CurrHospSympt',
                summary_it,
                'ricoverati_con_sintomi',
                'var')
  
  # Terapia intensiva
  output$CurrIntCare <-
    get_rend_ui('CurrIntCare', summary_it, 'terapia_intensiva', 'val')
  output$varCurrIntCare <-
    get_rend_ui('CurrIntCare', summary_it, 'terapia_intensiva', 'var')
  
  # Totale ospedalizzati
  output$CurrHosp <-
    get_rend_ui('CurrHosp', summary_it, 'totale_ospedalizzati', 'val')
  output$varCurrHosp <-
    get_rend_ui('CurrHosp', summary_it, 'totale_ospedalizzati', 'var')
  
  responHealeds <- reactiveVal('')
  responDeaths <- reactiveVal('')
  responSwabs <- reactiveVal('')
  
  observeEvent(input$selectVar, {
    if (responDeaths() == '') {
      output$Deaths <-
        get_rend_ui('Deaths', summary_it, 'deceduti', 'val')
      output$varDeaths <-
        get_rend_ui('Deaths', summary_it, 'deceduti', 'var')
      
      responDeaths('deceduti')
      
    } else {
      if (input$selectVar == 'deceduti' && responDeaths() != 'deceduti') {
        changeVarValue('DeathsTitle', var_descrip[var_descrip$field_name == 'deceduti', 'description'])
        
        output$Deaths <-
          get_rend_ui('Deaths', summary_it, 'deceduti', 'val')
        output$varDeaths <-
          get_rend_ui('Deaths', summary_it, 'deceduti', 'var')
        
        responDeaths('deceduti')
      }
      
      if (input$selectVar == 'nuovi_deceduti' &&
          responDeaths() != 'nuovi_deceduti') {
        changeVarValue('DeathsTitle', var_descrip[var_descrip$field_name == 'nuovi_deceduti', 'description'])
        
        output$Deaths <-
          get_rend_ui('Deaths', summary_it, 'nuovi_deceduti', 'val')
        output$varDeaths <-
          get_rend_ui('Deaths', summary_it, 'nuovi_deceduti', 'var')
        
        responDeaths('nuovi_deceduti')
      }
      
    }
    
  })
  
  observeEvent(input$selectVar, {
    if (responHealeds() == '') {
      # Dimessi guariti
      output$CumHealed <-
        get_rend_ui('CumHealed', summary_it, 'dimessi_guariti', 'val')
      output$varCumHealed <-
        get_rend_ui('CumHealed', summary_it, 'dimessi_guariti', 'var')
      
      responHealeds('dimessi_guariti')
      
    } else {
      if (input$selectVar == 'dimessi_guariti' &&
          responHealeds() != 'dimessi_guariti') {
        changeVarValue('CumHealedTitle', var_descrip[var_descrip$field_name == 'dimessi_guariti', 'description'])
        
        output$CumHealed <-
          get_rend_ui('CumHealed', summary_it, 'dimessi_guariti', 'val')
        output$varCumHealed <-
          get_rend_ui('CumHealed', summary_it, 'dimessi_guariti', 'var')
        
        responHealeds('dimessi_guariti')
      }
      
      if (input$selectVar == 'nuovi_dimessi' &&
          responHealeds() != 'nuovi_dimessi') {
        changeVarValue('CumHealedTitle', var_descrip[var_descrip$field_name == 'nuovi_dimessi', 'description'])
        
        output$CumHealed <-
          get_rend_ui('CumHealed', summary_it, 'nuovi_dimessi', 'val')
        output$varCumHealed <-
          get_rend_ui('CumHealed', summary_it, 'nuovi_dimessi', 'var')
        
        responHealeds('nuovi_dimessi')
      }
      
    }
    
  })
  
  observeEvent(input$selectVar, {
    if (responSwabs() == '') {
      # Dimessi guariti
      output$Swabs <-
        get_rend_ui('Swabs', summary_it, 'nuovi_tamponi', 'val')
      output$varSwabs <-
        get_rend_ui('Swabs', summary_it, 'nuovi_tamponi', 'var')
      
      responSwabs('nuovi_tamponi')
      
    } else {
      if (input$selectVar == 'nuovi_tamponi' &&
          responSwabs() != 'nuovi_tamponi') {
        changeVarValue('SwabsTitle', var_descrip[var_descrip$field_name == 'nuovi_tamponi', 'description'])
        
        output$Swabs <-
          get_rend_ui('Swabs', summary_it, 'nuovi_tamponi', 'val')
        output$varSwabs <-
          get_rend_ui('Swabs', summary_it, 'nuovi_tamponi', 'var')
        
        responSwabs('nuovi_tamponi')
      }
      
      if (input$selectVar == 'tamponi' &&
          responSwabs() != 'tamponi') {
        changeVarValue('SwabsTitle', var_descrip[var_descrip$field_name == 'tamponi', 'description'])
        
        output$Swabs <-
          get_rend_ui('Swabs', summary_it, 'tamponi', 'val')
        output$varSwabs <-
          get_rend_ui('Swabs', summary_it, 'tamponi', 'var')
        
        responSwabs('tamponi')
      }
      
      if (input$selectVar == 'casi_testati' &&
          responSwabs() != 'casi_testati') {
        changeVarValue('SwabsTitle', var_descrip[var_descrip$field_name == 'casi_testati', 'description'])
        
        output$Swabs <-
          get_rend_ui('Swabs', summary_it, 'casi_testati', 'val')
        output$varSwabs <-
          get_rend_ui('Swabs', summary_it, 'casi_testati', 'var')
        
        responSwabs('casi_testati')
      }
      
    }
    
  })
  
  output$CumCasesDistr <- renderPlotly({
    req(input$selRefDate)
    draw_distr_chart(
      summary_it = summary_it(),
      selRegions = sel_reg(),
      date = input$selRefDate,
      typeDistr = 'T'
    )
  })
  
  output$CurrPosDistr <- renderPlotly({
    req(input$selRefDate)
    draw_distr_chart(
      summary_it = summary_it(),
      selRegions = sel_reg(),
      date = input$selRefDate,
      typeDistr = 'P'
    )
  })
  
  # Stacked bar chart vs line chart
  
  output$timeCumCasesDistr <- renderPlotly({
    req(input$selRefDate)
    draw_distr_time_chart(
      data = dati_reg_add(),
      selRegions = sel_reg(),
      date = input$selRefDate,
      typeDistr = 'T',
      selChDistrCurr = input$selectChartTimeCurrDistr
    )
  })
  
  output$timePosDistrib <- renderPlotly({
    req(input$selRefDate)
    draw_distr_time_chart(
      data = dati_reg_add(),
      selRegions = sel_reg(),
      date = input$selRefDate,
      typeDistr = 'P',
      selChDistrCurr = input$selectChartTimeCurrDistr
    )
  })
  
  
  output$deTable <- renderDataTable({
    req(input$selRefDate)
    calc_table(
      data = dati_reg_add(),
      selRegions = sel_reg(),
      date = input$selRefDate
    )
  })
  
  
  
  # Selection Map
  
  output$selMap <- renderPlotly({
    plot_ly(
      sf_reg(),
      alpha = 1,
      color = ~ DEN_REG,
      colors = rep('#00ff00', dim(sf_reg())[1]),
      stroke = I("#666666"),
      span = I(1),
      key = ~ DEN_REG,
      source = 'M'
    ) %>%
      layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        showlegend = F,
        margin = list(
          l = 20,
          r = 20,
          b = 10,
          t = 10
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # click observer
  
  observeEvent(event_data("plotly_click", "M", priority = "event"), {
    reg <- event_data("plotly_click", "M")$key
    
    if (length(sel_reg()) == length(regions)) {
      sel_reg(reg)
    } else {
      if (reg %in% sel_reg() && length(sel_reg()) > 1) {
        sel_reg(sel_reg()[!(sel_reg() %in% reg)])
      }
      else {
        sel_reg(unique(c(sel_reg(), reg)))
      }
    }
    
    cols <-
      ifelse(sf_reg()$DEN_REG %in% sel_reg(), "#00ff00", '#222222')
    
    plotlyProxy("selMap", session) %>%
      plotlyProxyInvoke("restyle", list(fillcolor = cols))
    
  })
  
  
  observeEvent(event_data("plotly_doubleclick", "M"), {
    sel_reg(regions)
    
    plotlyProxy("selMap", session) %>%
      plotlyProxyInvoke("restyle", list(fillcolor = rep('#00ff00', length(regions))))
    
  })
  
  
  sel_reg <- reactiveVal(regions)
  
  sf_reg <- reactive({
    load_map_reg(path = 'Limiti01012020_g/Reg01012020_g/Reg01012020_g_WGS84.shp')
  })
  
  observeEvent(input$checkRisc, {
    setVar <- setNames(var_descrip$field_name, var_descrip$description)
    
    if (input$checkRisc) {
      if (input$selectVar == 'tasso_letalita' |
          input$selectVar == 'pos_tamponi') {
        setVar <-  setVar[!setVar %in% c('tasso_letalita', 'pos_tamponi')]
        updateSelectInput(session,
                          "selectVar",
                          choices = setVar,
                          select = 'totale_casi')
      } else {
        setVar <-  setVar[!setVar %in% c('tasso_letalita', 'pos_tamponi')]
        
        updateSelectInput(session,
                          "selectVar",
                          choices = setVar,
                          selected = input$selectVar)
      }
      
    } else {
      if (input$selectVar == 'tasso_letalita' |
          input$selectVar == 'pos_tamponi') {
        updateSelectInput(session,
                          "selectVar",
                          choices = setVar,
                          selected = 'totale_casi')
      } else {
        updateSelectInput(session,
                          "selectVar",
                          choices = setVar,
                          selected = input$selectVar)
      }
      
    }
  })
  
  output$selRefDate <- renderUI({
    req(dati_reg_add())
    dateInput(
      inputId = 'selRefDate',
      label = 'Data di riferimento:',
      min = min(dati_reg_add()$data) + 1,
      max = max(dati_reg_add()$data),
      value = max(dati_reg_add()$data),
      language = 'it'
    )
  })
  
  output$numCas <- renderUI({
    if (input$checkRisc) {
      sliderInput(
        "numCas",
        label = 'Seleziona numero di residenti:',
        min = 1000,
        max = 100000,
        step = 1000,
        value = 10000
      )
    } else {
      return(NULL)
    }
    
  })
  
  output$mapTitle <- renderText({
    paste("Mappa:", var_descrip[var_descrip$field_name == input$selectVar, "description"], sep =
            " ")
  })
  
  output$mapRegDistr <- renderPlotly({
    req(input$selRefDate)
    
    if (input$checkRisc) {
      req(input$numCas)
      
      draw_map(
        datasf = sf_reg(),
        data = summary_per_reg(),
        selRegions = sel_reg(),
        date = input$selRefDate,
        var = input$selectVar,
        MapColrPal = input$selectMapColrPal,
        ckRisc = input$checkRisc,
        numCasi = input$numCas
      )
      
    } else {
      draw_map(
        datasf = sf_reg(),
        data = summary_per_reg(),
        selRegions = sel_reg(),
        date = input$selRefDate,
        var = input$selectVar,
        MapColrPal = input$selectMapColrPal
      )
    }
    
  })
  
  output$timeSeries <- renderPlotly({
    req(input$selRefDate)
    
    if (input$checkRisc) {
      req(input$numCas)
      
      draw_time_series_plot(
        data = dati_reg_add(),
        selRegions = sel_reg(),
        date = input$selRefDate,
        var = input$selectVar,
        peReg = input$checkbSerieReg,
        ckRisc = input$checkRisc,
        numCasi = input$numCas
      )
      
    } else {
      draw_time_series_plot(
        data = dati_reg_add(),
        selRegions = sel_reg(),
        date = input$selRefDate,
        var = input$selectVar,
        peReg = input$checkbSerieReg
      )
      
    }
  })
  
  dati_residenti <- reactive({
    load_dati_res(path = "residenti2019.csv")
  })
  
  dati_reg <- reactive({
    load_dati_reg(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
    # load_dati_reg(path = 'dpc-covid19-ita-regioni.csv')
  })
  
  dati_reg_add <- reactive({
    load_dati_reg_add(dati_reg = dati_reg(), dati_res_reg = dati_residenti())
  })
  
  summary_per_reg <- reactive({
    req(input$selRefDate)
    load_summary_per_reg(data = dati_reg_add(),
                         date = input$selRefDate)
  })
  
  summary_it <- reactive({
    req(input$selRefDate)
    load_summary_it(
      data = dati_reg_add(),
      selRegions = sel_reg(),
      date = input$selRefDate
    )
  })
  
  changeVariableColor <- function(id, rgb) {
    session$sendCustomMessage('changeVariableColor',
                              message = list('id' = id, 'color' = rgb))
  }
  
  changeVarValue <- function(id, value) {
    session$sendCustomMessage('changeVarValue',
                              message = list('id' = id, 'value' = value))
  }
  
  observeEvent(input$selectVar, {
    changeVariableColor('CumCases', '#FFFFFF')
    changeVariableColor('CumCasesTitle', '#FFFFFF')
    
    changeVariableColor('CumHealed', '#FFFFFF')
    changeVariableColor('CumHealedTitle', '#FFFFFF')
    
    changeVariableColor('Deaths', '#FFFFFF')
    changeVariableColor('DeathsTitle', '#FFFFFF')
    
    changeVariableColor('NewPos', '#FFFFFF')
    changeVariableColor('NewPosTitle', '#FFFFFF')
    
    changeVariableColor('LethRate', '#FFFFFF')
    changeVariableColor('LethRateTitle', '#FFFFFF')
    
    changeVariableColor('PosSwabs', '#FFFFFF')
    changeVariableColor('PosSwabsTitle', '#FFFFFF')
    
    changeVariableColor('CurrCases', '#FFFFFF')
    changeVariableColor('CurrCasesTitle', '#FFFFFF')
    
    changeVariableColor('CurrHomeIs', '#FFFFFF')
    changeVariableColor('CurrHomeIsTitle', '#FFFFFF')
    
    changeVariableColor('CurrHospSympt', '#FFFFFF')
    changeVariableColor('CurrHospSymptTitle', '#FFFFFF')
    
    changeVariableColor('CurrIntCare', '#FFFFFF')
    changeVariableColor('CurrIntCareTitle', '#FFFFFF')
    
    changeVariableColor('CurrHosp', '#FFFFFF')
    changeVariableColor('CurrHospTitle', '#FFFFFF')
    
    changeVariableColor('Swabs', '#FFFFFF')
    changeVariableColor('SwabsTitle', '#FFFFFF')
    
    
    switch(
      input$selectVar,
      'totale_casi' = {
        changeVariableColor('CumCases', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
        changeVariableColor('CumCasesTitle', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
      },
      'dimessi_guariti' = {
        changeVariableColor('CumHealed', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
        changeVariableColor('CumHealedTitle', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
      },
      'nuovi_dimessi' = {
        changeVariableColor('CumHealed', var_descrip[var_descrip$field_name == 'nuovi_dimessi', "field_color"])
        changeVariableColor('CumHealedTitle', var_descrip[var_descrip$field_name == 'nuovi_dimessi', "field_color"])
      },
      'deceduti' = {
        changeVariableColor('Deaths', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
        changeVariableColor('DeathsTitle', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
      },
      'nuovi_deceduti' = {
        changeVariableColor('Deaths', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
        changeVariableColor('DeathsTitle', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
      },
      'nuovi_positivi' = {
        changeVariableColor('NewPos', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
        changeVariableColor('NewPosTitle', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
      },
      'tasso_letalita' = {
        changeVariableColor('LethRate', var_descrip[var_descrip$field_name == 'tasso_letalita', "field_color"])
        changeVariableColor('LethRateTitle', var_descrip[var_descrip$field_name == 'tasso_letalita', "field_color"])
      },
      'pos_tamponi' = {
        changeVariableColor('PosSwabs', var_descrip[var_descrip$field_name == 'pos_tamponi', "field_color"])
        changeVariableColor('PosSwabsTitle', var_descrip[var_descrip$field_name == 'pos_tamponi', "field_color"])
      },
      'totale_positivi' = {
        changeVariableColor('CurrCases', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
        changeVariableColor('CurrCasesTitle', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
      },
      'isolamento_domiciliare' = {
        changeVariableColor('CurrHomeIs', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
        changeVariableColor('CurrHomeIsTitle', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
      },
      'ricoverati_con_sintomi' = {
        changeVariableColor('CurrHospSympt', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
        changeVariableColor('CurrHospSymptTitle', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
      },
      'terapia_intensiva' = {
        changeVariableColor('CurrIntCare', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
        changeVariableColor('CurrIntCareTitle', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
      },
      'totale_ospedalizzati' = {
        changeVariableColor('CurrHosp', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
        changeVariableColor('CurrHospTitle', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
      },
      'casi_testati' = {
        changeVariableColor('Swabs', var_descrip[var_descrip$field_name == 'casi_testati', "field_color"])
        changeVariableColor('SwabsTitle', var_descrip[var_descrip$field_name == 'casi_testati', "field_color"])
      },
      'tamponi' = {
        changeVariableColor('Swabs', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
        changeVariableColor('SwabsTitle', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
      },
      'nuovi_tamponi' = {
        changeVariableColor('Swabs', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
        changeVariableColor('SwabsTitle', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
      },
      'casi_testati' = {
        changeVariableColor('Swabs', var_descrip[var_descrip$field_name == 'casi_testati', "field_color"])
        changeVariableColor('SwabsTitle', var_descrip[var_descrip$field_name == 'casi_testati', "field_color"])
      }
    )
    
    
    
  })
  
}


shinyApp(ui, server)
