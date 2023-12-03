require(shinydashboard)
require(tidyverse)
require(shiny)
require(plotly)
require(sf)
require(shinycssloaders)
require(dplyr)
require(leaflet)
require(scales)
library(markdown)

source("Script/UtilityFunctions.R")

sidebar <- dashboardSidebar(sidebarMenu(
  id = 'sidebar',
  
  menuItem("Italia",
           tabName = "italy-tb",
           icon = icon("gauge-high") ,
           startExpanded = TRUE),
  div(
    id = 'sidebar_it',
    conditionalPanel(
      "input.sidebar === 'italy-tb'",
      uiOutput(outputId = "outRefDate"),
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
        "selectedVar",
        label = "Seleziona variabile:",
        choices = setNames(var_descrip$field_name, var_descrip$description)
      ),
      uiOutput(outputId = 'rescByResInput'),
      uiOutput(outputId = 'numR'),
      checkboxInput("seriesByReg", label = "Decomponi serie per regione", value = F),
      
    )
    
  ),
  
  menuItem("Info", tabName = "info", icon = icon("info")),
  
  div(id = 'sidebar_info',
      conditionalPanel("input.sidebar === 'info'"))
))


body <- dashboardBody(
  skin = "black",
  
  includeCSS("styles.css"),
  
  tabItems(
    tabItem(tabName = "info",
            div(class = 'infoClass', style = "margin-top:-2em", includeMarkdown("info.md"))),
    
    tabItem(
      tabName = "italy-tb",
      fluidRow(
        box(
          width = 2,
          title = div(id = 'Form1Title', var_descrip[var_descrip$field_name == 'totale_casi', 'description']),
          fluidRow(
            class = 'first_row',
            id = "rowValForm1",
            withSpinner(uiOutput("Form1"), size = 0.7)
          ),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarForm1',
            withSpinner(uiOutput("varForm1"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'Form2Title', var_descrip[var_descrip$field_name == 'dimessi_guariti', 'description']),
          fluidRow(
            class = 'first_row',
            id = "rowValForm2",
            withSpinner(uiOutput("Form2"), size = 0.7)
          ),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarForm2',
            withSpinner(uiOutput("varForm2"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'Form3Title', var_descrip[var_descrip$field_name == 'deceduti', 'description']),
          fluidRow(
            class = 'first_row',
            id = "rowValForm3",
            withSpinner(uiOutput("Form3"), size = 0.7)
          ),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarForm3',
            withSpinner(uiOutput("varForm3"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'Form4Title', var_descrip[var_descrip$field_name == 'totale_positivi', 'description']),
          fluidRow(
            class = 'first_row',
            id = "rowValForm4",
            withSpinner(uiOutput("Form4"), size = 0.7)
          ),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarForm4',
            withSpinner(uiOutput("varForm4"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'Form5Title', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', 'description']),
          fluidRow(
            class = 'first_row',
            id = "rowValForm5",
            withSpinner(uiOutput("Form5"), size = 0.7)
          ),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarForm5',
            withSpinner(uiOutput("varForm5"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'Form6Title', var_descrip[var_descrip$field_name == 'positivi_tamponi', 'description']),
          fluidRow(
            class = 'first_row',
            id = "rowValForm6",
            withSpinner(uiOutput("Form6"), size = 0.7)
          ),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarForm6',
            withSpinner(uiOutput("varForm6"), size = 0.4)
          )
        ),
        box(
          width = 2,
          title = div(id = 'Form7Title', var_descrip[var_descrip$field_name == 'tamponi', 'description']),
          fluidRow(
            class = 'first_row',
            id = "rowValForm7",
            withSpinner(uiOutput("Form7"), size = 0.7)
          ),
          fluidRow(
            class = 'snd_row',
            id = 'rowVarForm7',
            withSpinner(uiOutput("varForm7"), size = 0.4)
          )
        )
      )
      ,
      fluidRow(column(5,
                      fluidRow(
                        box(
                          width = 12,
                          title = div(id = 'mapTitle', 'Mappa'),
                          withSpinner(leafletOutput(outputId = "map", height = 806),
                                      size = 1)
                        )
                      )),
               column(
                 7,
                 fluidRow(
                   box(
                     width = 6,
                     title = 'Distribuzione totale casi',
                     withSpinner(plotlyOutput(outputId = "CumCasesDistr", height = 314),
                                 size = 1)
                   ),
                   box(
                     width = 6,
                     title = 'Distribuzione attuali positivi',
                     withSpinner(plotlyOutput(outputId = "CurrPosDistr", height = 314),
                                 size = 1)
                   )
                 ),
                 fluidRow(
                   tabBox(
                     width = 12,
                     id = "tabset",
                     selected = "Serie storica variabile selezionata",
                     tabPanel(
                       "Distribuzione totale casi",
                       withSpinner(
                         plotlyOutput(outputId = "timeCumCasesDistr", height = 410),
                         size = 1
                       )
                     )
                     ,
                     tabPanel("Distribuzione positivi",
                              withSpinner(
                                plotlyOutput(outputId = "timePosDistrib", height = 410),
                                size = 1
                              ))
                     ,
                     tabPanel(
                       "Serie storica variabile selezionata",
                       withSpinner(plotlyOutput(outputId = "timeSeries", height = 410),
                                   size = 1)
                     )
                     
                   )
                 )
               )),
      
    )
  ),
  tags$script(src = "myscript.js"),
)

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "Covid-19 Dashboard"),
                    sidebar,
                    body)

server <- function(input, output, session) {
  get_rend_ui <- function(idUi, data, variab, type) {
    switch(type,
           'val' = {
             renderUI(div(
               class = 'valText',
               div(
                 class = 'FAV',
                 id = paste0(idUi, 'AbbrValue'),
                 data()[['adds']][[1]][[variab]][[2]]),
               div(
                 class = 'FRV',
                 id = paste0(idUi, 'RawValue'),
                 data()[['adds']][[1]][[variab]][[1]],
                 style = paste0('display: none;')
               )
             ))
           },
           'var' = {
             renderUI(div(
               class = 'varText',
               div(
                 id = paste0(idUi, 'Inc'),
                 data()[['adds']][[1]][[variab]][[3]],
                 style = data()[['adds']][[1]][[variab]][[6]]
               ),
               div(
                 id = paste0(idUi, 'PercInc'),
                 data()[['adds']][[1]][[variab]][[5]],
                 style = paste0(data()[['adds']][[1]][[variab]][[6]], ';', 'display: none;')
               )
             ))
           })
  }
  
  ## Load general data
  sel_reg <- reactiveVal(regions)
  
  reg_data <- reactive({
    load_reg_data_add(path_r = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv', 
                      path_p = 'popolazione_residente_2020-2022.csv')
  })
  
  summary <- reactive({
    req(input$inRefDate)
    load_summary(
      regData = reg_data(),
      selRegs = sel_reg(),
      refDate = input$inRefDate
    )
  })
  
  summary_per_reg <- reactive({
    req(input$inRefDate)
    load_summary_reg(regData = reg_data(),
                     refDate = input$inRefDate)
  })
  
  ## Sidebar
  
  # Data selector
  
  output$outRefDate <- renderUI({
    req(reg_data())
    dateInput(
      inputId = 'inRefDate',
      label = 'Data di riferimento:',
      min = min(reg_data()$data),
      max = max(reg_data()$data),
      value = max(reg_data()$data),
      language = 'it'
    )
  })
  
  # Map selector
  
  sf_reg <- reactive({
    load_map_reg(path = 'it_sf/Limiti01012022_g/Reg01012022_g/Reg01012022_g_WGS84.shp')
  })
  
  output$selMap <- renderPlotly({
    plot_ly(
      sf_reg(),
      type = 'scatter',
      mode = 'lines',
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
  
  # General selectors
  
  caricaCBI <- reactiveVal(value = T)
  numResDef <- reactiveVal(value = 10000)
  resByResDef <- reactiveVal(value = F)
  
  observeEvent(input$selectedVar, {
    req(caricaCBI)
    if (input$selectedVar != 'positivi_tamponi') {
      if (caricaCBI() == T) {
        output$rescByResInput <- renderUI({
          checkboxInput(inputId = "rescByRes",
                        label = "Riscala mappa e serie storica per numero di residenti",
                        value = resByResDef())
        })
        if (resByResDef() == T) {
          output$numR <- renderUI({
            sliderInput(
              "numRes",
              label = 'Seleziona numero di residenti:',
              min = 1000,
              max = 100000,
              step = 1000,
              value = numResDef()
            )
          })
        }
        caricaCBI(F)
      }
    } else {
      caricaCBI(T)
      if (!is.null(input$rescByRes)) {
        resByResDef(input$rescByRes)
      }
      if (!is.null(input$numRes)) {
        numResDef(input$numRes)
      }
      output$rescByResInput <- NULL
      output$numR <- NULL
    }
  })
  
  observeEvent(input$rescByRes, {
    if (input$rescByRes == T) {
      output$numR <- renderUI({
        sliderInput(
          "numRes",
          label = 'Seleziona numero di residenti:',
          min = 1000,
          max = 100000,
          step = 1000,
          value = numResDef()
        )
      })
    } else {
      if (!is.null(input$numRes)) {
        numResDef(input$numRes)
      }
      output$numR <- NULL
    }
  })
  
  
  ## Summary Forms
  
  responForm1 <- reactiveVal('')
  responForm2 <- reactiveVal('')
  responForm3 <- reactiveVal('')
  responForm4 <- reactiveVal('')
  responForm5 <- reactiveVal('')
  responForm6 <- reactiveVal('')
  responForm7 <- reactiveVal('')
  
  observeEvent(input$selectedVar, {
    if (responForm1() == '') {
      output$Form1 <-
        get_rend_ui('Form1', summary, 'totale_casi', 'val')
      output$varForm1 <-
        get_rend_ui('Form1', summary, 'totale_casi', 'var')
      
      responForm1('totale_casi')
      
    } else {
      if (input$selectedVar == 'totale_casi' && responForm1() != 'totale_casi') {
        changeFormTitle('Form1Title', var_descrip[var_descrip$field_name == 'totale_casi', 'description'])
        
        output$Form1 <-
          get_rend_ui('Form1', summary, 'totale_casi', 'val')
        output$varForm1 <-
          get_rend_ui('Form1', summary, 'totale_casi', 'var')
        
        responForm1('totale_casi')
      }
      
      if (input$selectedVar == 'nuovi_positivi' && responForm1() != 'nuovi_positivi') {
        changeFormTitle('Form1Title', var_descrip[var_descrip$field_name == 'nuovi_positivi', 'description'])
        
        output$Form1 <-
          get_rend_ui('Form1', summary, 'nuovi_positivi', 'val')
        output$varForm1 <-
          get_rend_ui('Form1', summary, 'nuovi_positivi', 'var')
        
        responForm1('nuovi_positivi')
      }
      
    }
    
  })
  
  observeEvent(input$selectedVar, {
    if (responForm2() == '') {
      output$Form2 <-
        get_rend_ui('Form2', summary, 'dimessi_guariti', 'val')
      output$varForm2 <-
        get_rend_ui('Form2', summary, 'dimessi_guariti', 'var')
      
      responForm2('dimessi_guariti')
      
    } else {
      if (input$selectedVar == 'dimessi_guariti' && responForm2() != 'dimessi_guariti') {
        changeFormTitle('Form2Title', var_descrip[var_descrip$field_name == 'dimessi_guariti', 'description'])
        
        output$Form2 <-
          get_rend_ui('Form2', summary, 'dimessi_guariti', 'val')
        output$varForm2 <-
          get_rend_ui('Form2', summary, 'dimessi_guariti', 'var')
        
        responForm2('dimessi_guariti')
      }
      
      if (input$selectedVar == 'nuovi_dimessi_guariti' && responForm2() != 'nuovi_dimessi_guariti') {
        changeFormTitle('Form2Title', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', 'description'])
        
        output$Form2 <-
          get_rend_ui('Form2', summary, 'nuovi_dimessi_guariti', 'val')
        output$varForm2 <-
          get_rend_ui('Form2', summary, 'nuovi_dimessi_guariti', 'var')
        
        responForm2('nuovi_dimessi_guariti')
      }
      
    }
    
  })
  
  observeEvent(input$selectedVar, {
    if (responForm3() == '') {
      output$Form3 <-
        get_rend_ui('Form3', summary, 'deceduti', 'val')
      output$varForm3 <-
        get_rend_ui('Form3', summary, 'deceduti', 'var')
      
      responForm3('deceduti')
      
    } else {
      if (input$selectedVar == 'deceduti' && responForm3() != 'deceduti') {
        changeFormTitle('Form3Title', var_descrip[var_descrip$field_name == 'deceduti', 'description'])
        
        output$Form3 <-
          get_rend_ui('Form3', summary, 'deceduti', 'val')
        output$varForm3 <-
          get_rend_ui('Form3', summary, 'deceduti', 'var')
        
        responForm3('deceduti')
      }
      
      if (input$selectedVar == 'nuovi_deceduti' && responForm3() != 'nuovi_deceduti') {
        changeFormTitle('Form3Title', var_descrip[var_descrip$field_name == 'nuovi_deceduti', 'description'])
        
        output$Form3 <-
          get_rend_ui('Form3', summary, 'nuovi_deceduti', 'val')
        output$varForm3 <-
          get_rend_ui('Form3', summary, 'nuovi_deceduti', 'var')
        
        responForm3('nuovi_deceduti')
      }
      
    }
    
  })
  
  observeEvent(input$selectedVar, {
    if (responForm4() == '') {
      output$Form4 <-
        get_rend_ui('Form4', summary, 'totale_positivi', 'val')
      output$varForm4 <-
        get_rend_ui('Form4', summary, 'totale_positivi', 'var')
      
      responForm4('totale_positivi')
      
    }
    
  })
  
  observeEvent(input$selectedVar, {
    if (responForm5() == '') {
      output$Form5 <-
        get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'val')
      output$varForm5 <-
        get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'var')
      
      responForm5('totale_ospedalizzati')
      
    } else {
      if (input$selectedVar == 'totale_ospedalizzati' && responForm5() != 'totale_ospedalizzati') {
        changeFormTitle('Form5Title', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'var')
        
        responForm5('totale_ospedalizzati')
      }
      
      if (input$selectedVar == 'isolamento_domiciliare' && responForm5() != 'isolamento_domiciliare') {
        changeFormTitle('Form5Title', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'isolamento_domiciliare', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'isolamento_domiciliare', 'var')
        
        responForm5('isolamento_domiciliare')
      }
      
      if (input$selectedVar == 'ricoverati_con_sintomi' && responForm5() != 'ricoverati_con_sintomi') {
        changeFormTitle('Form5Title', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'ricoverati_con_sintomi', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'ricoverati_con_sintomi', 'var')
        
        responForm5('ricoverati_con_sintomi')
      }
      
      if (input$selectedVar == 'terapia_intensiva' && responForm5() != 'terapia_intensiva') {
        changeFormTitle('Form5Title', var_descrip[var_descrip$field_name == 'terapia_intensiva', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'terapia_intensiva', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'terapia_intensiva', 'var')
        
        responForm5('terapia_intensiva')
      }
      
    }
    
  })
  
  observeEvent(input$selectedVar, {
    if (responForm6() == '') {
      output$Form6 <-
        get_rend_ui('Form6', summary, 'positivi_tamponi', 'val')
      output$varForm6 <-
        get_rend_ui('Form6', summary, 'positivi_tamponi', 'var')
      
      responForm6('positivi_tamponi')
      
    }
    
  })
  
  observeEvent(input$selectedVar, {
    if (responForm7() == '') {

      output$Form7 <-
        get_rend_ui('Form7', summary, 'tamponi', 'val')
      output$varForm7 <-
        get_rend_ui('Form7', summary, 'tamponi', 'var')
      
      responForm7('tamponi')
      
    } else {
      if (input$selectedVar == 'tamponi' && responForm7() != 'tamponi') {
        changeFormTitle('Form7Title', var_descrip[var_descrip$field_name == 'tamponi', 'description'])
        
        output$Form7 <-
          get_rend_ui('Form7', summary, 'tamponi', 'val')
        output$varForm7 <-
          get_rend_ui('Form7', summary, 'tamponi', 'var')
        
        responForm7('tamponi')
      }
      
      if (input$selectedVar == 'nuovi_tamponi' && responForm7() != 'nuovi_tamponi') {
        changeFormTitle('Form7Title', var_descrip[var_descrip$field_name == 'nuovi_tamponi', 'description'])
        
        output$Form7 <-
          get_rend_ui('Form7', summary, 'nuovi_tamponi', 'val')
        output$varForm7 <-
          get_rend_ui('Form7', summary, 'nuovi_tamponi', 'var')
        
        responForm7('nuovi_tamponi')
      }
      
    }
    
  })
  
  ## Bubble map
  
  filteredData <- reactive({
    if (input$selectedVar == 'positivi_tamponi') {
      get_map_data(
        regSummary = summary_per_reg(),
        selVar = input$selectedVar,
        selRegs = sel_reg()
      )
    } else {
      if (!is.null(input$rescByRes)) {
        if (input$rescByRes == T) {
          if (!is.null(input$numRes)) {
            get_map_data(
              regSummary = summary_per_reg(),
              selVar = input$selectedVar,
              selRegs = sel_reg(),
              resResc = input$rescByRes,
              resNumb = input$numRes
            )
          }
        } else {
          get_map_data(
            regSummary = summary_per_reg(),
            selVar = input$selectedVar,
            selRegs = sel_reg()
          )
        }
      }
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(summary_per_reg())  %>% addTiles() %>%
      fitBounds(~ min(long), ~ min(lat), ~ max(long), ~ max(lat))
  })
  
  observe({
    filteredData = filteredData()
    
    if(!is.null(filteredData)) {
    
    labs <- lapply(seq(nrow(filteredData)), function(i) {
      if(input$selectedVar == 'positivi_tamponi') {
        paste0(
          '<b>', filteredData[i, "denominazione_regione"], '</b>', '</br>',
          filteredData[i, "valore_base"], '</br>',
          paste(sep = ' ', filteredData[i, "valore_incr"], 'risp. a ieri'), '</br>'
        )
      } else {
        if (input$rescByRes) {
          paste0(
            '<b>', filteredData[i, "denominazione_regione"], '</b>', '</br>',
                   filteredData[i, "valore_base"], '</br>',
                   paste(sep = ' ','su', format(input$numRes, big.mark = ".", decimal.mark = ','), 'residenti'), '</br>')
        } else {
          paste0(
            '<b>', filteredData[i, "denominazione_regione"], '</b>', '</br>',
                   filteredData[i, "valore_base"], '</br>',
                   paste(sep = ' ', filteredData[i, "valore_incr"], 'risp. a ieri'), '</br>',
                   paste0('(', filteredData[i, "valore_incr_perc"], ')'), '</br>'
          )
        }
      }
    })
    
    leafletProxy("map", data = filteredData) %>%
      clearShapes() %>%
      addCircles(
        lng = ~ long,
        lat = ~ lat,
        radius = ~ circle_radius,
        weight = 1,
        color = "#666666",
        fillColor = var_descrip[var_descrip$field_name == input$selectedVar, "field_color"],
        fillOpacity = 0.6,
        label = lapply(labs, htmltools::HTML),
        labelOptions = labelOptions(
          style = list(
            "font-size" = "12px"
          )
        )
      )
    }
  })
  
  ## Bar charts
  
  output$CumCasesDistr <- renderPlotly({
   draw_distr_chart(
      summary = summary(),
      distrType = 'T'
    )
  })
  
  output$CurrPosDistr <- renderPlotly({
    draw_distr_chart(
      summary = summary(),
      distrType = 'P'
    )
  })
  
  ## Tab set
  
  observeEvent(input$tabset, {
    changeNavBarColor(var_descrip[var_descrip$field_name == input$selectedVar, "field_color"])
  })
  
  output$timeCumCasesDistr <- renderPlotly({
    draw_distr_time_chart(
      regData = reg_data(),
      selRegs = sel_reg(),
      distrType = 'T'
    )
  })
  
  output$timePosDistrib <- renderPlotly({
    draw_distr_time_chart(
      regData = reg_data(),
      selRegs = sel_reg(),
      distrType = 'P'
    )
  })
  
  # V time series
  
  toListen <- reactive({
    list(input$selectedVar, input$rescByRes, input$numRes)
  })
  
  observeEvent(toListen(), {
    if (input$selectedVar == 'positivi_tamponi') {
      output$timeSeries <- renderPlotly({
        draw_time_series_plot(
          regData = reg_data(),
          selRegs = sel_reg(),
          selVar = input$selectedVar,
          serByReg = input$seriesByReg
        )
      })
    } else {
      if (!is.null(input$rescByRes)) {
        if (input$rescByRes == F) {
          output$timeSeries <- renderPlotly({
            draw_time_series_plot(
              regData = reg_data(),
              selRegs = sel_reg(),
              selVar = input$selectedVar,
              serByReg = input$seriesByReg
            )
          })
        } else {
          req(input$numRes)
          output$timeSeries <- renderPlotly({
            draw_time_series_plot(
              regData = reg_data(),
              selRegs = sel_reg(),
              selVar = input$selectedVar,
              serByReg = input$seriesByReg,
              resResc = input$rescByRes,
              numRes = input$numRes
            )
          })
        }
      }
    }
  })
  
  ## JS calls
  
  changeVariableColor <- function(id, rgb) {
    session$sendCustomMessage('changeVariableColor',
                              message = list('id' = id, 'color' = rgb))
  }
  
  changeFormTitle <- function(id, value) {
    session$sendCustomMessage('changeFormTitle',
                              message = list('id' = id, 'value' = value))
  }
  
  changeNavBarColor <- function(rgb) {
    session$sendCustomMessage('changeNavBarColor',
                              message = list('color' = rgb))
  }
  
  observeEvent(input$selectedVar, {
    changeVariableColor('Form1', '#FFFFFF')
    changeVariableColor('Form1Title', '#FFFFFF')
    
    changeVariableColor('Form2', '#FFFFFF')
    changeVariableColor('Form2Title', '#FFFFFF')
    
    changeVariableColor('Form3', '#FFFFFF')
    changeVariableColor('Form3Title', '#FFFFFF')
    
    changeVariableColor('Form4', '#FFFFFF')
    changeVariableColor('Form4Title', '#FFFFFF')
    
    changeVariableColor('Form5', '#FFFFFF')
    changeVariableColor('Form5Title', '#FFFFFF')
    
    changeVariableColor('Form6', '#FFFFFF')
    changeVariableColor('Form6Title', '#FFFFFF')
    
    changeVariableColor('Form7', '#FFFFFF')
    changeVariableColor('Form7Title', '#FFFFFF')
    
    switch(
      input$selectedVar,
      'totale_casi' = {
        changeVariableColor('Form1', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
        changeVariableColor('Form1Title', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
      },
      'nuovi_positivi' = {
        changeVariableColor('Form1', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
        changeVariableColor('Form1Title', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
      },
      'dimessi_guariti' = {
        changeVariableColor('Form2', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
        changeVariableColor('Form2Title', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
      },
      'nuovi_dimessi_guariti' = {
        changeVariableColor('Form2', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
        changeVariableColor('Form2Title', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
      },
      'deceduti' = {
        changeVariableColor('Form3', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
        changeVariableColor('Form3Title', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
      },
      'nuovi_deceduti' = {
        changeVariableColor('Form3', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
        changeVariableColor('Form3Title', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
      },
      'totale_positivi' = {
        changeVariableColor('Form4', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
        changeVariableColor('Form4Title', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
      },
      'isolamento_domiciliare' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
        changeVariableColor('Form5Title', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
      },
      'totale_ospedalizzati' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
        changeVariableColor('Form5Title', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
      },
      'ricoverati_con_sintomi' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
        changeVariableColor('Form5Title', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
      },
      'terapia_intensiva' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
        changeVariableColor('Form5Title', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
      },
      'positivi_tamponi' = {
        changeVariableColor('Form6', var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
        changeVariableColor('Form6Title', var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
      },
      'tamponi' = {
        changeVariableColor('Form7', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
        changeVariableColor('Form7Title', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
      },
      'nuovi_tamponi' = {
        changeVariableColor('Form7', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
        changeVariableColor('Form7Title', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
      }
    )
    
  })
  
}


shinyApp(ui, server)
