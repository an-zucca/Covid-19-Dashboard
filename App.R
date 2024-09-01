library(shinydashboard)
library(tidyverse)
library(shiny)
library(plotly)
library(sf)
library(shinycssloaders)
library(dplyr)
library(leaflet)
library(scales)
library(markdown)

source("resources/UtilityFunctions.R")

sidebar <- dashboardSidebar(sidebarMenu(
  id = 'sidebar',
  menuItem(
    "Italia",
    tabName = "dashboard",
    icon = icon("dashboard"),
    startExpanded = TRUE
  ),
  div(
    id = 'sidebar_dashboard',
    conditionalPanel(
      "input.sidebar === 'dashboard'",
      uiOutput(outputId = "refDate"),
      tags$div(
        class = "form-group shiny-input-container",
        tags$label("Seleziona regioni:"),
        withSpinner(plotlyOutput(outputId = "regionsMap", height = 300), size = 0.8),
        tags$label(
          id = 'regionsMapNote',
          '(*) doppio clic su un\'area vuota della mappa per selezionare tutte le regioni'
        )
      ),
      selectInput(
        inputId = "variable",
        label = "Seleziona variabile:",
        choices = setNames(var_descrip$field_name, var_descrip$description)
      ),
      uiOutput(outputId = 'rescByResInput'),
      uiOutput(outputId = 'numR'),
      checkboxInput("seriesByReg", label = "Decomponi serie per regione", value = F)
    )
  ),
  menuItem("Info", tabName = "info", icon = icon("info")),
  div(id = 'sidebar_info', conditionalPanel("input.sidebar === 'info'"))
))

addSummaryBox <- function(id, var) {
  id_box <- paste('box', id, sep = "-")
  id_box_title <- paste('box', 'title', id, sep = "-")
  id_first_row <- paste0('rowValForm', id)
  id_first_ui <-  paste0('Form', id)
  id_snd_row <- paste0('rowVarForm', id)
  id_var_ui <-  paste0('varForm', id)
  
  summaryBox <- box(
    id = id_box,
    width = 2,
    title = div(id = id_box_title, var_descrip[var_descrip$field_name == var, 'description']),
    div(
      class = 'first_row',
      id = id_first_row,
      withSpinner(uiOutput(id_first_ui), size = 0.7)
    ),
    div(
      class = 'snd_row',
      id = id_snd_row,
      withSpinner(uiOutput(id_var_ui), size = 0.4)
    )
  )
}



body <- dashboardBody(
  skin = "black",
  includeCSS("css/styles.css"),
  tabItems(
    tabItem(
      tabName = "info",
      div(
        class = 'infoClass',
        style = "margin-top:-2em",
        includeMarkdown("info.md")
      )
    ),
    tabItem(
      tabName = "dashboard",
      fluidRow(
        addSummaryBox(id = 1, var = 'totale_casi'),
        addSummaryBox(id = 2, var = 'dimessi_guariti'),
        addSummaryBox(id = 3, var = 'deceduti'),
        addSummaryBox(id = 4, var = 'totale_positivi'),
        addSummaryBox(id = 5, var = 'totale_ospedalizzati'),
        addSummaryBox(id = 6, var = 'positivi_tamponi'),
        addSummaryBox(id = 7, var = 'tamponi')
      ),
      fluidRow(column(5, fluidRow(
        box(
          id = 'mapBox',
          width = 12,
          title = '',
          withSpinner(leafletOutput(outputId = "map", height = 850), size = 1)
        )
      )), column(
        7, fluidRow(
          box(
            width = 6,
            title = div(
              class = "container-div",
              div(class = "left-div", "Distribuzione totale casi"),
              div(
                class = "right-div",
                actionButton(
                  inputId = "pieChart",
                  label = "",
                  icon = icon("chart-pie"),
                  disabled = TRUE
                ),
                actionButton(
                  inputId = "hBarChart",
                  label = "",
                  icon = icon("chart-bar"),
                  disabled = FALSE
                ),
                actionButton(
                  inputId = "barChart",
                  label = "",
                  icon = icon("chart-simple"),
                  disabled = FALSE
                )
              )
            ),
            withSpinner(plotlyOutput(outputId = "CumCasesDistr", height = 314), size = 1)
          ),
          box(
            width = 6,
            title = div(
              class = "container-div",
              div(class = "left-div", "Distribuzione totale positivi"),
              div(
                class = "right-div",
                actionButton(
                  inputId = "pieChart1",
                  label = "",
                  icon = icon("chart-pie"),
                  disabled = FALSE
                ),
                actionButton(
                  inputId = "hBarChart1",
                  label = "",
                  icon = icon("chart-bar"),
                  disabled = TRUE
                ),
                actionButton(
                  inputId = "barChart1",
                  label = "",
                  icon = icon("chart-simple"),
                  disabled = FALSE
                )
              )
            ),
            withSpinner(plotlyOutput(outputId = "CurrPosDistr", height = 314), size = 1)
          )
        ), fluidRow(
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
            ),
            tabPanel("Distribuzione positivi", withSpinner(
              plotlyOutput(outputId = "timePosDistrib", height = 410), size = 1
            )),
            tabPanel(
              "Serie storica variabile selezionata",
              withSpinner(plotlyOutput(outputId = "timeSeries", height = 410), size = 1)
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
  v <- reactiveValues(data = 'pC')
  
  observeEvent(input$pieChart, {
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'barChart', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'hBarChart', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'pieChart', 'value' = TRUE))
    v$data <- 'pC'
  })
  
  observeEvent(input$hBarChart, {
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'barChart', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'hBarChart', 'value' = TRUE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'pieChart', 'value' = FALSE))
    v$data <- 'hBC'
  })
  
  observeEvent(input$barChart, {
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'barChart', 'value' = TRUE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'hBarChart', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'pieChart', 'value' = FALSE))
    
    v$data <- 'bC'
  })
  
  output$CumCasesDistr <- renderPlotly({
    if (!is.null(v$data)) {
      switch(v$data,
             'bC' = {
               draw_distr_bar_chart(summary = summary(), distrType = 'T', 'V')
             },
             'hBC' = {
               draw_distr_bar_chart(summary = summary(), distrType = 'T', 'H')
             },
             'pC' = {
               draw_donut_chart(summary = summary(), distrType = 'T')
             })
    }
    else {
      return ()
    }
  })
  
  v2 <- reactiveValues(data = 'hBC')
  
  observeEvent(input$pieChart1, {
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'barChart1', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'hBarChart1', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'pieChart1', 'value' = TRUE))
    v2$data <- 'pC'
  })
  
  observeEvent(input$hBarChart1, {
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'barChart1', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'hBarChart1', 'value' = TRUE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'pieChart1', 'value' = FALSE))
    v2$data <- 'hBC'
  })
  
  observeEvent(input$barChart1, {
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'barChart1', 'value' = TRUE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'hBarChart1', 'value' = FALSE))
    session$sendCustomMessage('disabledButton',
                              message = list('id' = 'pieChart1', 'value' = FALSE))
    
    v2$data <- 'bC'
  })
  
  output$CurrPosDistr <- renderPlotly({
    if (!is.null(v2$data)) {
      switch(v2$data,
             'bC' = {
               draw_distr_bar_chart(summary = summary(), distrType = 'P', 'V')
             },
             'hBC' = {
               draw_distr_bar_chart(summary = summary(), distrType = 'P', 'H')
             },
             'pC' = {
               draw_donut_chart(summary = summary(), distrType = 'P')
             })
    }
    else {
      return ()
    }
  })
  
  get_rend_ui <- function(idUi, data, variab, type) {
    res <- switch(type, 'val' = {
      renderUI(div(
        class = 'valText',
        div(
          class = 'FAV',
          id = paste0(idUi, 'AbbrValue'),
          data()[['adds']][[1]][[variab]][[2]]
        ),
        div(
          class = 'FRV',
          id = paste0(idUi, 'RawValue'),
          data()[['adds']][[1]][[variab]][[1]],
          style = paste0('display: none;')
        )
      ))
    }, 'var' = {
      renderUI(div(
        class = 'in_row',
        div(
          class = 'varText',
          id = paste0(idUi, 'Inc'),
          data()[['adds']][[1]][[variab]][[3]],
          style = paste0('color:', data()[['adds']][[1]][[variab]][[6]])
        ),
        div(
          class = 'varText',
          id = paste0(idUi, 'PercInc'),
          data()[['adds']][[1]][[variab]][[5]],
          style = paste0('color:', data()[['adds']][[1]][[variab]][[6]], ';', 'display: none;')
        ),
        style = paste0('background-color:', data()[['adds']][[1]][[variab]][[7]], ';')
      ))
    })
  }
  
  ## Load general data
  sel_reg <- reactiveVal(regions)
  
  reg_data <- reactive({
    load_reg_data_add(path_r = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv', path_p = 'resources/popolazione_residente_2020-2022.csv')
  })
  
  summary <- reactive({
    req(input$referenceDate)
    load_summary(
      regData = reg_data(),
      selRegs = sel_reg(),
      refDate = input$referenceDate
    )
  })
  
  summary_per_reg <- reactive({
    req(input$referenceDate)
    load_summary_reg(regData = reg_data(), refDate = input$referenceDate)
  })
  
  ## Sidebar
  
  # Data selector
  
  output$refDate <- renderUI({
    req(reg_data())
    dateInput(
      inputId = 'referenceDate',
      label = 'Data di riferimento:',
      min = min(reg_data()$data),
      max = max(reg_data()$data),
      value = max(reg_data()$data),
      language = 'it'
    )
  })
  
  # Map selector
  
  sf_reg <- reactive({
    load_map_reg(path = 'resources/it_sf/Limiti01012022_g/Reg01012022_g/Reg01012022_g_WGS84.shp')
  })
  
  output$regionsMap <- renderPlotly({
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
    
    plotlyProxy("regionsMap", session) %>%
      plotlyProxyInvoke("restyle", list(fillcolor = cols))
    
  })
  
  observeEvent(event_data("plotly_doubleclick", "M"), {
    sel_reg(regions)
    plotlyProxy("regionsMap", session) %>%
      plotlyProxyInvoke("restyle", list(fillcolor = rep('#00ff00', length(regions))))
    
  })
  
  # General selectors
  
  caricaCBI <- reactiveVal(value = T)
  numResDef <- reactiveVal(value = 10000)
  resByResDef <- reactiveVal(value = F)
  
  observeEvent(input$variable, {
    req(caricaCBI)
    if (input$variable != 'positivi_tamponi') {
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
  
  observeEvent(input$variable, {
    if (responForm1() == '') {
      output$Form1 <-
        get_rend_ui('Form1', summary, 'totale_casi', 'val')
      output$varForm1 <-
        get_rend_ui('Form1', summary, 'totale_casi', 'var')
      
      responForm1('totale_casi')
      
    } else {
      if (input$variable == 'totale_casi' &&
          responForm1() != 'totale_casi') {
        changeFormTitle('box-title-1', var_descrip[var_descrip$field_name == 'totale_casi', 'description'])
        
        output$Form1 <-
          get_rend_ui('Form1', summary, 'totale_casi', 'val')
        
        output$varForm1 <-
          get_rend_ui('Form1', summary, 'totale_casi', 'var')
        
        responForm1('totale_casi')
      }
      
      if (input$variable == 'nuovi_positivi' &&
          responForm1() != 'nuovi_positivi') {
        changeFormTitle('box-title-1', var_descrip[var_descrip$field_name == 'nuovi_positivi', 'description'])
        
        output$Form1 <-
          get_rend_ui('Form1', summary, 'nuovi_positivi', 'val')
        output$varForm1 <-
          get_rend_ui('Form1', summary, 'nuovi_positivi', 'var')
        
        responForm1('nuovi_positivi')
      }
      
    }
    
  })
  
  observeEvent(input$variable, {
    if (responForm2() == '') {
      output$Form2 <-
        get_rend_ui('Form2', summary, 'dimessi_guariti', 'val')
      output$varForm2 <-
        get_rend_ui('Form2', summary, 'dimessi_guariti', 'var')
      
      responForm2('dimessi_guariti')
      
    } else {
      if (input$variable == 'dimessi_guariti' &&
          responForm2() != 'dimessi_guariti') {
        changeFormTitle('box-title-2', var_descrip[var_descrip$field_name == 'dimessi_guariti', 'description'])
        
        output$Form2 <-
          get_rend_ui('Form2', summary, 'dimessi_guariti', 'val')
        output$varForm2 <-
          get_rend_ui('Form2', summary, 'dimessi_guariti', 'var')
        
        responForm2('dimessi_guariti')
      }
      
      if (input$variable == 'nuovi_dimessi_guariti' &&
          responForm2() != 'nuovi_dimessi_guariti') {
        changeFormTitle('box-title-2', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', 'description'])
        
        output$Form2 <-
          get_rend_ui('Form2', summary, 'nuovi_dimessi_guariti', 'val')
        output$varForm2 <-
          get_rend_ui('Form2', summary, 'nuovi_dimessi_guariti', 'var')
        
        responForm2('nuovi_dimessi_guariti')
      }
      
    }
    
  })
  
  observeEvent(input$variable, {
    if (responForm3() == '') {
      output$Form3 <-
        get_rend_ui('Form3', summary, 'deceduti', 'val')
      output$varForm3 <-
        get_rend_ui('Form3', summary, 'deceduti', 'var')
      
      responForm3('deceduti')
      
    } else {
      if (input$variable == 'deceduti' &&
          responForm3() != 'deceduti') {
        changeFormTitle('box-title-3', var_descrip[var_descrip$field_name == 'deceduti', 'description'])
        
        output$Form3 <-
          get_rend_ui('Form3', summary, 'deceduti', 'val')
        output$varForm3 <-
          get_rend_ui('Form3', summary, 'deceduti', 'var')
        
        responForm3('deceduti')
      }
      
      if (input$variable == 'nuovi_deceduti' &&
          responForm3() != 'nuovi_deceduti') {
        changeFormTitle('box-title-3', var_descrip[var_descrip$field_name == 'nuovi_deceduti', 'description'])
        
        output$Form3 <-
          get_rend_ui('Form3', summary, 'nuovi_deceduti', 'val')
        output$varForm3 <-
          get_rend_ui('Form3', summary, 'nuovi_deceduti', 'var')
        
        responForm3('nuovi_deceduti')
      }
      
    }
    
  })
  
  observeEvent(input$variable, {
    if (responForm4() == '') {
      output$Form4 <-
        get_rend_ui('Form4', summary, 'totale_positivi', 'val')
      output$varForm4 <-
        get_rend_ui('Form4', summary, 'totale_positivi', 'var')
      
      responForm4('totale_positivi')
    }
  })
  
  observeEvent(input$variable, {
    if (responForm5() == '') {
      output$Form5 <-
        get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'val')
      output$varForm5 <-
        get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'var')
      
      responForm5('totale_ospedalizzati')
      
    } else {
      if (input$variable == 'totale_ospedalizzati' &&
          responForm5() != 'totale_ospedalizzati') {
        changeFormTitle('box-title-5', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'totale_ospedalizzati', 'var')
        
        responForm5('totale_ospedalizzati')
      }
      
      if (input$variable == 'isolamento_domiciliare' &&
          responForm5() != 'isolamento_domiciliare') {
        changeFormTitle('box-title-5', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'isolamento_domiciliare', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'isolamento_domiciliare', 'var')
        
        responForm5('isolamento_domiciliare')
      }
      
      if (input$variable == 'ricoverati_con_sintomi' &&
          responForm5() != 'ricoverati_con_sintomi') {
        changeFormTitle('box-title-5', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'ricoverati_con_sintomi', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'ricoverati_con_sintomi', 'var')
        
        responForm5('ricoverati_con_sintomi')
      }
      
      if (input$variable == 'terapia_intensiva' &&
          responForm5() != 'terapia_intensiva') {
        changeFormTitle('box-title-5', var_descrip[var_descrip$field_name == 'terapia_intensiva', 'description'])
        
        output$Form5 <-
          get_rend_ui('Form5', summary, 'terapia_intensiva', 'val')
        output$varForm5 <-
          get_rend_ui('Form5', summary, 'terapia_intensiva', 'var')
        
        responForm5('terapia_intensiva')
      }
      
    }
    
  })
  
  observeEvent(input$variable, {
    if (responForm6() == '') {
      output$Form6 <-
        get_rend_ui('Form6', summary, 'positivi_tamponi', 'val')
      output$varForm6 <-
        get_rend_ui('Form6', summary, 'positivi_tamponi', 'var')
      
      responForm6('positivi_tamponi')
      
    }
    
  })
  
  observeEvent(input$variable, {
    if (responForm7() == '') {
      output$Form7 <-
        get_rend_ui('Form7', summary, 'tamponi', 'val')
      output$varForm7 <-
        get_rend_ui('Form7', summary, 'tamponi', 'var')
      
      responForm7('tamponi')
      
    } else {
      if (input$variable == 'tamponi' && responForm7() != 'tamponi') {
        changeFormTitle('box-title-7', var_descrip[var_descrip$field_name == 'tamponi', 'description'])
        
        output$Form7 <-
          get_rend_ui('Form7', summary, 'tamponi', 'val')
        output$varForm7 <-
          get_rend_ui('Form7', summary, 'tamponi', 'var')
        
        responForm7('tamponi')
      }
      
      if (input$variable == 'nuovi_tamponi' &&
          responForm7() != 'nuovi_tamponi') {
        changeFormTitle('box-title-7', var_descrip[var_descrip$field_name == 'nuovi_tamponi', 'description'])
        
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
    if (input$variable == 'positivi_tamponi') {
      get_map_data(
        regSummary = summary_per_reg(),
        selVar = input$variable,
        selRegs = sel_reg()
      )
    } else {
      if (!is.null(input$rescByRes)) {
        if (input$rescByRes == T) {
          if (!is.null(input$numRes)) {
            get_map_data(
              regSummary = summary_per_reg(),
              selVar = input$variable,
              selRegs = sel_reg(),
              resResc = input$rescByRes,
              resNumb = input$numRes
            )
          }
        } else {
          get_map_data(
            regSummary = summary_per_reg(),
            selVar = input$variable,
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
    
    if (!is.null(filteredData)) {
      labs <- lapply(seq(nrow(filteredData)), function(i) {
        if (input$variable == 'positivi_tamponi') {
          paste0(
            '<b>',
            filteredData[i, "denominazione_regione"],
            '</b>',
            '</br>',
            filteredData[i, "valore_base"],
            '</br>',
            paste(sep = ' ', filteredData[i, "valore_incr"], 'risp. a ieri'),
            '</br>'
          )
        } else {
          if (input$rescByRes) {
            paste0(
              '<b>',
              filteredData[i, "denominazione_regione"],
              '</b>',
              '</br>',
              filteredData[i, "valore_base"],
              '</br>',
              paste(
                sep = ' ',
                'su',
                format(
                  input$numRes,
                  big.mark = ".",
                  decimal.mark = ','
                ),
                'residenti'
              ),
              '</br>'
            )
          } else {
            paste0(
              '<b>',
              filteredData[i, "denominazione_regione"],
              '</b>',
              '</br>',
              filteredData[i, "valore_base"],
              '</br>',
              paste(sep = ' ', filteredData[i, "valore_incr"], 'risp. a ieri'),
              '</br>',
              paste0('(', filteredData[i, "valore_incr_perc"], ')'),
              '</br>'
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
          fillColor = var_descrip[var_descrip$field_name == input$variable, "field_color"],
          fillOpacity = 0.6,
          label = lapply(labs, htmltools::HTML),
          labelOptions = labelOptions(style = list("font-size" = "12px"))
        )
    }
  })
  
  ## Tab set
  
  observeEvent(input$tabset, {
    changeNavBarColor(var_descrip[var_descrip$field_name == input$variable, "field_color"])
  })
  
  output$timeCumCasesDistr <- renderPlotly({
    draw_distr_time_chart(
      regData = reg_data(),
      refDate = input$referenceDate,
      selRegs = sel_reg(),
      distrType = 'T'
    )
  })
  
  output$timePosDistrib <- renderPlotly({
    draw_distr_time_chart(
      regData = reg_data(),
      refDate = input$referenceDate,
      selRegs = sel_reg(),
      distrType = 'P'
    )
  })
  
  # V time series
  
  toListen <- reactive({
    list(input$variable,
         input$rescByRes,
         input$numRes,
         input$referenceDate)
  })
  
  observeEvent(toListen(), {
    req(input$referenceDate)
    if (input$variable == 'positivi_tamponi') {
      output$timeSeries <- renderPlotly({
        draw_time_series_plot(
          regData = reg_data(),
          refDate = input$referenceDate,
          selRegs = sel_reg(),
          selVar = input$variable,
          serByReg = input$seriesByReg
        )
      })
    } else {
      if (!is.null(input$rescByRes)) {
        if (input$rescByRes == F) {
          output$timeSeries <- renderPlotly({
            draw_time_series_plot(
              regData = reg_data(),
              refDate = input$referenceDate,
              selRegs = sel_reg(),
              selVar = input$variable,
              serByReg = input$seriesByReg
            )
          })
        } else {
          req(input$numRes)
          output$timeSeries <- renderPlotly({
            draw_time_series_plot(
              regData = reg_data(),
              refDate = input$referenceDate,
              selRegs = sel_reg(),
              selVar = input$variable,
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
    session$sendCustomMessage('changeVariableColor', message = list('id' = id, 'color' = rgb))
  }
  
  printHelloWorld <- function(id, rgb) {
    session$sendCustomMessage('printHelloWorld', message = list('id' = id, 'color' = rgb))
  }
  
  changeFormTitle <- function(id, value) {
    session$sendCustomMessage('changeFormTitle', message = list('id' = id, 'value' = value))
  }
  
  changeNavBarColor <- function(rgb) {
    session$sendCustomMessage('changeNavBarColor', message = list('color' = rgb))
  }
  
  changeBorderColor <- function(id, rgb) {
    session$sendCustomMessage('changeBorderColor', message = list('id' = id, 'color' = rgb))
  }
  
  changeBgColor <- function(id, rgb) {
    session$sendCustomMessage('changeBgColor', message = list('id' = id, 'color' = rgb))
  }
  
  
  
  observeEvent(input$variable, {
    default_color <- '#000'
    default_box_border_color <- '#D2D6DE'
    changeVariableColor('Form1', default_color)
    changeVariableColor('box-title-1', default_color)
    changeBorderColor('box-1', default_box_border_color)
    
    changeVariableColor('Form2', default_color)
    changeVariableColor('box-title-2', default_color)
    changeBorderColor('box-2', default_box_border_color)
    
    changeVariableColor('Form3', default_color)
    changeVariableColor('box-title-3', default_color)
    changeBorderColor('box-3', default_box_border_color)
    
    changeVariableColor('Form4', default_color)
    changeVariableColor('box-title-4', default_color)
    changeBorderColor('box-4', default_box_border_color)
    
    changeVariableColor('Form5', default_color)
    changeVariableColor('box-title-5', default_color)
    changeBorderColor('box-5', default_box_border_color)
    
    changeVariableColor('Form6', default_color)
    changeVariableColor('box-title-6', default_color)
    changeBorderColor('box-6', default_box_border_color)
    
    changeVariableColor('Form7', default_color)
    changeVariableColor('box-title-7', default_color)
    changeBorderColor('box-7', default_box_border_color)
    
    switch(
      input$variable,
      'totale_casi' = {
        changeVariableColor('Form1', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
        changeVariableColor('box-title-1', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
        changeBorderColor('box-1', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'totale_casi', "field_color"])
      },
      'nuovi_positivi' = {
        changeVariableColor('Form1', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
        changeVariableColor('box-title-1', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
        changeBorderColor('box-1', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
      },
      'dimessi_guariti' = {
        changeVariableColor('Form2', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
        changeVariableColor('box-title-2', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
        changeBorderColor('box-2', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
      },
      'nuovi_dimessi_guariti' = {
        changeVariableColor('Form2', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
        changeVariableColor('box-title-2', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
        changeBorderColor('box-2', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'nuovi_dimessi_guariti', "field_color"])
      },
      'deceduti' = {
        changeVariableColor('Form3', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
        changeVariableColor('box-title-3', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
        changeBorderColor('box-3', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
      },
      'nuovi_deceduti' = {
        changeVariableColor('Form3', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
        changeVariableColor('box-title-3', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
        changeBorderColor('box-3', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
      },
      'totale_positivi' = {
        changeVariableColor('Form4', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
        changeVariableColor('box-title-4', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
        changeBorderColor('box-4', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
      },
      'isolamento_domiciliare' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
        changeVariableColor('box-title-5', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
        changeBorderColor('box-5', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
      },
      'totale_ospedalizzati' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
        changeVariableColor('box-title-5', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
        changeBorderColor('box-5', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
      },
      'ricoverati_con_sintomi' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
        changeVariableColor('box-title-5', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
        changeBorderColor('box-5', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
      },
      'terapia_intensiva' = {
        changeVariableColor('Form5', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
        changeVariableColor('box-title-5', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
        changeBorderColor('box-5', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
      },
      'positivi_tamponi' = {
        changeVariableColor('Form6', var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
        changeVariableColor('box-title-6', var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
        changeBorderColor('box-6', var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'positivi_tamponi', "field_color"])
      },
      'tamponi' = {
        changeVariableColor('Form7', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
        changeVariableColor('box-title-7', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
        changeBorderColor('box-7', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
      },
      'nuovi_tamponi' = {
        changeVariableColor('Form7', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
        changeVariableColor('box-title-7', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
        changeNavBarColor(var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
        changeBorderColor('box-7', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
        changeBorderColor('mapBox', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
      }
    )
    
  })
  
}


shinyApp(ui, server)
