require(shinydashboard)
require(tidyverse)
require(shiny)
require(shinythemes)
require(plotly)
require(sf)
require(shinyWidgets)
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
        withSpinner(
          plotlyOutput(outputId = "selMap", height = 300),
          type = 4,
          size = 0.6
        ),
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
        "selectChartTypeCurrDistr",
        label = "Tipo grafico distribuzione:",
        choices = list("Sunbarst chart" = 'S', "Horizontal bar" = 'H'),
        selected = 'H'
      ),
      selectInput(
        "selectChartTimeCurrDistr",
        label = "Tipo grafico distribuzione nel tempo:",
        choices = list("Stacked bar chart" = 'S', "Line chart" = 'L'),
        selected = 'S'
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
  tags$script(src = "myscript.js"),
  
  tabItems(
    tabItem(tabName = "info",
            div(style = "margin-top:-2em", includeMarkdown("info.md"))),
    
    tabItem(
      tabName = "italy-tb",
      fluidRow(
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'totale_casi', 'description'],
          withSpinner(
            uiOutput("CumCases"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varCumCases"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = textOutput("healedTitle"),
          withSpinner(
            uiOutput("CumHealed"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varCumHealed"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = textOutput("deathsTitle"),
          withSpinner(
            uiOutput("Deaths"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varDeaths"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'nuovi_positivi', 'description'],
          withSpinner(
            uiOutput("NewCases"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varNewCases"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'tasso_letalita', 'description'],
          withSpinner(
            uiOutput("LethRate"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varLethRate"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = textOutput("swabsTitle"),
          withSpinner(
            uiOutput("Swabs"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varSwabs"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'totale_positivi', 'description'],
          withSpinner(
            uiOutput("CurrCases"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varCurrCases"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'isolamento_domiciliare', 'description'],
          withSpinner(
            uiOutput("CurrHomeIs"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varCurrHomeIs"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', 'description'],
          withSpinner(
            uiOutput("CurrHospSympt"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varCurrHospSympt"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'terapia_intensiva', 'description'],
          withSpinner(
            uiOutput("CurrIntCare"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varCurrIntCare"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'totale_ospedalizzati', 'description'],
          withSpinner(
            uiOutput("CurrHosp"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varCurrHosp"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        ),
        box(
          width = 2,
          height = 150,
          title = var_descrip[var_descrip$field_name == 'casi_testati', 'description'],
          withSpinner(
            uiOutput("TestedCases"),
            type = 7,
            size = 0.6,
            proxy.height = "43px"
          ),
          withSpinner(
            uiOutput("varTestedCases"),
            type = 7,
            size = 0.3,
            proxy.height = "43px"
          )
        )
      )
      ,
      fluidRow(column(5,
                      fluidRow(
                        box(
                          width = 12,
                          height = 950,
                          title = textOutput('mapTitle'),
                          withSpinner(plotlyOutput(outputId = "mapRegDistr", height = 880), type = 4)
                        )
                      )),
               column(
                 7,
                 fluidRow(
                   box(
                     width = 6,
                     title = textOutput('titleCumCasesDistr'),
                     withSpinner(
                       plotlyOutput(outputId = "CumCasesDistr"),
                       type = 4,
                       size = 0.7
                     )
                   ),
                   box(
                     width = 6,
                     title = textOutput('title2'),
                     withSpinner(
                       plotlyOutput(outputId = "CurrPosDistr"),
                       type = 4,
                       size = 0.7
                     )
                   )
                 ),
                 fluidRow(tabBox(
                   width = 12,
                   id = "tabset",
                   tabPanel(
                     "Distribuzione totale casi",
                     withSpinner(
                       plotlyOutput(outputId = "timeCumCasesDistr"),
                       type = 4,
                       size = 0.7
                     )
                   )
                   ,
                   tabPanel(
                     "Distribuzione positivi",
                     withSpinner(
                       plotlyOutput(outputId = "timePosDistrib"),
                       type = 4,
                       size = 0.7
                     )
                   )
                   ,
                   tabPanel("Serie storica",
                            withSpinner(
                              plotlyOutput(outputId = "timeSeries"),
                              type = 4,
                              size = 0.7
                            ))
                   
                 ))
               )),
      fluidRow(box(
        width = 12,
        withSpinner(
          dataTableOutput(outputId = "deTable"),
          type = 4,
          size = 0.7
        )
      ))
      
      
    )
  )
)

ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "Covid-19 Dashboard"),
                    sidebar,
                    body)

server <- function(input, output, session) {
  output$CumCases <-
    renderUI(div(
      class = 'valText',
      prettyNum(
        summary()$totale_casi[1],
        big.mark = ".",
        decimal.mark = ","
      )
    ))
  
  output$varCumCases <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$totale_casi[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$totale_casi[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$healedTitle <-
    renderText(paste(var_descrip[var_descrip$field_name ==
                                   'dimessi_guariti', "description"]))
  
  output$CumHealed <-
    renderUI(div(
      class = 'valText',
      prettyNum(
        summary()$dimessi_guariti[1],
        big.mark = ".",
        decimal.mark = ","
      )
    ))
  
  output$varCumHealed <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$dimessi_guariti[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$dimessi_guariti[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$deathsTitle <-
    renderText(paste(var_descrip[var_descrip$field_name ==
                                   'deceduti', "description"]))
  
  output$Deaths <-
    renderUI(div(
      class = 'valText',
      prettyNum(
        summary()$deceduti[1],
        big.mark = ".",
        decimal.mark = ","
      )
    ))
  
  output$varDeaths <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$deceduti[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$deceduti[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$NewCases <-
    renderUI(div(
      class = 'valText',
      prettyNum(
        summary()$nuovi_positivi[1],
        big.mark = ".",
        decimal.mark = ","
      )
    ))
  
  output$varNewCases <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$nuovi_positivi[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$nuovi_positivi[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$LethRate <- renderUI(div(class = 'valText', paste0(
    prettyNum(
      summary()$tasso_letalita[1],
      big.mark = ".",
      decimal.mark = ","
    ),
    '%'
  )))
  
  output$varLethRate <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$tasso_letalita[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$tasso_letalita[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$swabsTitle <-
    renderText(paste(var_descrip[var_descrip$field_name ==
                                   'tamponi', "description"]))
  
  output$Swabs <-
    renderUI(div(
      class = 'valText',
      prettyNum(
        summary()$tamponi[1],
        big.mark = ".",
        decimal.mark = ","
      )
    ))
  
  output$varSwabs <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$tamponi[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(summary()$tamponi[2], big.mark = " "),
    "%"
  )))
  
  output$CurrCases <-
    renderUI(div(
      class = 'valText',
      prettyNum(
        summary()$totale_positivi[1],
        big.mark = ".",
        decimal.mark = ","
      )
    ))
  
  output$varCurrCases <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$totale_positivi[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$totale_positivi[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$CurrHomeIs <- renderUI(div(
    class = 'valText',
    prettyNum(
      summary()$isolamento_domiciliare[1],
      big.mark = ".",
      decimal.mark = ","
    )
  ))
  
  output$varCurrHomeIs <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$isolamento_domiciliare[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$isolamento_domiciliare[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$CurrHospSympt <- renderUI(div(
    class = 'valText',
    prettyNum(
      summary()$ricoverati_con_sintomi[1],
      big.mark = ".",
      decimal.mark = ","
    )
  ))
  
  output$varCurrHospSympt <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$ricoverati_con_sintomi[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$ricoverati_con_sintomi[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$CurrIntCare <- renderUI(div(
    class = 'valText',
    prettyNum(
      summary()$terapia_intensiva[1],
      big.mark = ".",
      decimal.mark = ","
    )
  ))
  
  output$varCurrIntCare <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$terapia_intensiva[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$terapia_intensiva[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$CurrHosp <- renderUI(div(
    class = 'valText',
    prettyNum(
      summary()$totale_ospedalizzati[1],
      big.mark = ".",
      decimal.mark = ","
    )
  ))
  
  output$varCurrHosp <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$totale_ospedalizzati[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$totale_ospedalizzati[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  output$TestedCases <-
    renderUI(div(
      class = 'valText',
      prettyNum(
        summary()$casi_testati[1],
        big.mark = ".",
        decimal.mark = ","
      )
    ))
  
  output$varTestedCases <- renderUI(div(class = 'varText', paste0(
    ifelse(
      prettyNum(
        summary()$casi_testati[2],
        big.mark = ".",
        decimal.mark = ","
      ) >= 0,
      "+",
      ""
    ),
    prettyNum(
      summary()$casi_testati[2],
      big.mark = ".",
      decimal.mark = ","
    ),
    "%"
  )))
  
  
  observeEvent(input$selectVar, {
    if (input$selectVar == 'nuovi_dimessi') {
      output$healedTitle <- renderText({
        paste(var_descrip[var_descrip$field_name == 'nuovi_dimessi', "description"])
      })
      
      output$CumHealed <- renderUI(div(
        class = 'valText',
        prettyNum(
          summary()$nuovi_dimessi[1],
          big.mark = ".",
          decimal.mark = ","
        )
      ))
      
      output$varCumHealed <- renderUI(div(class = 'varText', paste0(
        ifelse(
          prettyNum(
            summary()$nuovi_dimessi[2],
            big.mark = ".",
            decimal.mark = ","
          ) >= 0,
          "+",
          ""
        ),
        prettyNum(
          summary()$nuovi_dimessi[2],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
    }
    
    
    if (input$selectVar == 'dimessi_guariti') {
      output$healedTitle <- renderText({
        paste(var_descrip[var_descrip$field_name == 'dimessi_guariti', "description"])
      })
      
      output$CumHealed <- renderUI(div(
        class = 'valText',
        prettyNum(
          summary()$dimessi_guariti[1],
          big.mark = ".",
          decimal.mark = ","
        )
      ))
      
      output$varCumHealed <- renderUI(div(class = 'varText', paste0(
        ifelse(
          prettyNum(
            summary()$dimessi_guariti[2],
            big.mark = ".",
            decimal.mark = ","
          ) >= 0,
          "+",
          ""
        ),
        prettyNum(
          summary()$dimessi_guariti[2],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
    }
    
    
    if (input$selectVar == 'nuovi_deceduti') {
      output$deathsTitle <- renderText({
        paste(var_descrip[var_descrip$field_name == 'nuovi_deceduti', "description"])
      })
      
      output$Deaths <- renderUI(div(
        class = 'valText',
        prettyNum(
          summary()$nuovi_deceduti[1],
          big.mark = ".",
          decimal.mark = ","
        )
      ))
      
      output$varDeaths <- renderUI(div(class = 'varText', paste0(
        ifelse(
          prettyNum(
            summary()$nuovi_deceduti[2],
            big.mark = ".",
            decimal.mark = ","
          ) >= 0,
          "+",
          ""
        ),
        prettyNum(
          summary()$nuovi_deceduti[2],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
    }
    
    if (input$selectVar == 'deceduti') {
      output$deathsTitle <- renderText({
        paste(var_descrip[var_descrip$field_name == 'deceduti', "description"])
      })
      
      output$Deaths <-
        renderUI(div(
          class = 'valText',
          prettyNum(
            summary()$deceduti[1],
            big.mark = ".",
            decimal.mark = ","
          )
        ))
      
      output$varDeaths <- renderUI(div(class = 'varText', paste0(
        ifelse(
          prettyNum(
            summary()$deceduti[2],
            big.mark = ".",
            decimal.mark = ","
          ) >= 0,
          "+",
          ""
        ),
        prettyNum(
          summary()$deceduti[2],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
    }
    
    
    if (input$selectVar == 'nuovi_tamponi') {
      output$swabsTitle <- renderText({
        paste(var_descrip[var_descrip$field_name == 'nuovi_tamponi', "description"])
      })
      
      output$Swabs <- renderUI(div(
        class = 'valText',
        prettyNum(
          summary()$nuovi_tamponi[1],
          big.mark = ".",
          decimal.mark = ","
        )
      ))
      
      output$varSwabs <- renderUI(div(class = 'varText', paste0(
        ifelse(
          prettyNum(
            summary()$nuovi_tamponi[2],
            big.mark = ".",
            decimal.mark = ","
          ) >= 0,
          "+",
          ""
        ),
        prettyNum(
          summary()$nuovi_tamponi[2],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
    }
    
    if (input$selectVar == 'tamponi') {
      output$swabsTitle <- renderText({
        paste(var_descrip[var_descrip$field_name == 'tamponi', "description"])
      })
      
      output$Swabs <-
        renderUI(div(
          class = 'valText',
          prettyNum(
            summary()$tamponi[1],
            big.mark = ".",
            decimal.mark = ","
          )
        ))
      
      output$varSwabs <- renderUI(div(class = 'varText', paste0(
        ifelse(
          prettyNum(
            summary()$tamponi[2],
            big.mark = ".",
            decimal.mark = ","
          ) >= 0,
          "+",
          ""
        ),
        prettyNum(
          summary()$tamponi[2],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
    }
    
    if (input$selectVar == 'pos_tamponi') {
      output$swabsTitle <- renderText({
        paste(var_descrip[var_descrip$field_name == 'pos_tamponi', "description"])
      })
      
      output$Swabs <- renderUI(div(class = 'valText', paste0(
        prettyNum(
          summary()$pos_tamponi[1],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
      output$varSwabs <- renderUI(div(class = 'varText', paste0(
        ifelse(
          prettyNum(
            summary()$pos_tamponi[2],
            big.mark = ".",
            decimal.mark = ","
          ) >= 0,
          "+",
          ""
        ),
        prettyNum(
          summary()$pos_tamponi[2],
          big.mark = ".",
          decimal.mark = ","
        ),
        "%"
      )))
      
    }
    
    
    
  })
  
  
  # Sunburst vs horizontal bar chart
  
  output$CumCasesDistr <- renderPlotly({
    req(input$selRefDate)
    draw_distr_chart(
      data = dati_reg_add(),
      selRegions = sel_reg(),
      date = input$selRefDate,
      typeDistr = 'T',
      selChDistrCurr = input$selectChartTypeCurrDistr
    )
  })
  
  output$titleCumCasesDistr <-
    renderText({
      'Distribuzione totale casi'
    })
  
  output$CurrPosDistr <- renderPlotly({
    req(input$selRefDate)
    draw_distr_chart(
      data = dati_reg_add(),
      selRegions = sel_reg(),
      date = input$selRefDate,
      typeDistr = 'P',
      selChDistrCurr = input$selectChartTypeCurrDistr
    )
  })
  
  output$title2 <-
    renderText({
      'Distribuzione attuali positivi'
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
  
  output$selMap <- renderPlotly({
    m <- list(l = 20,
              r = 20,
              b = 10,
              t = 10)
    
    cols <-
      c(
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00",
        "#00ff00"
      )
    
    plot_ly(
      sf_reg(),
      alpha = 1,
      color = ~ DEN_REG,
      colors = cols,
      stroke = I("#666666"),
      span = I(1),
      key = ~ DEN_REG,
      source = 'M'
    ) %>%
      layout(
        plot_bgcolor = '#222d32',
        paper_bgcolor = '#222d32',
        showlegend = F,
        margin = m,
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
    
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
  
  
  dati_reg <- reactive({
    load_dati_reg(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")
  })
  
  dati_residenti <- reactive({
    load_dati_res(path = "residenti2019.csv")
  })
  
  dati_reg_add <- reactive({
    load_dati_reg_add(dati_reg = dati_reg(), dati_res_reg = dati_residenti())
  })
  
  output$selRefDate <- renderUI({
    req(dati_reg_add())
    sliderInput(
      inputId = "selRefDate",
      label = "Data di riferimento:",
      min = min(dati_reg_add()$data),
      max = max(dati_reg_add()$data),
      value = max(dati_reg_add()$data)
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
        data = dati_reg_add(),
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
        data = dati_reg_add(),
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
  
  summary <- reactive({
    req(input$selRefDate)
    load_summary(
      data = dati_reg_add(),
      selRegions = sel_reg(),
      date = input$selRefDate
    )
  })
  
  
  # Map: clicks observer
  
  observeEvent(event_data("plotly_click", "M", priority = "event"), {
    reg <- event_data("plotly_click", "M")$key
    
    if (length(sel_reg()) == 20) {
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
    
    cols <-
      ifelse(sf_reg()$DEN_REG %in% sel_reg(), "#00ff00", '#00ff00')
    
    plotlyProxy("selMap", session) %>%
      plotlyProxyInvoke("restyle", list(fillcolor = cols))
    
  })
  
  
  observeEvent(input$selectVar, {
    changeTxtColor('CumCases', '#FFFFFF')
    changeTxtColor('CumHealed', '#FFFFFF')
    changeTxtColor('Deaths', '#FFFFFF')
    changeTxtColor('NewCases', '#FFFFFF')
    changeTxtColor('LethRate', '#FFFFFF')
    changeTxtColor('Swabs', '#FFFFFF')
    changeTxtColor('CurrCases', '#FFFFFF')
    changeTxtColor('CurrHomeIs', '#FFFFFF')
    changeTxtColor('CurrHospSympt', '#FFFFFF')
    changeTxtColor('CurrIntCare', '#FFFFFF')
    changeTxtColor('CurrHosp', '#FFFFFF')
    changeTxtColor('TestedCases', '#FFFFFF')
    
    
    switch(
      input$selectVar,
      'totale_casi' = {
        changeTxtColor('CumCases', var_descrip[var_descrip$field_name == input$selectVar, "field_color"])
      },
      'dimessi_guariti' = {
        changeTxtColor('CumHealed', var_descrip[var_descrip$field_name == 'dimessi_guariti', "field_color"])
      },
      'nuovi_dimessi' = {
        changeTxtColor('CumHealed', var_descrip[var_descrip$field_name == 'nuovi_dimessi', "field_color"])
      },
      'deceduti' = {
        changeTxtColor('Deaths', var_descrip[var_descrip$field_name == 'deceduti', "field_color"])
      },
      'nuovi_deceduti' = {
        changeTxtColor('Deaths', var_descrip[var_descrip$field_name == 'nuovi_deceduti', "field_color"])
      },
      'nuovi_positivi' = {
        changeTxtColor('NewCases', var_descrip[var_descrip$field_name == 'nuovi_positivi', "field_color"])
      },
      'tasso_letalita' = {
        changeTxtColor('LethRate', var_descrip[var_descrip$field_name == 'tasso_letalita', "field_color"])
      },
      'tamponi' = {
        changeTxtColor('Swabs', var_descrip[var_descrip$field_name == 'tamponi', "field_color"])
      },
      'nuovi_tamponi' = {
        changeTxtColor('Swabs', var_descrip[var_descrip$field_name == 'nuovi_tamponi', "field_color"])
      },
      'pos_tamponi' = {
        changeTxtColor('Swabs', var_descrip[var_descrip$field_name == 'pos_tamponi', "field_color"])
      },
      'totale_positivi' = {
        changeTxtColor('CurrCases', var_descrip[var_descrip$field_name == 'totale_positivi', "field_color"])
      },
      'isolamento_domiciliare' = {
        changeTxtColor('CurrHomeIs', var_descrip[var_descrip$field_name == 'isolamento_domiciliare', "field_color"])
      },
      'ricoverati_con_sintomi' = {
        changeTxtColor('CurrHospSympt', var_descrip[var_descrip$field_name == 'ricoverati_con_sintomi', "field_color"])
      },
      'terapia_intensiva' = {
        changeTxtColor('CurrIntCare', var_descrip[var_descrip$field_name == 'terapia_intensiva', "field_color"])
      },
      'totale_ospedalizzati' = {
        changeTxtColor('CurrHosp', var_descrip[var_descrip$field_name == 'totale_ospedalizzati', "field_color"])
      },
      'casi_testati' = {
        changeTxtColor('TestedCases', var_descrip[var_descrip$field_name == 'casi_testati', "field_color"])
      }
    )
    
    
  })
  
  
  changeTxtColor <- function(id, rgb) {
    session$sendCustomMessage(type = 'changeTxtColor',
                              message = list('id' = id, 'color' = rgb))
  }
  
}

shinyApp(ui, server)
