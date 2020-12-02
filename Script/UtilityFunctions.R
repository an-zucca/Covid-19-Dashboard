

var_descrip <- data.frame(
  field_name = c(
    "totale_casi",
    "dimessi_guariti",
    "nuovi_dimessi",
    "deceduti",
    "nuovi_deceduti",
    "nuovi_positivi",
    "pos_tamponi",
    "tasso_letalita",
    "tamponi",
    "nuovi_tamponi",
    "totale_positivi",
    "isolamento_domiciliare",
    "ricoverati_con_sintomi",
    "terapia_intensiva",
    "totale_ospedalizzati",
    "casi_testati"
  ),
  description = c(
    "Totale casi",
    "Dimessi/Guariti",
    "Nuovi dimessi/guariti",
    "Deceduti",
    "Nuovi deceduti",
    "Nuovi positivi",
    "Positivi/Tamponi",
    "Tasso di letalita",
    "Tamponi",
    "Nuovi tamponi",
    "Attuali positivi",
    "Isolamento domiciliare",
    "Ricoverati con sintomi",
    "Terapia intensiva",
    "Totale ospedalizzati",
    "Casi testati"
  ),
  field_color = c(
    "#fe00ce",
    "#86ce00",
    "#86ce00",
    "#9d9da5",
    "#9d9da5",
    "#0df9ff",
    "#22ffa7",
    "#b68e00",
    "#22ffa7",
    "#22ffa7",
    "#d626ff",
    "#f6f926",
    "#ff9616",
    "#fd3216",
    "#00b5f7",
    "#fed4c4"
  ),
  stringsAsFactors = FALSE
)


regions <-
  list(
    "Abruzzo",
    "Basilicata",
    "Calabria",
    "Campania",
    "Emilia-Romagna",
    "Friuli Venezia Giulia",
    "Lazio",
    "Liguria",
    "Lombardia",
    "Marche",
    "Molise",
    "Piemonte",
    "Puglia",
    "Sardegna",
    "Sicilia",
    "Toscana",
    "Trentino-Alto Adige",
    "Umbria",
    "Valle d'Aosta",
    "Veneto"
  )

# Read regional data

load_dati_reg <- function(path) {
  dati_reg <-
    data.table::fread(input = path)
  
  dati_reg <- dati_reg %>%
    select(-lat,-long,-variazione_totale_positivi,-note) %>%
    mutate(
      data = as.Date(strptime(data, format = "%Y-%m-%dT%H:%M:%S", tz = 'UTC')),
      denominazione_regione = ifelse(
        denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"),
        "Trentino-Alto Adige",
        denominazione_regione
      ),
      codice_regione = ifelse(
        denominazione_regione %in% "Trentino-Alto Adige",
        4,
        codice_regione
      )
    ) %>%
    group_by(data, codice_regione, denominazione_regione) %>%
    summarise(
      ricoverati_con_sintomi = sum(ricoverati_con_sintomi),
      terapia_intensiva = sum(terapia_intensiva),
      totale_ospedalizzati = sum(totale_ospedalizzati),
      isolamento_domiciliare = sum(isolamento_domiciliare),
      totale_positivi = sum(totale_positivi),
      nuovi_positivi = abs(sum(nuovi_positivi)),
      dimessi_guariti = sum(dimessi_guariti),
      deceduti = sum(deceduti),
      casi_da_sospetto_diagnostico = sum(casi_da_sospetto_diagnostico),
      casi_da_screening = sum(casi_da_screening),
      totale_casi = sum(totale_casi),
      tamponi = sum(tamponi),
      casi_testati = sum(casi_testati)
    ) %>% ungroup()
  
  return(dati_reg)
}


# Read residents data

load_dati_res <- function(path) {
  dati_res <-
    read.csv(file = 'residenti2019_clean.csv',
             stringsAsFactors = FALSE,
             sep = ';')
  
  colnames(dati_res) <- c('territorio', 'totale')
  
  return(dati_res)
}


# Add data

load_dati_reg_add <- function(dati_reg, dati_res_reg) {
  dati_reg <-
    dati_reg %>% mutate(tasso_letalita = round((deceduti / totale_casi) * 100, 3))
  
  
  dati_r <- dati_reg[0, ]
  dati_r$nuovi_deceduti <- 0
  dati_r$nuovi_dimessi <- 0
  dati_r$nuovi_tamponi <- 0
  
  for (reg in regions) {
    sd <- dati_reg %>% filter(denominazione_regione == reg)
    
    
    sd <- sd %>% mutate(
      nuovi_deceduti = abs(c(0, diff(deceduti))),
      nuovi_dimessi = abs(c(0, diff(
        dimessi_guariti
      ))),
      nuovi_tamponi = abs(c(0, diff(tamponi)))
    )
    
    
    dati_r <- rbind(dati_r, sd)
    
    
  }
  
  dati_reg <- dati_r
  
  dati_reg <-
    dati_reg %>% mutate(pos_tamponi = round((nuovi_positivi / nuovi_tamponi) *
                                              100, 3))
  
  
  colnames(dati_res_reg) <-
    c('denominazione_regione', 'num_abitanti')
  
  dati_reg_add = left_join(dati_reg, dati_res_reg)
  
  return(dati_reg_add)
}

# Load summary

load_summary <- function (data, selRegions, date) {
  summary <-
    data %>% filter(data %in% c(date, date - 1) &
                      denominazione_regione %in% selRegions) %>%
    group_by(data) %>% summarise(
      totale_casi = sum(totale_casi),
      dimessi_guariti = sum(dimessi_guariti),
      nuovi_dimessi = sum(nuovi_dimessi),
      deceduti = sum(deceduti),
      nuovi_deceduti = sum(nuovi_deceduti),
      nuovi_positivi = sum(nuovi_positivi),
      tamponi = sum(tamponi),
      nuovi_tamponi = sum(nuovi_tamponi),
      totale_positivi = sum(totale_positivi),
      isolamento_domiciliare = sum(isolamento_domiciliare),
      ricoverati_con_sintomi = sum(ricoverati_con_sintomi),
      terapia_intensiva = sum(terapia_intensiva),
      totale_ospedalizzati = sum(totale_ospedalizzati),
      casi_testati = sum(casi_testati)
    ) %>% ungroup() %>%
    mutate(tasso_letalita = round((deceduti / totale_casi) * 100, 3),
           pos_tamponi = round((nuovi_positivi / nuovi_tamponi) * 100, 3))
  
  curr_totale_casi <- summary[summary$data == date, "totale_casi"]
  incr_totale_casi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "totale_casi"] - summary[summary$data == (date -
                                                                                                               1), "totale_casi"]) / summary[summary$data == (date - 1), "totale_casi"] *
                                      100, 3), 0)
  
  curr_dimessi_guariti <-
    summary[summary$data == date, "dimessi_guariti"]
  incr_dimessi_guariti <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "dimessi_guariti"] - summary[summary$data == (date -
                                                                                                                   1), "dimessi_guariti"]) / summary[summary$data == (date - 1), "dimessi_guariti"] *
                                      100, 3), 0)
  
  curr_nuovi_dimessi <-
    summary[summary$data == date, "nuovi_dimessi"]
  incr_nuovi_dimessi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "nuovi_dimessi"] - summary[summary$data == (date -
                                                                                                                 1), "nuovi_dimessi"]) / summary[summary$data == (date - 1), "nuovi_dimessi"] *
                                      100, 3), 0)
  
  curr_deceduti <- summary[summary$data == date, "deceduti"]
  incr_deceduti <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "deceduti"] - summary[summary$data == (date -
                                                                                                            1), "deceduti"]) / summary[summary$data == (date - 1), "deceduti"] * 100, 3), 0)
  
  
  curr_nuovi_deceduti <-
    summary[summary$data == date, "nuovi_deceduti"]
  incr_nuovi_deceduti <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "nuovi_deceduti"] - summary[summary$data == (date -
                                                                                                                  1), "nuovi_deceduti"]) / summary[summary$data == (date - 1), "nuovi_deceduti"] *
                                      100, 3), 0)
  
  
  curr_nuovi_positivi <-
    summary[summary$data == date, "nuovi_positivi"]
  incr_nuovi_positivi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "nuovi_positivi"] - summary[summary$data == (date -
                                                                                                                  1), "nuovi_positivi"]) / summary[summary$data == (date - 1), "nuovi_positivi"] *
                                      100, 3), 0)
  
  curr_tamponi <- summary[summary$data == date, "tamponi"]
  incr_tamponi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "tamponi"] - summary[summary$data == (date -
                                                                                                           1), "tamponi"]) / summary[summary$data == (date - 1), "tamponi"] * 100, 3), 0)
  
  curr_nuovi_tamponi <-
    summary[summary$data == date, "nuovi_tamponi"]
  incr_nuovi_tamponi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "nuovi_tamponi"] - summary[summary$data == (date -
                                                                                                                 1), "nuovi_tamponi"]) / summary[summary$data == (date - 1), "nuovi_tamponi"] *
                                      100, 3), 0)
  
  curr_totale_positivi <-
    summary[summary$data == date, "totale_positivi"]
  incr_totale_positivi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "totale_positivi"] - summary[summary$data == (date -
                                                                                                                   1), "totale_positivi"]) / summary[summary$data == (date - 1), "totale_positivi"] *
                                      100, 3), 0)
  
  curr_isolamento_domiciliare <-
    summary[summary$data == date, "isolamento_domiciliare"]
  incr_isolamento_domiciliare <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "isolamento_domiciliare"] - summary[summary$data == (date -
                                                                                                                          1), "isolamento_domiciliare"]) / summary[summary$data == (date - 1), "isolamento_domiciliare"] *
                                      100, 3), 0)
  
  curr_ricoverati_con_sintomi <-
    summary[summary$data == date, "ricoverati_con_sintomi"]
  incr_ricoverati_con_sintomi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "ricoverati_con_sintomi"] - summary[summary$data == (date -
                                                                                                                          1), "ricoverati_con_sintomi"]) / summary[summary$data == (date - 1), "ricoverati_con_sintomi"] *
                                      100, 3), 0)
  
  curr_terapia_intensiva <-
    summary[summary$data == date, "terapia_intensiva"]
  incr_terapia_intensiva <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "terapia_intensiva"] - summary[summary$data == (date -
                                                                                                                     1), "terapia_intensiva"]) / summary[summary$data == (date - 1), "terapia_intensiva"] *
                                      100, 3), 0)
  
  curr_totale_ospedalizzati <-
    summary[summary$data == date, "totale_ospedalizzati"]
  incr_totale_ospedalizzati <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "totale_ospedalizzati"] - summary[summary$data == (date -
                                                                                                                        1), "totale_ospedalizzati"]) / summary[summary$data == (date - 1), "totale_ospedalizzati"] *
                                      100, 3), 0)
  
  curr_casi_testati <- summary[summary$data == date, "casi_testati"]
  incr_casi_testati <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "casi_testati"] - summary[summary$data == (date -
                                                                                                                1), "casi_testati"]) / summary[summary$data == (date - 1), "casi_testati"] *
                                      100, 3), 0)
  
  curr_tasso_letalita <-
    summary[summary$data == date, "tasso_letalita"]
  incr_tasso_letalita <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "tasso_letalita"] - summary[summary$data == (date -
                                                                                                                  1), "tasso_letalita"]) / summary[summary$data == (date - 1), "tasso_letalita"] *
                                      100, 3), 0)
  
  curr_pos_tamponi <- summary[summary$data == date, "pos_tamponi"]
  incr_pos_tamponi <-
    ifelse(nrow(summary) > 1, round((summary[summary$data == date, "pos_tamponi"] - summary[summary$data == (date -
                                                                                                               1), "pos_tamponi"]) / summary[summary$data == (date - 1), "pos_tamponi"] *
                                      100, 3), 0)
  
  
  summary <-
    list(
      'totale_casi' = c(curr_totale_casi, incr_totale_casi),
      'dimessi_guariti' = c(curr_dimessi_guariti, incr_dimessi_guariti),
      'nuovi_dimessi' = c(curr_nuovi_dimessi, incr_nuovi_dimessi),
      'deceduti' = c(curr_deceduti, incr_deceduti),
      'nuovi_deceduti' = c(curr_nuovi_deceduti, incr_nuovi_deceduti),
      'nuovi_positivi' = c(curr_nuovi_positivi, incr_nuovi_positivi),
      'tamponi' = c(curr_tamponi, incr_tamponi),
      'nuovi_tamponi' = c(curr_nuovi_tamponi, incr_nuovi_tamponi),
      'pos_tamponi' = c(curr_pos_tamponi, incr_pos_tamponi),
      'totale_positivi' = c(curr_totale_positivi, incr_totale_positivi),
      'isolamento_domiciliare' = c(curr_isolamento_domiciliare, incr_isolamento_domiciliare),
      'ricoverati_con_sintomi' = c(curr_ricoverati_con_sintomi, incr_ricoverati_con_sintomi),
      'terapia_intensiva' = c(curr_terapia_intensiva, incr_terapia_intensiva),
      'totale_ospedalizzati' = c(curr_totale_ospedalizzati, incr_totale_ospedalizzati),
      'casi_testati' = c(curr_casi_testati, incr_casi_testati),
      'tasso_letalita' = c(curr_tasso_letalita, incr_tasso_letalita)
    )
  
  return(summary)
  
}


load_map_reg <- function(path) {
  data <- st_read(path, stringsAsFactors = FALSE)
  data <- data[order(data$DEN_REG), ]
  return(data)
}


draw_distr_chart <-
  function(data,
           selRegions,
           date,
           typeDistr,
           selChDistrCurr) {
    if (typeDistr == 'T') {
      var <-
        c('totale_casi',
          'dimessi_guariti',
          'deceduti',
          'totale_positivi')
    } else if (typeDistr == 'P') {
      var <-
        c(
          'totale_positivi',
          'isolamento_domiciliare',
          'ricoverati_con_sintomi',
          'terapia_intensiva'
        )
    }
    
    summary <-
      data %>% filter(data %in% c(date) &
                        denominazione_regione %in% selRegions) %>%
      group_by(data) %>% summarise(
        var1 = sum(!!(sym(var[1]))),
        var2 = sum(!!(sym(var[2]))),
        var3 = sum(!!(sym(var[3]))),
        var4 = sum(!!(sym(var[4])))
      ) %>% ungroup() %>% select(-data)
    
    if (selChDistrCurr == 'S') {
      labs <-
        c(var_descrip[var_descrip$field_name == var[1], "description"],
          var_descrip[var_descrip$field_name == var[2], "description"],
          var_descrip[var_descrip$field_name == var[3], "description"],
          var_descrip[var_descrip$field_name == var[4], "description"])
      
      pars <- c("",
                var_descrip[var_descrip$field_name == var[1], "description"],
                var_descrip[var_descrip$field_name == var[1], "description"],
                var_descrip[var_descrip$field_name == var[1], "description"])
      
      vals <- as.numeric(summary)
      
      cols <-
        c(var_descrip[var_descrip$field_name == var[1], "field_color"],
          var_descrip[var_descrip$field_name == var[2], "field_color"],
          var_descrip[var_descrip$field_name == var[3], "field_color"],
          var_descrip[var_descrip$field_name == var[4], "field_color"])
      
      
      fig <- plot_ly(
        labels = labs,
        parents = pars,
        values = vals,
        type = 'sunburst',
        branchvalues = 'total',
        textinfo = 'label+percent parent',
        insidetextfont = list(color = '#fff'),
        outsidetextfont = list(color = '#fff'),
        marker = list(colors = cols),
        alpha = 1
      ) %>%
        layout(
          showlegend = T,
          paper_bgcolor = 'rgba(34,34,34,1)',
          margin = list(
            l = 30,
            r = 30,
            b = 10,
            t = 10
          )
        )
    } else if (selChDistrCurr == 'H') {
      x_sav <-
        c(var_descrip[var_descrip$field_name == var[2], "description"],
          var_descrip[var_descrip$field_name == var[3], "description"],
          var_descrip[var_descrip$field_name == var[4], "description"])
      
      y_sav <- as.numeric(summary[1, c(2:4)])
      
      c_color <-
        c(var_descrip[var_descrip$field_name == var[2], "field_color"],
          var_descrip[var_descrip$field_name == var[3], "field_color"],
          var_descrip[var_descrip$field_name == var[4], "field_color"])
      
      data <- data.frame(xx = x_sav,
                         yy = y_sav,
                         cc = c_color)
      
      f1 <-
        data[data$xx == var_descrip[var_descrip$field_name == var[2], "description"], ]
      f1 <- f1 %>% mutate(pp =  yy / summary[1, 'var1'])
      
      f2 <-
        data[data$xx == var_descrip[var_descrip$field_name == var[3], "description"], ]
      f2 <- f2 %>% mutate(pp =  yy / summary[1, 'var1'])
      
      f3 <-
        data[data$xx == var_descrip[var_descrip$field_name == var[4], "description"], ]
      f3 <- f3 %>% mutate(pp =  yy / summary[1, 'var1'])
      
      fig <- data %>%
        plot_ly(
          x = ~ reorder(xx, yy),
          y = ~ yy,
          type = "bar",
          marker = list(color = ~ cc)
        )
      
      fig <- fig %>% add_annotations(
        xref = 'x',
        yref = 'y',
        x = f1$xx,
        y = f1$yy + max(y_sav) / 10,
        text = paste(round(f1$pp * 100, 3), '%'),
        font = list(color = f1$cc),
        showarrow = FALSE
      )
      
      fig <- fig %>% add_annotations(
        xref = 'x',
        yref = 'y',
        x = f2$xx,
        y = f2$yy + max(y_sav) / 10,
        text = paste(round(f2$pp * 100, 3), '%'),
        font = list(color =  f2$cc),
        showarrow = FALSE
      )
      
      fig <- fig %>% add_annotations(
        xref = 'x',
        yref = 'y',
        x = f3$xx,
        y = f3$yy + max(y_sav) / 10,
        text = paste(round(f3$pp * 100, 3), '%'),
        font = list(color = f3$cc),
        showarrow = FALSE
      )
      
      fig %>% layout(
        xaxis = list(title = '', tickangle = 45),
        yaxis = list(title = ''),
        plot_bgcolor = 'rgb(34,34,34)',
        paper_bgcolor = 'rgb(34,34,34)',
        font = list(color = 'white'),
        legend = list(x = 0, y = 1)
      )
    }
    
    
  }


draw_distr_time_chart <-
  function(data,
           selRegions,
           date,
           typeDistr,
           selChDistrCurr) {
    if (typeDistr == 'T') {
      var <- c('totale_positivi', 'dimessi_guariti', 'deceduti')
    } else if (typeDistr == 'P') {
      var <-
        c('terapia_intensiva',
          'ricoverati_con_sintomi',
          'isolamento_domiciliare')
    }
    
    summary <-
      data %>% filter(data <= date &
                        denominazione_regione %in% selRegions) %>%
      group_by(data) %>% summarise(var1 = sum(!!(sym(var[1]))),
                                   var2 = sum(!!(sym(var[2]))),
                                   var3 = sum(!!(sym(var[3])))) %>% ungroup()
    
    if (selChDistrCurr == 'S') {
      summary %>%
        plot_ly(
          x = ~ data,
          y = ~ var1,
          name = var_descrip[var_descrip$field_name == var[1], "description"],
          type = "bar",
          color = I(var_descrip[var_descrip$field_name == var[1], "field_color"])
        ) %>%
        add_trace(y = ~ var2,
                  name = var_descrip[var_descrip$field_name == var[2], "description"],
                  color = I(var_descrip[var_descrip$field_name == var[2], "field_color"])) %>%
        add_trace(y = ~ var3,
                  name = var_descrip[var_descrip$field_name == var[3], "description"],
                  color = I(var_descrip[var_descrip$field_name == var[3], "field_color"])) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ''),
          barmode = 'stack',
          plot_bgcolor = 'rgb(34,34,34)',
          paper_bgcolor = 'rgb(34,34,34)',
          font = list(color = 'white'),
          legend = list(x = 0, y = 1)
        )
      
      
    } else if (selChDistrCurr == 'L') {
      summary %>%
        plot_ly() %>%
        add_trace(
          x = ~ data,
          y = ~ var1,
          type = 'scatter',
          mode = 'lines',
          name = var_descrip[var_descrip$field_name == var[1], "description"],
          line = list(width = 2, color = var_descrip[var_descrip$field_name == var[1], "field_color"]),
          fill = 'tozeroy',
          fillcolor = toRGB(var_descrip[var_descrip$field_name == var[1], "field_color"], alpha = 0.05)
        ) %>%
        add_trace(
          x = ~ data,
          y = ~ var2,
          type = 'scatter',
          mode = 'lines',
          name = var_descrip[var_descrip$field_name == var[2], "description"],
          line = list(width = 2, color = var_descrip[var_descrip$field_name == var[2], "field_color"]),
          fill = 'tozeroy',
          fillcolor = toRGB(var_descrip[var_descrip$field_name == var[2], "field_color"], alpha = 0.05)
        ) %>%
        add_trace(
          x = ~ data,
          y = ~ var3,
          type = 'scatter',
          mode = 'lines',
          name = var_descrip[var_descrip$field_name == var[3], "description"],
          line = list(width = 2, color = var_descrip[var_descrip$field_name == var[3], "field_color"]),
          fill = 'tozeroy',
          fillcolor = toRGB(var_descrip[var_descrip$field_name == var[3], "field_color"], alpha = 0.05)
        ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ''),
          plot_bgcolor = 'rgb(34,34,34)',
          paper_bgcolor = 'rgb(34,34,34)',
          font = list(color = 'white'),
          legend = list(x = 0, y = 1)
        )
      
      
    }
  }

draw_time_series_plot <-
  function(data,
           selRegions,
           date,
           var,
           peReg = F,
           ckRisc = F,
           numCasi = NULL) {
    summary <-
      data %>% filter(data <= date &
                        denominazione_regione %in% selRegions)
    
    if (peReg) {
      if (ckRisc) {
        if (var != 'tasso_letalita') {
          p <- plot_ly()
          
          for (reg in selRegions) {
            xvalue = summary[summary$denominazione_regione == reg, ]$data
            
            subSum <-
              summary %>% filter(data == date & denominazione_regione == reg)
            num_abitanti <- subSum$num_abitanti
            
            yvalue = summary[summary$denominazione_regione == reg, ][[var]] /
              (num_abitanti / numCasi)
            
            p <-
              add_trace(
                p,
                x = xvalue,
                y = yvalue,
                type = 'scatter',
                mode = "lines",
                name = reg
              )
          }
          
          p %>%
            layout(
              xaxis = list(title = ""),
              showlegend = T,
              yaxis = list(title = ''),
              plot_bgcolor = 'rgb(34,34,34)',
              paper_bgcolor = 'rgb(34,34,34)',
              font = list(color = 'white')
            )
          
        }
        
      } else {
        p <- plot_ly()
        
        for (reg in selRegions) {
          p <-
            add_trace(
              p,
              x = summary[summary$denominazione_regione == reg, ]$data,
              y = summary[summary$denominazione_regione == reg, ][[var]],
              type = 'scatter',
              mode = "lines",
              name = reg
            )
        }
        
        p %>%
          layout(
            xaxis = list(title = ""),
            showlegend = T,
            yaxis = list(title = ''),
            plot_bgcolor = 'rgb(34,34,34)',
            paper_bgcolor = 'rgb(34,34,34)',
            font = list(color = 'white')
          )
        
      }
      
    } else {
      if (ckRisc) {
        if (var != 'tasso_letalita') {
          summary <- summary %>%
            group_by(data) %>% summarise(totale = sum(!!(sym(var))),
                                         num_abitanti = sum(num_abitanti)) %>% ungroup()
          
          summary <-
            summary %>% mutate(totale = totale / (num_abitanti / numCasi))
          
          summary %>%
            plot_ly(
              x = ~ data,
              y = ~ totale,
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = var_descrip[var_descrip$field_name ==
                                                sym(var), "field_color"]),
              marker = list(color = var_descrip[var_descrip$field_name == sym(var), "field_color"]),
              name = var
            )  %>%
            layout(
              xaxis = list(title = ""),
              showlegend = F,
              yaxis = list(title = ''),
              plot_bgcolor = 'rgb(34,34,34)',
              paper_bgcolor = 'rgb(34,34,34)',
              font = list(color = 'white'),
              legend = list(x = 0, y = 1)
            )
          
        }
        
        
        
      } else {
        if ((var != 'tasso_letalita') & (var != 'pos_tamponi')) {
          summary <- summary %>%
            group_by(data) %>% summarise(totale = sum(!!(sym(var)))) %>% ungroup()
          
        } else {
          if (var == 'tasso_letalita') {
            summary <- summary %>%
              group_by(data) %>% summarise(deceduti = sum(deceduti),
                                           totale_casi = sum(totale_casi)) %>% mutate(totale = round((deceduti /
                                                                                                        totale_casi) * 100, 3))
          }
          
          if (var == 'pos_tamponi') {
            summary <- summary %>%
              group_by(data) %>% summarise(
                nuovi_positivi = sum(nuovi_positivi),
                nuovi_tamponi = sum(nuovi_tamponi)
              ) %>% mutate(totale = round((nuovi_positivi / nuovi_tamponi) *
                                            100, 3))
          }
        }
        
        summary %>%
          plot_ly(
            x = ~ data,
            y = ~ totale,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = var_descrip[var_descrip$field_name ==
                                              sym(var), "field_color"]),
            marker = list(color = var_descrip[var_descrip$field_name == sym(var), "field_color"]),
            name = var
          )  %>%
          layout(
            xaxis = list(title = ""),
            showlegend = F,
            yaxis = list(title = ''),
            plot_bgcolor = 'rgb(34,34,34)',
            paper_bgcolor = 'rgb(34,34,34)',
            font = list(color = 'white'),
            legend = list(x = 0, y = 1)
          )
      }
    }
  }




draw_sel_map <- function(data, selRegions) {
  m <- list(l = 20,
            r = 20,
            b = 10,
            t = 10)
  
  cols <- ifelse(data$DEN_REG %in% selRegions, "#00ff00", '#222222')
  
  plot_ly(
    data,
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
      margin = m
    ) %>%
    config(displayModeBar = FALSE)
  
}



draw_map <-
  function(datasf,
           data,
           selRegions,
           date,
           var,
           MapColrPal,
           ckRisc = F,
           numCasi = NULL) {
    if (ckRisc) {
      summary <-
        data %>% filter(data %in% c(date) &
                          denominazione_regione %in% selRegions)
      
      summary <-
        summary[, c("denominazione_regione", var, 'num_abitanti')]
      
      summary <- summary %>% mutate(num_abitanti_n = num_abitanti / numCasi,
                                    var2 = get(var) / num_abitanti_n) %>% select(-num_abitanti_n)
      
      summary <- summary[, c("denominazione_regione", 'var2')]
      colnames(summary) <- c("denominazione_regione", var)
      
      colnames(summary) <- c("DEN_REG", var)
      
      req <- left_join(datasf, summary)
      req <- req %>% filter(DEN_REG %in% selRegions)
      
      detPal <- switch(MapColrPal,
                       'SH' = {
                         pal <-
                           colorRampPalette(c("#ffffff", var_descrip[var_descrip$field_name == var, "field_color"]),
                                            interpolate = c("linear", "spline"))
                         pal(9)
                       },
                       'V' = {
                         'viridis'
                       },
                       'Y' = {
                         'YlOrRd'
                       })
      
      p <-
        plot_ly(
          data = req,
          split = ~ DEN_REG,
          color = ~ get(var),
          alpha = 1,
          colors = detPal,
          stroke = I("black"),
          text = ~ paste0(DEN_REG, "\n", round(get(var), 2)),
          hoveron = "fills",
          hoverinfo = "text",
          showlegend = F
        ) %>%
        colorbar(title = '', tickfont = list(color = '#FFFFFF')) %>%
        layout(
          plot_bgcolor = 'rgb(34,34,34)',
          paper_bgcolor = 'rgb(34,34,34)',
          margin = list(
            l = 20,
            r = 20,
            b = 10,
            t = 10
          )
        )
    }
    
    else {
      summary <-
        data %>% filter(data %in% c(date) &
                          denominazione_regione %in% selRegions)
      
      summary <- summary[, c("denominazione_regione", var)]
      
      colnames(summary) <- c("DEN_REG", var)
      
      req <- left_join(datasf, summary)
      req <- req %>% filter(DEN_REG %in% selRegions)
      
      detPal <- switch(MapColrPal,
                       'SH' = {
                         pal <-
                           colorRampPalette(c("#ffffff", var_descrip[var_descrip$field_name == var, "field_color"]),
                                            interpolate = c("linear", "spline"))
                         pal(9)
                       },
                       'V' = {
                         'viridis'
                       },
                       'Y' = {
                         'YlOrRd'
                       })
      
      p <-
        plot_ly(
          data = req,
          split = ~ DEN_REG,
          color = ~ get(var),
          alpha = 1,
          colors = detPal,
          stroke = I("black"),
          text = ~ paste0(DEN_REG, "\n", get(var)),
          hoveron = "fills",
          hoverinfo = "text",
          showlegend = F
        ) %>%
        colorbar(title = '', tickfont = list(color = '#FFFFFF')) %>%
        layout(
          plot_bgcolor = 'rgb(34,34,34)',
          paper_bgcolor = 'rgb(34,34,34)',
          margin = list(
            l = 20,
            r = 20,
            b = 10,
            t = 10
          )
        )
      
      
      
    }
  }



calc_table <- function(data, selRegions, date) {
  summary <- data %>%
    filter(data %in% c(date) &
             denominazione_regione %in% selRegions) %>%
    select(denominazione_regione,
           var_descrip$field_name,
           denominazione_regione)
  
  datatable(
    summary,
    rownames = FALSE,
    colnames = c(
      'Regione' = 'denominazione_regione',
      setNames(var_descrip$field_name, var_descrip$description)
    ),
    options = list(
      scrollX = T,
      autoWidth = TRUE,
      columnDefs = list(list(
        className = 'dt-center', targets = "_all"
      ))
    ),
    filter = 'top'
  )
  
}
