var_descrip <- data.frame(
  field_name = c(
    "totale_casi",
    "dimessi_guariti",
    "nuovi_dimessi",
    "deceduti",
    "nuovi_deceduti",
    "nuovi_positivi",
    "tasso_letalita",
    "pos_tamponi",
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
    "Dimessi / Guariti",
    "Nuovi dimessi/guariti",
    "Deceduti",
    "Nuovi deceduti",
    "Nuovi positivi",
    "Tasso di letalità",
    "Positivi / Tamponi",
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
    "#22ffa7",
    "#b68e00",
    "#0df9ff",
    "#FF3384",
    "#FF3384",
    "#d626ff",
    "#f6f926",
    "#ff9616",
    "#fd3216",
    "#00b5f7",
    "#FF3384"
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

swabs_minimum_threshold <- 1000

# Read regional data

load_dati_reg <- function(path) {
  dati_reg <- data.table::fread(input = path)
  
  dati_reg <- dati_reg %>%
    mutate(
      data = as.Date(strptime(data, format = "%Y-%m-%d", tz = 'UTC')),
      denominazione_regione = ifelse(
        denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"),
        "Trentino-Alto Adige",
        denominazione_regione
      ),
      codice_regione = ifelse(
        denominazione_regione %in% "Trentino-Alto Adige",
        4,
        codice_regione
      ),
      casi_testati = ifelse(is.na(casi_testati), 0, casi_testati)
    ) %>%
    group_by(data, codice_regione, denominazione_regione) %>%
    summarise(
      totale_casi = sum(totale_casi),
      dimessi_guariti = sum(dimessi_guariti),
      deceduti = sum(deceduti),
      nuovi_positivi = abs(sum(nuovi_positivi)),
      totale_positivi = sum(totale_positivi),
      isolamento_domiciliare = sum(isolamento_domiciliare),
      ricoverati_con_sintomi = sum(ricoverati_con_sintomi),
      terapia_intensiva = sum(terapia_intensiva),
      totale_ospedalizzati = sum(totale_ospedalizzati),
      casi_testati = sum(casi_testati),
      tamponi = sum(tamponi)
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
  dati_r <- dati_reg[0,]
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
    dati_reg %>% mutate(
      pos_tamponi = ifelse(
        nuovi_positivi == 0 |
          nuovi_tamponi == 0,
        0,
        round(nuovi_positivi / nuovi_tamponi * 100, 3)
      ),
      tasso_letalita = ifelse(
        deceduti == 0 |
          totale_casi == 0,
        0,
        round(deceduti / totale_casi * 100, 3)
      )
    )
  
  
  colnames(dati_res_reg) <-
    c('denominazione_regione', 'num_abitanti')
  
  dati_reg_add = left_join(dati_reg, dati_res_reg)
  
  return(dati_reg_add)
}

perc_increase_calculator <- function (prec_value, curr_value) {
  if (prec_value == 0) {
    if (curr_value == 0) {
      return(0)
    } else {
      return(NA)
    }
  } else {
    if (prec_value == curr_value) {
      return(0)
    }
    else {
      return((curr_value - prec_value) / prec_value * 100)
    }
  }
}

f_ret_basic_summary <- function(prec_record, curr_record, variab) {
  prec_value <- prec_record[[variab]]
  curr_value <- curr_record[[variab]]
  
  diff_value <- curr_value - prec_value
  incr_value <- perc_increase_calculator(prec_value, curr_value)
  
  if (variab %in% c('totale_casi')) {
    # OK
    curr_value_nuovi_tamponi <- curr_record[['nuovi_tamponi']]
    curr_value_nuovi_dimessi <- curr_record[['nuovi_dimessi']]
    curr_value_nuovi_positivi <- curr_record[['nuovi_positivi']]
    
    if (prec_value == curr_value) {
      incr_value_style <-
        ifelse(
          curr_value_nuovi_tamponi < swabs_minimum_threshold,
          # verifico se il mancato incremento dei casi può essere dovuto al basso numero di tamponi
          'color:#ffa500',
          'color:#00ff00'
        )
      
      sign_value <- '+'
      ico_value <- '◉'
      
    } else if (prec_value < curr_value) {
      # c'è stato incremento dei casi rispetto al giorno precedente
      
      if (curr_value_nuovi_tamponi < swabs_minimum_threshold |
          curr_value_nuovi_positivi < curr_value_nuovi_dimessi) {
        incr_value_style <-  'color:#ffa500'
      } else {
        incr_value_style <-  'color:#ff0000'
      }
      
      sign_value <- '+'
      ico_value <- '▲'
    } else {
      # non si dovrebbe presentare (salvo ricalcolo)
      incr_value_style <- 'color:#00ff00'
      sign_value <- ''
      ico_value <- '▼'
    }
    
  } else if (variab == 'dimessi_guariti') {
    # OK
    curr_value_totale_positivi <- curr_record[['totale_positivi']]
    curr_value_nuovi_positivi <- curr_record[['nuovi_positivi']]
    curr_value_nuovi_deceduti <- curr_record[['nuovi_deceduti']]
    curr_value_nuovi_tamponi <- curr_record[['nuovi_tamponi']]
    curr_value_nuovi_dimessi <- curr_record[['nuovi_dimessi']]
    
    if (prec_value == curr_value) {
      # significa che non ci sono stati dimessi
      if (curr_value_nuovi_deceduti > 0) {
        # ci sono stati deceduti
        incr_value_style <- 'color:#ff0000'
      } else if (curr_value_totale_positivi > 0 |
                 curr_value_nuovi_tamponi < swabs_minimum_threshold) {
        # ci sono positivi o non si sta monitorando a sufficienza.
        incr_value_style <- 'color:#ffa500'
      } else {
        incr_value_style <- 'color:#00ff00' # ok
      }
      sign_value <- '+'
      ico_value <- '◉'
    } else if (prec_value < curr_value) {
      # c'è stato incremento dei dimessi
      if ((curr_value_nuovi_deceduti / curr_value_nuovi_dimessi) > 0.03) {
        # se il rapporto tra nuovi deceduti e nuovi dimessi è superiore del 3%
        incr_value_style <- 'color:#ff0000'
      } else if ((curr_value_totale_positivi / curr_value_nuovi_dimessi) > 4) {
        # se il totale dei positivi è 4 volte il numero dei dimessi
        incr_value_style <- 'color:#ffa500'
      } else if (curr_value_nuovi_tamponi < 1000) {
        # Non si sta monitorando a sufficienza (<1000 tamponi)
        incr_value_style <- 'color:#ffa500'
      } else {
        incr_value_style <- 'color:#00ff00' # ok
      }
      sign_value <- '+'
      ico_value <- '▲'
    } else {
      if ((curr_value_nuovi_deceduti / curr_value_nuovi_dimessi) > 0.03) {
        # se il rapporto tra nuovi deceduti e nuovi dimessi è superiore del 3%
        incr_value_style <- 'color:#ff0000'
      } else if ((curr_value_totale_positivi / curr_value_nuovi_dimessi) > 4) {
        # se il totale dei positivi è 4 volte il numero dei dimessi
        incr_value_style <- 'color:#ffa500'
      } else if (curr_value_nuovi_tamponi < 1000) {
        # Non si sta monitorando a sufficienza (<1000 tamponi)
        incr_value_style <- 'color:#ffa500'
      } else {
        incr_value_style <- 'color:#00ff00' # ok
      }
      sign_value <- ''
      ico_value <- '▼'
    }
    
  } else if (variab == 'nuovi_dimessi') {
    curr_value_nuovi_positivi <- curr_record[['nuovi_positivi']]
    curr_value_nuovi_deceduti <- curr_record[['nuovi_deceduti']]
    curr_value_nuovi_tamponi <- curr_record[['nuovi_tamponi']]
    curr_value_nuovi_dimessi <- curr_record[['nuovi_dimessi']]
    
    if (prec_value == curr_value) {
      if (curr_value == 0) {
        # non ci sono nuovi dimessi
        if (curr_value_nuovi_deceduti > 0) {
          # ci sono stati deceduti
          incr_value_style <- 'color:#ff0000'
        } else if (curr_value_nuovi_positivi > curr_value |
                   curr_value_nuovi_tamponi < 1000) {
          # I nuovi positivi superano i dimessi. O si può pensare di gestire il rapporto tra totale positivi e nuovi dimessi per verificare se il tasso di dimissione è sostenibile.
          # Non si sta monitorando a sufficienza (<1000 tamponi)
          incr_value_style <- 'color:#ffa500'
        } else {
          incr_value_style <- 'color:#00ff00'
        }
        sign_value <- '+'
        ico_value <- '◉'
      } else {
        # il numero di dimessi è uguale al giorno precedente
        
        if (curr_value_nuovi_deceduti > curr_value) {
          # i nuovi deceduti superano i nuovi dimessi. Si potrebbe pensare di impostare un valore soglia più basso.
          incr_value_style <- 'color:#ff0000'
        } else if (curr_value_nuovi_positivi > curr_value |
                   curr_value_nuovi_tamponi < 1000) {
          # I nuovi positivi superano i dimessi. O si può pensare di gestire il rapporto tra totale positivi e nuovi dimessi per verificare se il tasso di dimissione è sostenibile.
          # Non si sta monitorando a sufficienza (<1000 tamponi)
          incr_value_style <- 'color:#ffa500'
        } else {
          incr_value_style <- 'color:#00ff00' # ok
        }
        
        sign_value <- '+'
        ico_value <- '◉'
      }
      
    } else if (prec_value < curr_value) {
      if (curr_value_nuovi_deceduti > curr_value) {
        # i nuovi deceduti superano i nuovi dimessi. Si potrebbe pensare di impostare un valore soglia più basso.
        incr_value_style <- 'color:#ff0000'
      } else if (curr_value_nuovi_positivi > curr_value |
                 curr_value_nuovi_tamponi < 1000) {
        # I nuovi positivi superano i dimessi. O si può pensare di gestire il rapporto tra totale positivi e nuovi dimessi per verificare se il tasso di dimissione è sostenibile.
        # Non si sta monitorando a sufficienza (<1000 tamponi)
        incr_value_style <- 'color:#ffa500'
      } else {
        incr_value_style <- 'color:#00ff00' # ok
      }
      sign_value <- '+'
      ico_value <- '▲'
    } else {
      if (curr_value_nuovi_deceduti > curr_value) {
        # i nuovi deceduti superano i nuovi dimessi. Si potrebbe pensare di impostare un valore soglia più basso.
        incr_value_style <- 'color:#ff0000'
      } else if (curr_value_nuovi_positivi > curr_value |
                 curr_value_nuovi_tamponi < 1000) {
        # I nuovi positivi superano i dimessi. O si può pensare di gestire il rapporto tra totale positivi e nuovi dimessi per verificare se il tasso di dimissione è sostenibile.
        # Non si sta monitorando a sufficienza (<1000 tamponi)
        incr_value_style <- 'color:#ffa500'
      } else {
        incr_value_style <- 'color:#00ff00' # ok
      }
      
      sign_value <- ''
      ico_value <- '▼'
    }
  }  else if (variab %in% c('deceduti')) {
    if (prec_value == 0) {
      if (curr_value == 0) {
        incr_value_style <- 'color:#00ff00'
        sign_value <- '+'
        ico_value <- '◉'
      } else {
        incr_value_style <- 'color:#ff0000'
        sign_value <- '+'
        ico_value <- '▲'
      }
    } else {
      if (prec_value == curr_value) {
        incr_value_style <- 'color:#00ff00'
        sign_value <- '+'
        ico_value <- '◉'
      }
      else {
        if (prec_value < curr_value) {
          incr_value_style <- 'color:#ff0000'
          sign_value <- '+'
          ico_value <- '▲'
        } else {
          # non si dovrebbe presentare (salvo ricalcolo)
          incr_value_style <- 'color:#00ff00'
          sign_value <- ''
          ico_value <- '▼'
        }
      }
    }
  }
  else if (variab %in% c(
    'nuovi_deceduti',
    'nuovi_positivi',
    'tasso_letalita',
    'pos_tamponi',
    'totale_positivi',
    'isolamento_domiciliare',
    'ricoverati_con_sintomi',
    'terapia_intensiva',
    'totale_ospedalizzati'
  )) {
    if (prec_value == 0) {
      if (curr_value == 0) {
        incr_value_style <- 'color:#00ff00'
        sign_value <- '+'
        ico_value <- '◉'
      } else {
        incr_value_style <- 'color:#ff0000'
        sign_value <- '+'
        ico_value <- '▲'
      }
    } else {
      if (prec_value == curr_value) {
        incr_value_style <- 'color:#ffa500'
        sign_value <- '+'
        ico_value <- '◉'
      }
      else {
        if (prec_value < curr_value) {
          incr_value_style <- 'color:#ff0000'
          sign_value <- '+'
          ico_value <- '▲'
        } else {
          incr_value_style <- 'color:#00ff00'
          sign_value <- ''
          ico_value <- '▼'
        }
      }
    }
  } else if (variab %in% c('tamponi', 'casi_testati', 'nuovi_tamponi')) {
    if (prec_value == 0) {
      if (curr_value == 0) {
        incr_value_style <- 'color:#ffa500'
        sign_value <- '+'
        ico_value <- '◉'
      } else {
        incr_value_style <- 'color:#00ff00'
        sign_value <- '+'
        ico_value <- '▲'
      }
    } else {
      if (prec_value == curr_value) {
        incr_value_style <- 'color:#ffa500'
        sign_value <- '+'
        ico_value <- '◉'
      }
      else {
        if (prec_value < curr_value) {
          incr_value_style <- 'color:#00ff00'
          sign_value <- '+'
          ico_value <- '▲'
        } else {
          incr_value_style <- 'color:#ff0000'
          sign_value <- ''
          ico_value <- '▼'
        }
      }
    }
    
  }
  
  f_curr_value <- {
    if (variab %in% c('pos_tamponi', 'tasso_letalita')) {
      paste0(format(
        round(curr_value, 3),
        nsmall = 3,
        big.mark = ".",
        decimal.mark = ","
      ),
      '%')
    }
    else {
      format(curr_value,
             big.mark = ".",
             decimal.mark = ",")
    }
  }
  
  f_diff_value <- paste0(sign_value,
                         format(diff_value,
                                big.mark = ".",
                                decimal.mark = ","))
  
  f_incr_value <- {
    if (is.na(incr_value)) {
      ico_value
    } else {
      paste0(
        sign_value,
        format(
          round(incr_value, 2),
          nsmall = 2,
          big.mark = ".",
          decimal.mark = ","
        ),
        '%',
        ifelse(ico_value == '◉', paste0(' ', ico_value), ico_value)
      )
    }
  }
  
  return(
    list(
      f_curr_value,
      diff_value,
      f_diff_value,
      incr_value,
      f_incr_value,
      sign_value,
      ico_value,
      incr_value_style
    )
  )
  
  
}


f_ret_summary <- function(prec_record, curr_record) {
  summary <-
    list(
      'totale_casi' = f_ret_basic_summary(prec_record, curr_record, 'totale_casi'),
      'dimessi_guariti' = f_ret_basic_summary(prec_record, curr_record, 'dimessi_guariti'),
      'nuovi_dimessi' = f_ret_basic_summary(prec_record, curr_record, 'nuovi_dimessi'),
      'deceduti' = f_ret_basic_summary(prec_record, curr_record, 'deceduti'),
      'nuovi_deceduti' = f_ret_basic_summary(prec_record, curr_record, 'nuovi_deceduti'),
      'nuovi_positivi' = f_ret_basic_summary(prec_record, curr_record, 'nuovi_positivi'),
      'tamponi' = f_ret_basic_summary(prec_record, curr_record, 'tamponi'),
      'nuovi_tamponi' = f_ret_basic_summary(prec_record, curr_record, 'nuovi_tamponi'),
      'pos_tamponi' = f_ret_basic_summary(prec_record, curr_record, 'pos_tamponi'),
      'totale_positivi' = f_ret_basic_summary(prec_record, curr_record, 'totale_positivi'),
      'isolamento_domiciliare' = f_ret_basic_summary(prec_record, curr_record, 'isolamento_domiciliare'),
      'ricoverati_con_sintomi' = f_ret_basic_summary(prec_record, curr_record, 'ricoverati_con_sintomi'),
      'terapia_intensiva' = f_ret_basic_summary(prec_record, curr_record, 'terapia_intensiva'),
      'totale_ospedalizzati' = f_ret_basic_summary(prec_record, curr_record, 'totale_ospedalizzati'),
      'casi_testati' = f_ret_basic_summary(prec_record, curr_record, 'casi_testati'),
      'tasso_letalita' = f_ret_basic_summary(prec_record, curr_record, 'tasso_letalita')
    )
  
}

# Load summary it

load_summary_it <- function (data, selRegions, date) {
  summary <-
    data %>% filter((data %in% c(date, date - 1)) &
                      (denominazione_regione %in% selRegions)) %>%
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
    mutate(
      pos_tamponi = ifelse(
        nuovi_positivi == 0 |
          nuovi_tamponi == 0,
        0,
        round(nuovi_positivi / nuovi_tamponi * 100, 3)
      ),
      tasso_letalita = ifelse(
        deceduti == 0 |
          totale_casi == 0,
        0,
        round(deceduti / totale_casi * 100, 3)
      )
    )
  
  curr_record <- summary[summary$data == date, ]
  prec_record <- summary[summary$data == (date - 1), ]
  
  summary <-
    summary[summary$data == date, ] %>% mutate(adds = list(f_ret_summary(prec_record, curr_record)))
  
  
  return(summary)
}

# Load summary reg

load_summary_per_reg <- function(data, date) {
  curr_summary_per_reg <- data %>% filter(data %in% c(date))
  prec_summary_per_reg <- data %>% filter(data %in% c(date - 1))
  
  
  res <- curr_summary_per_reg %>% mutate(adds = list(NA))
  
  for (reg in regions) {
    prec <-
      prec_summary_per_reg %>% filter(denominazione_regione == reg)
    curr <-
      curr_summary_per_reg %>% filter(denominazione_regione == reg)
    res[res$denominazione_regione == reg, ]$adds <-
      list(f_ret_summary(prec, curr))
  }
  
  return(res)
  
}


load_map_reg <- function(path) {
  data <- st_read(path, stringsAsFactors = FALSE)
  data <- data[order(data$DEN_REG),]
  return(data)
}

draw_distr_chart <-
  function(summary_it,
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
    x_sav <-
      c(var_descrip[var_descrip$field_name == var[2], "description"],
        var_descrip[var_descrip$field_name == var[3], "description"],
        var_descrip[var_descrip$field_name == var[4], "description"])
    
    y_sav <-
      c(summary_it[[var[2]]], summary_it[[var[3]]], summary_it[[var[4]]])
    
    c_color <-
      c(var_descrip[var_descrip$field_name == var[2], "field_color"],
        var_descrip[var_descrip$field_name == var[3], "field_color"],
        var_descrip[var_descrip$field_name == var[4], "field_color"])
    
    dataf <- data.frame(xx = x_sav,
                        yy = y_sav,
                        cc = c_color)
    
    f1 <-
      dataf[dataf$xx == var_descrip[var_descrip$field_name == var[2], "description"],]
    f1 <- f1 %>% mutate(pp =  yy / summary_it[[var[1]]])
    
    f2 <-
      dataf[dataf$xx == var_descrip[var_descrip$field_name == var[3], "description"],]
    f2 <- f2 %>% mutate(pp =  yy / summary_it[[var[1]]])
    
    f3 <-
      dataf[dataf$xx == var_descrip[var_descrip$field_name == var[4], "description"],]
    f3 <- f3 %>% mutate(pp =  yy / summary_it[[var[1]]])
    
    fig <- dataf %>%
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
      font = list(color = f2$cc),
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
      xaxis = list(
        title = '',
        showgrid = FALSE,
        tickangle = 45
      ),
      yaxis = list(
        title = '',
        showgrid = TRUE,
        gridcolor = 'rgb(40,40,40)',
        tickcolor = 'rgb(40,40,40)',
        tickwidth = 0.1
      ),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      font = list(color = 'white'),
      legend = list(x = 0, y = 1)
    )
    
    
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
          xaxis = list(title = '', showgrid = FALSE),
          yaxis = list(
            title = '',
            showgrid = TRUE,
            gridcolor = 'rgb(40,40,40)',
            tickcolor = 'rgb(40,40,40)'
          ),
          barmode = 'stack',
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
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
          xaxis = list(title = '', showgrid = FALSE),
          yaxis = list(
            title = '',
            showgrid = TRUE,
            gridcolor = 'rgb(40,40,40)',
            tickcolor = 'rgb(40,40,40)'
          ),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
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
            xvalue = summary[summary$denominazione_regione == reg,]$data
            
            subSum <-
              summary %>% filter(data == date &
                                   denominazione_regione == reg)
            num_abitanti <- subSum$num_abitanti
            
            yvalue = summary[summary$denominazione_regione == reg,][[var]] /
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
              xaxis = list(title = '', showgrid = FALSE),
              yaxis = list(
                title = '',
                showgrid = TRUE,
                gridcolor = 'rgb(40,40,40)',
                tickcolor = 'rgb(40,40,40)'
              ),
              showlegend = T,
              plot_bgcolor = 'rgba(0,0,0,0)',
              paper_bgcolor = 'rgba(0,0,0,0)',
              font = list(color = 'white')
            )
          
        }
        
      } else {
        p <- plot_ly()
        
        for (reg in selRegions) {
          p <-
            add_trace(
              p,
              x = summary[summary$denominazione_regione == reg,]$data,
              y = summary[summary$denominazione_regione == reg,][[var]],
              type = 'scatter',
              mode = "lines",
              name = reg
            )
        }
        
        p %>%
          layout(
            xaxis = list(title = '', showgrid = FALSE),
            yaxis = list(
              title = '',
              showgrid = TRUE,
              gridcolor = 'rgb(40,40,40)',
              tickcolor = 'rgb(40,40,40)'
            ),
            showlegend = T,
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)',
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
                                                var, "field_color"]),
              marker = list(color = var_descrip[var_descrip$field_name == var, "field_color"]),
              name = var
            )  %>%
            layout(
              xaxis = list(title = '', showgrid = FALSE),
              yaxis = list(
                title = '',
                showgrid = TRUE,
                gridcolor = 'rgb(40,40,40)',
                tickcolor = 'rgb(40,40,40)'
              ),
              showlegend = F,
              plot_bgcolor = 'rgba(0,0,0,0)',
              paper_bgcolor = 'rgba(0,0,0,0)',
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
            line = list(color = var_descrip[var_descrip$field_name == var, "field_color"]),
            marker = list(color = var_descrip[var_descrip$field_name == var, "field_color"]),
            name = var
          )  %>%
          layout(
            xaxis = list(title = '', showgrid = FALSE),
            yaxis = list(
              title = '',
              showgrid = TRUE,
              gridcolor = 'rgb(40,40,40)',
              tickcolor = 'rgb(40,40,40)'
            ),
            showlegend = F,
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)',
            font = list(color = 'white'),
            legend = list(x = 0, y = 1)
          )
      }
    }
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
    summary <- data %>% filter(denominazione_regione %in% selRegions)
    datasf <- datasf %>% filter(DEN_REG %in% selRegions)
    
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
    
    if (ckRisc) {
      summary <-
        summary[, c("denominazione_regione", var, 'num_abitanti')]
      
      summary <-
        summary %>% mutate(
          num_abitanti_n = num_abitanti / numCasi,
          var_rescale = get(var) / num_abitanti_n
        ) %>% select(-num_abitanti_n)
      
      summary <-
        summary[, c("denominazione_regione", 'var_rescale')]
      colnames(summary) <- c("DEN_REG", var)
      
      req <- inner_join(datasf, summary)
      
      p <-
        plot_ly(
          data = req,
          split = ~ DEN_REG,
          color = ~ get(var),
          alpha = 1,
          colors = detPal,
          stroke = I("#222222"),
          text =
            ~ paste0(DEN_REG,
                     "\n",
                     round(get(var), 2)),
          hoveron = "fills",
          hoverinfo = "text",
          showlegend = F
        ) %>%
        colorbar(title = '', tickfont = list(color = '#FFFFFF')) %>%
        layout(
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE),
          margin = list(
            l = 20,
            r = 20,
            b = 10,
            t = 10
          )
        )
      
      
    } else {
      summary <- summary[, c("denominazione_regione", var, 'adds')]
      
      summary <-
        summary %>% mutate(
          f_curr_value = '',
          f_diff_value = '',
          f_incr_value = ''
        )
      
      for (reg in selRegions) {
        summary[summary$denominazione_regione == reg, ]$f_curr_value <-
          summary[summary$denominazione_regione == reg, ]$adds[[1]][[var]][[1]]
        summary[summary$denominazione_regione == reg, ]$f_diff_value <-
          summary[summary$denominazione_regione == reg, ]$adds[[1]][[var]][[3]]
        summary[summary$denominazione_regione == reg, ]$f_incr_value <-
          summary[summary$denominazione_regione == reg, ]$adds[[1]][[var]][[5]]
      }
      
      colnames(summary) <-
        c("DEN_REG",
          var,
          'adds',
          'f_curr_value',
          'f_diff_value',
          'f_incr_value')
      
      req <- inner_join(datasf, summary)
      
      p <-
        plot_ly(
          data = req,
          split = ~ DEN_REG,
          color = ~ get(var),
          alpha = 1,
          colors = detPal,
          stroke = I("#222222"),
          text =
            ~ paste0(
              DEN_REG,
              "\n",
              f_curr_value,
              "\n",
              paste0(f_diff_value, ' ', 'risp. a ieri', '\n'),
              paste0('(', f_incr_value, ')', '\n')
            ),
          hoveron = "fills",
          hoverinfo = "text",
          showlegend = F
        ) %>%
        colorbar(title = '', tickfont = list(color = '#FFFFFF')) %>%
        layout(
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)',
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE),
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
