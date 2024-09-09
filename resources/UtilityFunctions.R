var_descrip <- data.frame(
  field_name = c(
    "totale_positivi",
    "totale_casi",
    "nuovi_positivi",
    "dimessi_guariti",
    "nuovi_dimessi_guariti",
    "deceduti",
    "nuovi_deceduti",
    "totale_ospedalizzati",
    "ricoverati_con_sintomi",
    "terapia_intensiva",
    "isolamento_domiciliare",
    "positivi_tamponi",
    "tamponi",
    "nuovi_tamponi"
  ),
  description = c(
    "Totale positivi",
    "Totale casi",
    "Nuovi positivi",
    "Dimessi / Guariti",
    "Nuovi dimessi/guariti",
    "Deceduti",
    "Nuovi deceduti",
    "Totale ospedalizzati",
    "Ricoverati con sintomi",
    "Terapia intensiva",
    "Isolamento domiciliare",
    "Positivi / Tamponi",
    "Tamponi",
    "Nuovi tamponi"
  ),
  field_color = c(
    "#d626ff",
    "#fe00ce",
    "#22ffa7",
    "#86ce00",
    "#86ce00",
    "#9d9da5",
    "#9d9da5",
    "#00b5f7",
    "#ff9616",
    "#fd3216",
    "#FFD500",
    "#0df9ff",
    "#ff3384",
    "#ff3384"
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
    "Friuli-Venezia Giulia",
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


df_variab_incr <- data.frame(
  variab = c(
    "totale_casi",
    "nuovi_positivi",
    "dimessi_guariti",
    "nuovi_dimessi_guariti",
    "deceduti",
    "nuovi_deceduti",
    "totale_positivi",
    "totale_ospedalizzati",
    "isolamento_domiciliare",
    "ricoverati_con_sintomi",
    "terapia_intensiva",
    "positivi_tamponi",
    "tamponi",
    "nuovi_tamponi"
  ),
  incr_variab = c(
    "nuovi_positivi",
    "nuovi_positivi_var",
    "nuovi_dimessi_guariti",
    "nuovi_dimessi_guariti_var",
    "nuovi_deceduti",
    "nuovi_deceduti_var",
    "totale_positivi_var",
    "totale_ospedalizzati_var",
    "isolamento_domiciliare_var",
    "ricoverati_con_sintomi_var",
    "terapia_intensiva_var",
    "positivi_tamponi_var",
    "nuovi_tamponi",
    "nuovi_tamponi_var"
  )
)

plot_bg_color <- 'rgb(252, 252, 255)' # C8C8FF 'rgb(252, 252, 255)' #FCFCFF
paper_bg_color <- 'rgb(252, 252, 255)' #'C8C8FF 'rgb(252, 252, 255)' #FCFCFF

f_ret_format_number <- function(data, mode = 1) {
  if (mode == 1) {
    return(format(data, big.mark = ".", decimal.mark = ","))
  }
  if (mode == 2) {
    if (abs(data) < 10000) {
      return(format(data, big.mark = ".", decimal.mark = ","))
    }
    
    if (abs(data)  >= 10000 & abs(data) <= 999999) {
      return(paste0(round(data / 1000), 'K'))
    }
    
    if (abs(data)  >= 1000000) {
      return(paste0(round(data / 1000000), 'Mln'))
    }
  }
  return(NA)
}

f_ret_format_variab <- function(data, variab, mode = 1) {
  variab_value <- data[[variab]]
  
  if (variab == 'positivi_tamponi') {
    return(paste0(
      format(
        round(variab_value, 2),
        nsmall = 2,
        big.mark = ".",
        decimal.mark = ","
      ),
      '%'
    ))
  }
  
  return(f_ret_format_number(variab_value, mode))
}

f_ret_format_incr_variab <- function(data,
                                     variab,
                                     mode = 1,
                                     perc = 0) {
  variab_incr <- df_variab_incr[df_variab_incr$variab == variab, 'incr_variab']  # recupera il nome della variabile di incremento
  incr_variab_value <- data[[variab_incr]]
  sign_incr_variab <- ifelse(incr_variab_value >= 0, '+', '')
  
  if (variab_incr == 'positivi_tamponi_var') {
    perc <- 1
  }
  
  if (perc == 0) {
    return(paste0(
      sign_incr_variab,
      f_ret_format_number(incr_variab_value, mode)
    ))
  }
  if (perc == 1) {
    variab_value <- data[[variab]]
    incr_perc_value = ((variab_value-(variab_value-incr_variab_value))/(variab_value-incr_variab_value))*100
    
    return(paste0(
      sign_incr_variab,
      format(
        round(incr_perc_value, 3),
        nsmall = 2,
        big.mark = ".",
        decimal.mark = ","
      ),
      '%'
    ))
    
  }
}

f_ret_incr_color <- function(data, variab) {
  variab_incr <- df_variab_incr[df_variab_incr$variab == variab, 'incr_variab']
  incr_value <- data[[variab_incr]]
  
  # basic
  if (variab %in% c('tamponi',
                    'nuovi_tamponi',
                    'dimessi_guariti',
                    'nuovi_dimessi_guariti')) {
    if (incr_value > 0) {
      incr_color <- '#2fb12f'
      incr_bg_color <- '#b7ed84'
    } else {
      incr_color <- '#ff0000'
      incr_bg_color <- '#fbb'
    }
  } else {
    if (incr_value > 0) {
      incr_color <- '#ff0000'
      incr_bg_color <- '#fbb'
    } else {
      incr_color <- '#2fb12f'
      incr_bg_color <- '#b7ed84'
    }
  }
  
  output <- list(incr_color, incr_bg_color)
  
  return(output)
}

f_ret_basic_summary <- function(data, variab) {
  curr_value <- f_ret_format_variab(data, variab)
  abbr_curr_value <- f_ret_format_variab(data, variab, mode = 2)
  incr_value <- f_ret_format_incr_variab(data, variab)
  abbr_incr_value <- f_ret_format_incr_variab(data, variab, mode = 2)
  incr_perc_value <- f_ret_format_incr_variab(data, variab, perc = 1)
  incr_color <- f_ret_incr_color(data, variab)[1]
  incr_bg_color <- f_ret_incr_color(data, variab)[2]
  
  return(
    list(
      curr_value,
      abbr_curr_value,
      incr_value,
      abbr_incr_value,
      incr_perc_value,
      incr_color,
      incr_bg_color
    )
  )
  
}

f_ret_summary <- function(data) {
  summary <-
    list(
      'totale_casi' = f_ret_basic_summary(data, 'totale_casi'),
      'nuovi_positivi' = f_ret_basic_summary(data, 'nuovi_positivi'),
      'dimessi_guariti' = f_ret_basic_summary(data, 'dimessi_guariti'),
      'nuovi_dimessi_guariti' = f_ret_basic_summary(data, 'nuovi_dimessi_guariti'),
      'deceduti' = f_ret_basic_summary(data, 'deceduti'),
      'nuovi_deceduti' = f_ret_basic_summary(data, 'nuovi_deceduti'),
      'totale_positivi' = f_ret_basic_summary(data, 'totale_positivi'),
      'isolamento_domiciliare' = f_ret_basic_summary(data, 'isolamento_domiciliare'),
      'ricoverati_con_sintomi' = f_ret_basic_summary(data, 'ricoverati_con_sintomi'),
      'terapia_intensiva' = f_ret_basic_summary(data, 'terapia_intensiva'),
      'totale_ospedalizzati' = f_ret_basic_summary(data, 'totale_ospedalizzati'),
      'positivi_tamponi' = f_ret_basic_summary(data, 'positivi_tamponi'),
      'tamponi' = f_ret_basic_summary(data, 'tamponi'),
      'nuovi_tamponi' = f_ret_basic_summary(data, 'nuovi_tamponi')
    )
  
  return(summary)
}

load_reg_data_add <- function(path_r, path_p) {
  reg_data <- load_reg_data(path = path_r) %>% mutate(anno = as.numeric(format(data, '%Y')))
  res_data <- load_resident_data(path = path_p)
  
  reg_data_add <- inner_join(
    reg_data,
    res_data,
    c("denominazione_regione" = "denominazione_regione", "anno" = "anno")
  )
  
  return(reg_data_add)
}

# Read regional data from path
load_reg_data <- function(path) {
  dati_reg <- data.table::fread(input = path)
  
  dati_reg <- dati_reg %>%
    mutate(
      data = as.Date(strptime(data, format = "%Y-%m-%d", tz = 'UTC')),
      denominazione_regione = ifelse(
        denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"),
        "Trentino-Alto Adige",
        ifelse(
          denominazione_regione == "Friuli Venezia Giulia",
          "Friuli-Venezia Giulia",
          denominazione_regione
        )
      ),
      codice_regione = ifelse(
        denominazione_regione == "Trentino-Alto Adige",
        4,
        codice_regione
      )
    ) %>%
    group_by(data, codice_regione, denominazione_regione) %>%
    summarise(
      lat = mean(lat),
      long = mean(long),
      totale_casi = sum(totale_casi),
      dimessi_guariti = sum(dimessi_guariti),
      deceduti = sum(deceduti),
      nuovi_positivi = sum(nuovi_positivi),
      totale_positivi = sum(totale_positivi),
      isolamento_domiciliare = sum(isolamento_domiciliare),
      ricoverati_con_sintomi = sum(ricoverati_con_sintomi),
      terapia_intensiva = sum(terapia_intensiva),
      totale_ospedalizzati = sum(totale_ospedalizzati),
      tamponi = sum(tamponi)
    ) %>% ungroup()
  
  min_data <- min(dati_reg$data)
  
  dati_reg <-
    dati_reg %>% group_by(denominazione_regione) %>% mutate(
      nuovi_tamponi = if_else(
        data == min_data,
        tamponi,
        tamponi - lag(tamponi, default = first(tamponi))
      ),
      ricoverati_con_sintomi_var = if_else(
        data == min_data,
        ricoverati_con_sintomi,
        ricoverati_con_sintomi - lag(ricoverati_con_sintomi, default = first(ricoverati_con_sintomi))
      ),
      terapia_intensiva_var = if_else(
        data == min_data,
        terapia_intensiva,
        terapia_intensiva - lag(terapia_intensiva, default = first(terapia_intensiva))
      ),
      totale_ospedalizzati_var = if_else(
        data == min_data,
        totale_ospedalizzati,
        totale_ospedalizzati - lag(totale_ospedalizzati, default = first(totale_ospedalizzati))
      ),
      isolamento_domiciliare_var = if_else(
        data == min_data,
        isolamento_domiciliare,
        isolamento_domiciliare - lag(isolamento_domiciliare, default = first(isolamento_domiciliare))
      ),
      totale_positivi_var = if_else(
        data == min_data,
        totale_positivi,
        totale_positivi - lag(totale_positivi, default = first(totale_positivi))
      ),
      nuovi_dimessi_guariti = if_else(
        data == min_data,
        dimessi_guariti,
        dimessi_guariti - lag(dimessi_guariti, default = first(dimessi_guariti))
      ),
      nuovi_deceduti = if_else(
        data == min_data,
        deceduti,
        deceduti - lag(deceduti, default = first(deceduti))
      ),
      nuovi_positivi_var = if_else(
        data == min_data,
        nuovi_positivi,
        nuovi_positivi - lag(nuovi_positivi, default = first(nuovi_positivi))
      )
    ) %>% ungroup()
  
  dati_reg <- dati_reg %>% mutate(positivi_tamponi = if_else((nuovi_positivi <= 0) |
                                                               (nuovi_tamponi <= 0),
                                                             0,
                                                             nuovi_positivi / nuovi_tamponi * 100
  )) %>% group_by(denominazione_regione) %>% mutate(
    nuovi_tamponi_var = if_else(
      data == min_data,
      nuovi_tamponi,
      nuovi_tamponi - lag(nuovi_tamponi, default = first(nuovi_tamponi))
    ),
    nuovi_dimessi_guariti_var = if_else(
      data == min_data,
      nuovi_dimessi_guariti,
      nuovi_dimessi_guariti - lag(nuovi_dimessi_guariti, default = first(nuovi_dimessi_guariti))
    ),
    nuovi_deceduti_var = if_else(
      data == min_data,
      nuovi_deceduti,
      nuovi_deceduti - lag(nuovi_deceduti, default = first(nuovi_deceduti))
    ),
    positivi_tamponi_var = if_else(
      data == min_data,
      positivi_tamponi,
      positivi_tamponi - lag(positivi_tamponi, default = first(positivi_tamponi))
    )
  ) %>% ungroup()
  
  return(dati_reg)
}

load_summary <- function (regData, selRegs, refDate) {
  summary <-
    regData %>% filter((data %in% c(refDate)) &
                         (denominazione_regione %in% selRegs)) %>%
    group_by(data) %>% summarise(
      totale_casi = sum(totale_casi),
      nuovi_positivi = sum(nuovi_positivi),
      nuovi_positivi_var = sum(nuovi_positivi_var),
      dimessi_guariti = sum(dimessi_guariti),
      nuovi_dimessi_guariti = sum(nuovi_dimessi_guariti),
      nuovi_dimessi_guariti_var = sum(nuovi_dimessi_guariti_var),
      deceduti = sum(deceduti),
      nuovi_deceduti = sum(nuovi_deceduti),
      nuovi_deceduti_var = sum(nuovi_deceduti_var),
      totale_positivi = sum(totale_positivi),
      totale_positivi_var = sum(totale_positivi_var),
      totale_ospedalizzati = sum(totale_ospedalizzati),
      totale_ospedalizzati_var = sum(totale_ospedalizzati_var),
      isolamento_domiciliare = sum(isolamento_domiciliare),
      isolamento_domiciliare_var = sum(isolamento_domiciliare_var),
      ricoverati_con_sintomi = sum(ricoverati_con_sintomi),
      ricoverati_con_sintomi_var = sum(ricoverati_con_sintomi_var),
      terapia_intensiva = sum(terapia_intensiva),
      terapia_intensiva_var = sum(terapia_intensiva_var),
      positivi_tamponi = mean(positivi_tamponi),
      positivi_tamponi_var = mean(positivi_tamponi_var),
      tamponi = sum(tamponi),
      nuovi_tamponi = sum(nuovi_tamponi),
      nuovi_tamponi_var = sum(nuovi_tamponi_var)
    ) %>% ungroup()
  
  summary <- summary %>% mutate(adds = list(f_ret_summary(summary)))
  
  return(summary)
}

load_summary_reg <- function (regData, refDate) {
  summary <-
    regData %>% filter(data == refDate) %>% mutate(adds = list(NA))
  
  for (reg in summary$denominazione_regione) {
    rowReg <- summary %>% filter(denominazione_regione == reg)
    summary[summary$denominazione_regione == reg, ]$adds <- list(f_ret_summary(rowReg))
  }
  
  return(summary)
}

# Load residents data
load_resident_data <- function(path) {
  res_data <-
    read.csv(file = path,
             stringsAsFactors = FALSE,
             sep = ',')
  
  # trascina gli ultimi dati disponibili
  last_year <- max(res_data$anno)
  last_res_data <- res_data %>% filter(anno == last_year)
  
  for (x in (last_year + 1):(last_year + 11)) {
    new_data <- last_res_data %>% mutate(anno = x)
    res_data <- rbind(res_data, new_data)
  }
  
  return(res_data)
}

load_map_reg <- function(path) {
  data <- st_read(path, stringsAsFactors = FALSE)
  data <- data %>% select(DEN_REG, geometry) %>% mutate(DEN_REG = ifelse(
    DEN_REG == 'Friuli Venezia Giulia',
    "Friuli-Venezia Giulia",
    DEN_REG
  ))
  data <- data[order(data$DEN_REG), ]
  return(data)
}

get_map_data <-
  function(regSummary,
           selVar,
           selRegs,
           resResc = F,
           resNumb = 10000) {
    summary <-
      regSummary %>% filter(denominazione_regione %in% selRegs)
    
    summary <-
      summary %>% select(c(
        "denominazione_regione",
        "lat",
        "long",
        "residenti",
        all_of(selVar),
        'adds'
      ))
    
    summary <-
      summary %>% mutate(
        valore_base = map(adds, ~ .[[selVar]][[1]]) %>% unlist(),
        valore_incr = map(adds, ~ .[[selVar]][[3]]) %>% unlist(),
        valore_incr_perc = map(adds, ~ .[[selVar]][[5]]) %>% unlist()
      )
    
    colnames(summary) <-
      c(
        "denominazione_regione",
        "lat",
        "long",
        "residenti",
        "raw_value",
        "adds",
        "valore_base",
        "valore_incr",
        "valore_incr_perc"
      )
    
    if (resResc == T && selVar != 'positivi_tamponi') {
      summary <-
        summary %>% mutate(
          value = raw_value / (residenti / resNumb),
          valore_base = format(
            round(value, 2),
            nsmall = 2,
            big.mark = ".",
            decimal.mark = ","
          )
        )
    } else {
      summary <- summary %>% mutate(value = raw_value)
    }
    
    summary$circle_radius <- rescale(summary$value, to = c(15000, 55000))
    return(summary)
  }

draw_distr_bar_chart <- function(summary_it, distrType, barType) {
  if (distrType == 'T') {
    var <-
      c('totale_casi',
        'dimessi_guariti',
        'deceduti',
        'totale_positivi')
  } else if (distrType == 'P') {
    var <-
      c(
        'totale_positivi',
        'isolamento_domiciliare',
        'ricoverati_con_sintomi',
        'terapia_intensiva'
      )
  }
  
  x_sav <-
    c(var_descrip[var_descrip$field_name == var[2], "description"], var_descrip[var_descrip$field_name == var[3], "description"], var_descrip[var_descrip$field_name == var[4], "description"])
  
  y_sav <-
    c(summary_it[[var[2]]], summary_it[[var[3]]], summary_it[[var[4]]])
  
  c_color <-
    c(var_descrip[var_descrip$field_name == var[2], "field_color"], var_descrip[var_descrip$field_name == var[3], "field_color"], var_descrip[var_descrip$field_name == var[4], "field_color"])
  
  perc <- c(summary_it[[var[2]]] / summary_it[[var[1]]],
            summary_it[[var[3]]] / summary_it[[var[1]]],
            summary_it[[var[4]]] / summary_it[[var[1]]])
  
  dataf <- data.frame(xx = x_sav,
                      yy = y_sav,
                      pp = perc,
                      cc = c_color)
  
  if (barType == 'V') {
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
      x = dataf[1, ]$xx,
      y = dataf[1, ]$yy + max(dataf$yy) / 10,
      text = paste(round(dataf[1, ]$pp * 100, 3), '%'),
      font = list(color = dataf[1, ]$cc),
      showarrow = FALSE
    )
    
    fig <- fig %>% add_annotations(
      xref = 'x',
      yref = 'y',
      x = dataf[2, ]$xx,
      y = dataf[2, ]$yy + max(dataf$yy) / 10,
      text = paste(round(dataf[2, ]$pp * 100, 3), '%'),
      font = list(color = dataf[2, ]$cc),
      showarrow = FALSE
    )
    
    fig <- fig %>% add_annotations(
      xref = 'x',
      yref = 'y',
      x = dataf[3, ]$xx,
      y = dataf[3, ]$yy + max(dataf$yy) / 10,
      text = paste(round(dataf[3, ]$pp * 100, 3), '%'),
      font = list(color = dataf[3, ]$cc),
      showarrow = FALSE
    )
    
    fig <- fig %>% layout(
      xaxis = list(
        title = '',
        showgrid = FALSE,
        tickangle = 45
      ),
      yaxis = list(
        title = '',
        showgrid = TRUE,
        zeroline = FALSE
      ),
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = paper_bg_color
    )
    
    return(fig)
  }
  
  if (barType == 'H') {
    fig <- dataf %>% plot_ly(
      x = ~ yy,
      y = ~ reorder(xx, yy),
      type = 'bar',
      orientation = 'h',
      marker = list(color = ~ cc)
    )
    
    fig <- fig %>% add_annotations(
      xref = 'x',
      yref = 'y',
      x = dataf[1, ]$yy + max(dataf$yy) / 6,
      y = dataf[1, ]$xx,
      text = paste(round(dataf[1, ]$pp * 100, 3), '%'),
      showarrow = FALSE,
      font = list(color = dataf[1, ]$cc)
    )
    
    fig <- fig %>% add_annotations(
      xref = 'x',
      yref = 'y',
      x = dataf[2, ]$yy + max(dataf$yy) / 6,
      y = dataf[2, ]$xx,
      text = paste(round(dataf[2, ]$pp * 100, 3), '%'),
      showarrow = FALSE,
      font = list(color = dataf[2, ]$cc)
    )
    
    fig <- fig %>% add_annotations(
      xref = 'x',
      yref = 'y',
      x = dataf[3, ]$yy + max(dataf$yy) / 6,
      y = dataf[3, ]$xx,
      text = paste(round(dataf[3, ]$pp * 100, 3), '%'),
      showarrow = FALSE,
      font = list(color = dataf[3, ]$cc)
    )
    
    fig <- fig %>% layout(
      xaxis = list(
        title = '',
        showgrid = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(title = '', showgrid = FALSE),
      margin = list(pad = 14),
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = paper_bg_color
    )
    
  }
  
  else {
    return ()
  }
  
}

draw_donut_chart <- function(summary_it, distrType, barType) {
  if (distrType == 'T') {
    var <-
      c('totale_casi',
        'dimessi_guariti',
        'deceduti',
        'totale_positivi')
  } else if (distrType == 'P') {
    var <-
      c(
        'totale_positivi',
        'isolamento_domiciliare',
        'ricoverati_con_sintomi',
        'terapia_intensiva'
      )
  }
  
  x_sav <-
    c(var_descrip[var_descrip$field_name == var[2], "description"], var_descrip[var_descrip$field_name == var[3], "description"], var_descrip[var_descrip$field_name == var[4], "description"])
  
  y_sav <-
    c(summary_it[[var[2]]], summary_it[[var[3]]], summary_it[[var[4]]])
  
  c_color <-
    c(var_descrip[var_descrip$field_name == var[2], "field_color"], var_descrip[var_descrip$field_name == var[3], "field_color"], var_descrip[var_descrip$field_name == var[4], "field_color"])
  
  perc <- c(summary_it[[var[2]]] / summary_it[[var[1]]],
            summary_it[[var[3]]] / summary_it[[var[1]]],
            summary_it[[var[4]]] / summary_it[[var[1]]])
  
  df <- data.frame(xx = x_sav,
                   yy = y_sav,
                   pp = perc,
                   cc = c_color)
  
  fig <- df %>% plot_ly(
    labels = ~ xx,
    values = ~ yy,
    marker = list(colors = ~ cc)
  )
  fig <- fig %>% add_pie(hole = 0.6)
  fig <- fig %>% layout(
    title = "",
    showlegend = T,
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    annotations = list(text = var_descrip[var_descrip$field_name == var[1], "description"], showarrow =
                         FALSE)
    # plot_bgcolor = plot_bg_color,
    # paper_bgcolor = paper_bg_color
  )
  
  fig
}

draw_distr_time_chart <-
  function(regData, refDate, selRegs, distrType) {
    if (distrType == 'T') {
      var <- c('totale_positivi', 'dimessi_guariti', 'deceduti')
    } else if (distrType == 'P') {
      var <-
        c('terapia_intensiva',
          'ricoverati_con_sintomi',
          'isolamento_domiciliare')
    }
    
    summary <-
      regData %>% select(c("data", "denominazione_regione", all_of(var))) %>% filter(denominazione_regione %in% selRegs &
                                                                                       data <= refDate)  %>%
      group_by(data) %>% summarise(var1 = sum(!!(sym(var[1]))),
                                   var2 = sum(!!(sym(var[2]))),
                                   var3 = sum(!!(sym(var[3])))) %>% ungroup()
    
    fig <- summary %>% plot_ly() %>%
      add_trace(
        x = ~ data,
        y = ~ var1,
        type = 'scatter',
        mode = 'lines',
        name = var_descrip[var_descrip$field_name == var[1], "description"],
        line = list(width = 2, color = var_descrip[var_descrip$field_name == var[1], "field_color"]),
        fill = 'tozeroy',
        fillcolor = toRGB(var_descrip[var_descrip$field_name == var[1], "field_color"], alpha = 0.2)
      ) %>%
      add_trace(
        x = ~ data,
        y = ~ var2,
        type = 'scatter',
        mode = 'lines',
        name = var_descrip[var_descrip$field_name == var[2], "description"],
        line = list(width = 2, color = var_descrip[var_descrip$field_name == var[2], "field_color"]),
        fill = 'tozeroy',
        fillcolor = toRGB(var_descrip[var_descrip$field_name == var[2], "field_color"], alpha = 0.2)
      ) %>%
      add_trace(
        x = ~ data,
        y = ~ var3,
        type = 'scatter',
        mode = 'lines',
        name = var_descrip[var_descrip$field_name == var[3], "description"],
        line = list(width = 2, color = var_descrip[var_descrip$field_name == var[3], "field_color"]),
        fill = 'tozeroy',
        fillcolor = toRGB(var_descrip[var_descrip$field_name == var[3], "field_color"], alpha = 0.2)
      ) %>%
      layout(
        xaxis = list(title = '', showgrid = FALSE),
        yaxis = list(title = '', showgrid = TRUE),
        plot_bgcolor = plot_bg_color,
        paper_bgcolor = paper_bg_color,
        legend = list(x = 0, y = 1)
      )
    
    return(fig)
  }

draw_time_series_plot <-
  function(regData,
           refDate,
           selRegs,
           selVar,
           serByReg = F,
           resResc = F,
           numRes = NA) {
    summary <-
      regData %>% select(c(
        "data",
        "denominazione_regione",
        "residenti",
        all_of(selVar)
      )) %>% filter(denominazione_regione %in% selRegs &
                      data <= refDate)
    
    if (serByReg) {
      fig <- plot_ly()
      
      if (resResc) {
        # riscala per numero residenti
        summary <-
          summary %>% mutate(variab = !!(sym(selVar)) / (residenti / numRes))
      } else {
        summary <- summary %>% mutate(variab = !!(sym(selVar)))
      }
      
      for (reg in selRegs) {
        fig <-
          add_trace(
            fig,
            x = summary[summary$denominazione_regione == reg, ]$data,
            y = summary[summary$denominazione_regione == reg, ]$variab,
            type = 'scatter',
            mode = "lines",
            name = reg
          )
      }
      
      fig <- fig %>%
        layout(
          xaxis = list(title = '', showgrid = FALSE),
          yaxis = list(
            title = '',
            showgrid = TRUE,
            #gridcolor = 'rgb(40,40,40)',
            #tickcolor = 'rgb(40,40,40)'),
            showlegend = T,
            plot_bgcolor = plot_bg_color,
            paper_bgcolor = paper_bg_color
            # font = list(color = 'white')
          )
        )
    }
    else {
      if (selVar == 'positivi_tamponi') {
        summary <-
          summary %>% group_by(data) %>% summarise(variab = mean(!!(sym(selVar))))
      } else {
        if (resResc) {
          # riscala per numero residenti
          summary <-
            summary %>% group_by(data) %>% summarise(variab = sum(!!(sym(selVar))) / (sum(residenti) / numRes))
        } else {
          summary <-
            summary %>% group_by(data) %>% summarise(variab = sum(!!(sym(selVar))))
        }
      }
      
      fig <- plot_ly(
        x = summary$data,
        y = summary$variab,
        name = var_descrip[var_descrip$field_name == selVar, "description"],
        type = "bar",
        marker = list(color = var_descrip[var_descrip$field_name == selVar, "field_color"])
      ) %>%
        layout(
          xaxis = list(title = '', showgrid = FALSE),
          yaxis = list(
            title = '',
            showgrid = TRUE,
            #gridcolor = 'rgb(40,40,40)',
            #tickcolor = 'rgb(40,40,40)'),
            showlegend = T,
            plot_bgcolor = plot_bg_color,
            paper_bgcolor = paper_bg_color,
            #font = list(color = 'white'),
            legend = list(x = 0, y = 1)
          )
        )
    }
    
    return(fig)
    
  }
