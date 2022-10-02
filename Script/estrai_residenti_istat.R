estrai_residenti_istat <-
  function(csvFilename) {
    regions_istat <-
      list(
        "Piemonte",
        "Valle d'Aosta / Vallée d'Aoste",
        "Liguria",
        "Lombardia",
        "Trentino Alto Adige / Südtirol",
        "Veneto",
        "Friuli-Venezia Giulia",
        "Emilia-Romagna",
        "Toscana",
        "Umbria",
        "Marche",
        "Lazio",
        "Abruzzo",
        "Molise",
        "Campania",
        "Puglia",
        "Basilicata",
        "Calabria",
        "Sicilia",
        "Sardegna"
      )
    
    dati_res <-
      read.csv(file = csvFilename,
               stringsAsFactors = FALSE,
               sep = ',')
    
    dati_res <-
      dati_res %>% filter(Territorio %in% regions_istat &
                            Sesso == "totale" &
                            Stato.civile == "totale") %>%
      mutate(
        Territorio = ifelse(
          Territorio == "Trentino Alto Adige / Südtirol",
          "Trentino-Alto Adige",
          Territorio
        )
      ) %>%
      mutate(
        Territorio = ifelse(
          Territorio == "Valle d'Aosta / Vallée d'Aoste",
          "Valle d'Aosta",
          Territorio
        )
      ) %>%
      group_by(Territorio, Seleziona.periodo) %>%
      summarise(Value = sum(Value)) %>% ungroup() %>%
      rename(
        denominazione_regione = Territorio,
        anno = Seleziona.periodo,
        residenti = Value
      )
    
    write.csv(dati_res,
              "popolazione_residente_2020-2022.csv",
              row.names = FALSE)
    
  }
