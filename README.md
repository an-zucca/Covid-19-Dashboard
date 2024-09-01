# Covid-19 Dashboard

---
- Sviluppata da: [Andrea Zucca](https://www.linkedin.com/in/a-zucca)
---

Una [Shiny app](https://shiny.posit.co/) che riporta l'andamento della pandemia di Covid-19 in Italia.

Visita l'app [qui](https://andreazucca.shinyapps.io/Covid19Dashboard/).

https://github.com/user-attachments/assets/e49871c2-1a04-4e48-bf08-944fe124ece8

## Contesto
L’app nasce come progetto del corso “Data visualization, data analytics, data modeling” del master universitario di II livello “Big Data. Metodi statistici per la società della conoscenza”. Il master, organizzato dall’Università degli Studi di Roma “[La Sapienza](https://www.uniroma1.it/it/pagina-strutturale/home)”, è stato frequentato nell’anno accademico 2019/2020.

L'attuale versione della dashboard è il risultato di ottimizzazioni successive.

## Funzionamento
L’app, sviluppata in R, impiega [Shiny](https://shiny.posit.co/) e [Shiny Dashboard](https://rstudio.github.io/shinydashboard/).  
I grafici interativi sono realizzati utilizzando la libreria [Plotly](https://plotly.com/r/).  
La bubble map si avvale della libreria [Leaflet](https://leafletjs.com/).  
A ogni accesso l’app scarica e elabora i dati italiani sull’epidemia da Covid-19.

## Fonti
I dati sulla pandemia, aggiornati a cadenza giornaliera, sono [condivisi](https://github.com/pcm-dpc/COVID-19) dal [Dipartimento della Protezione Civile](https://www.protezionecivile.gov.it/it/).  
La mappa che consente di selezionare le regioni di interesse è realizzata utilizzando gli [shape file](https://www.istat.it/notizia/confini-delle-unita-amministrative-a-fini-statistici-al-1-gennaio-2018-2/) forniti dall’[Istat](https://www.istat.it/).

## Crediti
L’app trae ispirazione da:
* [Covid19App](https://github.com/minmar94/StatGroup19-Covid19App), realizzata dal gruppo di ricerca [StatGroup-19](https://www.uniroma1.it/it/notizia/statgroup-19).  
* [Dashboard ufficiale](https://mappe.protezionecivile.gov.it/it/mappe-e-dashboards-emergenze/dashboards-coronavirus/) per l’Italia, mantenuta dal Dipartimento della Protezione Civile.
