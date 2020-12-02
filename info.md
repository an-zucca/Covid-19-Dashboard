---
title: "Untitled"
author: "Zucca"
date: "30/8/2020"
output: html_document
---

# Autore

![](img/user_andrea.jpg)

Andrea Zucca
_____________________________________________________________________________________________________________________________________________________
# L'App
L'app, qui alla sua prima versione, rappresenta il progetto di fine corso in "Data visualization, data analytics, data modeling".<br/>
Il corso è parte del Master universitario di II livello in "Big Data. Metodi statistici per la società della conoscenza" (A.A. 2019-2020). Dipartimento Scienze Statistiche. Università degli Studi di Roma "La Sapienza".
_____________________________________________________________________________________________________________________________________________________

# Funzionamento
L'app sviluppata in R utilizza le librerie Shiny e ShinyDashboard.<br/>
I grafici sono realizzati utilizzando la libreria [Plotly](https://plotly.com/r/).<br/>
A ogni accesso l'app scarica e elabora gli ultimi dati disponibili sull'epidemia da Covid-19.
_____________________________________________________________________________________________________________________________________________________

# Fonti
I dati sulla pandemia, aggiornati a cadenza giornaliera, sono condivisi dal Dipartimento della Protezione civile al seguente [link](https://github.com/pcm-dpc/COVID-19).<br/>
La mappa delle regioni è realizzata utilizzando gli shape file resi [qui](https://www.istat.it/it/archivio/222527) disponibili dall'Istat.
_____________________________________________________________________________________________________________________________________________________

# Info e crediti
L'app trae spunto da:

* [Covid19App](https://statgroup19.shinyapps.io/Covid19App/), realizzata dal gruppo di ricerca [StatGroup-19](https://www.uniroma1.it/it/notizia/statgroup-19). 
* [Dashboard ufficiale](http://opendatadpc.maps.arcgis.com/apps/opsdashboard/index.html#/b0c68bce2cce478eaac82fe38d4138b1) per l'Italia, mantenuta dal Dipartimento della Protezione civile. <br/> <br/>

Il codice per generare l'app è reso [qui](https://github.com/zuccaandrea/Covid19App) disponibile.
_____________________________________________________________________________________________________________________________________________________

# Criticità e sviluppi futuri
* La mappa per selezionare le regioni viene ricaricata ogni volta che l'utente seleziona o deseleziona una regione <br/> 
*Problema risolvibile via JavaScript*
* Con il passare dei mesi, all'aumentare dei dati, lo spazio disponibile per la visualizzazione delle serie storiche potrebbe non essere sufficiente a una analisi visiva agile delle curve <br/>
*Nonostante Plotly consenta di evidenziare porzioni di un grafico, può essere utile prevedere una visualizzazione a schermo intero degli stessi*
* L'uso dei colori verde e rosso può aiutare a interpretare se un incremento o decremento, rispetto al giorno precedente, di una variabile del sommario, è un dato positivo o negativo nella lotta alla pandemia.
* Implementare supporto multilingua.