server <- function(input, output, session) {

library(tidyverse)
library(lubridate)
library(shiny)
library(highcharter)
library(reticulate) # permet d'utiliser python
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(fontawesome)
library(xts)
	
use_virtualenv("~/climate/venv/", required = TRUE)

source_python("~/climate/getdata.py")

# ecriture en csv du fichier avec les stations

# peut mettre Hourly, Daily, Monthly
# les donnees daily sont mise à jour jusqu'à j - 15
# les donnees hourly sont mise à jour jusqu'à j -1

  bbmin <- reactiveVal(NULL)
  bbmax <- reactiveVal(NULL)
  trig <- reactiveVal(NULL)
  width <- reactiveVal(NULL)
  ## implementation de la liste des stations plus rapide dans la partie serveur
  stations()
  
  st <- read.csv("stations.csv", sep = ";")
  st$display_name <- paste(st$name, st$country)
  st <- st[st$hourly_start != "" | st$daily_start != "",]
  st <- st[st$hourly_end != "" | st$daily_end != "",]
  name_stations <- st$name
  names(name_stations) <- st$display_name
  
  updateSelectizeInput(session,"recherche", choices = name_stations, selected = "", server = T)
  
  # ne mettre à jour les données que lorsque le bouton de validation est enclenché
  observeEvent(input$valid,{
    
    bmax <- max(as.Date(st[st$name == input$recherche,]$daily_end), as.Date(st[st$name == input$recherche,]$hourly_end), na.rm = T)
    bmin <- min(as.Date(st[st$name == input$recherche,]$daily_start), as.Date(st[st$name == input$recherche,]$hourly_start), na.rm = T)
    deb <- bmax - 5000
    if((as.character(bmin) == "Inf") | (as.character(bmax) == "-Inf")){
      bmax <- as.Date(Sys.Date())
      bmin <- (bmax - 10000)
      deb <- bmax - 2000
    }
    updateSliderInput(session, "range", min = as.Date(bmin)+1, max = as.Date(bmax),
                      value = c(deb, as.Date(bmax)))
    val = input$trigger+1
    updateNumericInput(session, "trigger", value = val)
    
  })
  
  observeEvent(input$trigger,{
    observe({shinyjs::click("go")})
  })
  
  observeEvent(input$range,{
    bbmin(input$range[1])
    bbmax(input$range[2])
  })
  
  site <- eventReactive(input$go, {
    if(input$recherche %in% st$name){
      input$recherche
    }else{
      "Paris-Orly"
    }
  })
  
  datas <- eventReactive(input$go, {
    try(getdata(site(), bbmin(), bbmax(), "temp"))
  })
  dataspluv <- eventReactive(input$go, {
    try(getdata(site(), bbmin(), bbmax(), "pluie"))
  })
  dataswind <- eventReactive(input$go, {
    try(getdata(site(), bbmin(), bbmax(), "vent"))
  })
  
  pst <- eventReactive(input$go, {
    graphspiral(site(), datas())
  })
  
  output$graphtemp <- renderHighchart({
    validate(need(datas(),""
    ))
    graphtemp(site(),datas())
  })
  
  output$graphpluv <- renderHighchart({
    validate(need(dataspluv(),""
    ))
    graphpluv(site(),dataspluv())})
  
  output$graphvent <- renderHighchart({
    validate(need(dataswind(),""
    ))
    graphvent(site(),dataswind())})
  
  output$graphspiral <- renderImage({
    validate(need(datas(),""
    ))
    filename <- paste(sample(100, 1),"spiral.png",sep = "")
    ggsave(filename, pst()[[1]], width = 9, height = 8, dpi = 200, units = "in")
    list(src = filename, width = "100%")
  },deleteFile = TRUE)
  
  output$graphtrend <- renderImage({
    validate(need(datas(),""
    ))
    filename <- paste(sample(100, 1),"trend.png",sep = "")
    ggsave(filename, pst()[[2]], width = 8.5, height = 4, dpi = 200, units = "in")
    list(src = filename, width = "100%")
  },deleteFile = TRUE)
  
  output$pbp <- renderHighchart({
    validate(need(datas(),""
    ))
    bplot(site(), datas())})
  
  output$downloadtrend <- downloadHandler(
    filename = function(){paste("monthlyanomalytrend",'.png',sep='')},
    content = function(file){
      ggsave(file, pst()[[2]], width = 8.5, height = 4, dpi = 500, units = "in")
    }
  )
  
  output$downloadspiral<- downloadHandler(
    filename = function(){paste("monthlyanomalyspiral",'.png',sep='')},
    content = function(file2){
      ggsave(file2, pst()[[1]], width = 9, height = 7, dpi = 500, units = "in")
    })
  
  
  latr <- eventReactive(site(),{
    st[st$name == site(),]$latitude
  })
  
  longr <- eventReactive(site(),{
    st[st$name == site(),]$longitude
  })
  output$map <- renderLeaflet({
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = "red"
    )
    
    leaflet(st) %>% addTiles() %>%
      fitBounds(~min(longr()-1.95), ~min(latr()-1.95), ~max(longr()+1.95), ~max(latr()+1.95))%>%
      addAwesomeMarkers(~longr(), ~latr(), icon=icons, label=~as.character(site()))
  })
  
  
  output$globalmap <- renderLeaflet({
    leaflet(st,options = leafletOptions(preferCanvas = TRUE, minZoom = 2)) %>% addTiles() %>%
      fitBounds(~min(-90), ~min(-10), ~max(100), ~max(75.74))%>%
      setMaxBounds(~min(-95), ~min(-100), ~max(95), ~max(100))%>%
      addCircleMarkers(~longitude, ~latitude, label=~as.character(name),
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom =  7,
                                                             iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-';  
    if (childCount < 100) {  
      c += 'large';  
    } else if (childCount < 1000) {  
      c += 'medium';  
    } else { 
      c += 'small';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }")))
  })
  
  
  output$trendtext <- renderText({
    validate(need(datas(),"Sorry, there is no data for this station. You may select another one near by"
    ))
    trend(site(),datas())
  })
  
  observeEvent(input$map,{
    shinyjs::toggle("toggle-map")
  })
  observeEvent(input$exmap,{
    shinyjs::toggle("toggle-map")
  })
  
  
  observeEvent(input$plu,{
    shinyjs::toggle("toggle-pluv")
  })
  observeEvent(input$explu,{
    shinyjs::toggle("toggle-pluv")
  })
  
  observeEvent(input$win,{
    shinyjs::toggle("toggle-wind")
  })
  observeEvent(input$exwin,{
    shinyjs::toggle("toggle-wind")
  })
  
  observeEvent(input$inf,{
    shinyjs::toggle("toggle-info")
  })
  observeEvent(input$exinf,{
    shinyjs::toggle("toggle-info")
  })
  
  observeEvent(input$contact,{
    shinyjs::toggle("toggle-contact")
    shinyjs::hide("toggle-source")
    shinyjs::hide("toggle-disclaimer")
  })
  
  observeEvent(input$excontact,{
    shinyjs::toggle("toggle-contact")
  })
  
  observeEvent(input$source,{
    shinyjs::toggle("toggle-source")
    shinyjs::hide("toggle-contact")
    shinyjs::hide("toggle-disclaimer")
  })
  
  observeEvent(input$exsource,{
    shinyjs::toggle("toggle-source")
  })
  
  observeEvent(input$disclaimer,{
    shinyjs::toggle("toggle-disclaimer")
    shinyjs::hide("toggle-contact")
    shinyjs::hide("toggle-source")
  })
  
  observeEvent(input$exdisclaimer,{
    shinyjs::toggle("toggle-disclaimer")
  })
  
  
  #####################################################################################################################    
  #####################################################################################################################
  #####################################################################################################################
  date_range <- function (lieu){
    date_max <- max(c(unique(st[st$name == lieu,]$daily_end),
                      unique(st[st$name == lieu,]$hourly_end)))
    date_min <- min(c(unique(st[st$name == lieu,]$daily_start),
                      unique(st[st$name == lieu,]$hourly_start)))
    date_limit <- c("daily_start" = st[st$name == lieu,]$daily_start,
                    "hourly_start" = st[st$name == lieu,]$hourly_start,
                    "daily_end" = st[st$name == lieu,]$daily_end,
                    "hourly_end" = st[st$name == lieu,]$hourly_end)
    return(c(date_max, date_min, date_limit))
  }
  
  getdata <- function (lieu,dmin,dmax, parametre){
    
    
    if(parametre == "temp"){
      index_d <- c(1,2,3,11)
      index_r <- c(1,12)
    }
    if(parametre == "pluie"){
      index_d <- c(4,11)
      index_r <- c(4,12)
    }
    if(parametre == "press"){
      index_d <- c(9,11)
      index_r <- c(9,12)
    }
    if(parametre == "vent"){
      index_d <- c(7,11)
      index_r <- c(7,12)
    }
    req <- date_range(lieu)
    
    if(req[['hourly_start']] != ""){
      if((dmin < req[['daily_start']]) & (dmin >= req[['hourly_start']])){ # necessite complement avec la methode hourly
        
        if(dmax <= req[['daily_start']]){
          borne_max <- dmax 
        }else{
          borne_max <-  req[['daily_start']]
        }
        
        dtcomp <- getdt(lieu,Hourly, paste(dmin,"00:00:00"), paste(borne_max, "00:00:00"))
        dtcomp$dateheure <- as_datetime(rownames(dtcomp))
        dtcomp <- dtcomp[,index_r]
        
        if(parametre == "pluie"){
          ajout_inf <- setNames(aggregate(dtcomp[,1], by = list(substr(dtcomp$dateheure,1,10)), 'sum', na.rm = T), c("date", "val"))
        }else{
          ajout_inf <- setNames(aggregate(dtcomp[,1], by = list(substr(dtcomp$dateheure,1,10)), 'mean', na.rm = T), c("date", "val"))
        }
        
        
        if(parametre == "temp"){
          ajout_inf_min <- setNames(aggregate(dtcomp[,1], by = list(substr(dtcomp$dateheure,1,10)), 'min', na.rm = T), c("date", "val"))
          #ajout_inf_min[ajout_inf_min$val == "Inf",]$val <- NA
          ajout_inf_max <- setNames(aggregate(dtcomp[,1], by = list(substr(dtcomp$dateheure,1,10)), 'max', na.rm = T), c("date", "val"))
          #ajout_inf_max[ajout_inf_max$val == "-Inf",]$val <- NA
          
          ajout_inf <- setNames(inner_join(ajout_inf, ajout_inf_min, "date"), c("date", "val", "val1"))
          ajout_inf <- setNames(inner_join(ajout_inf, ajout_inf_max, "date"), c("date", "val", "val1", "val2"))
        }
      }
    }
    
    if(req[['hourly_end']] != ""){
      if((dmax > req[['daily_end']]) & (dmax <= req[['hourly_end']])){ # necessite complement avec la methode hourly
        
        if(dmin >= req[['daily_end']]){
          borne_min <- dmin
        }else{
          borne_min <- req[['daily_end']]
        }
        dtcomp2 <- getdt(lieu,Hourly, paste(borne_min,"00:00:00"), paste(dmax, "00:00:00"))
        dtcomp2$dateheure <- as_datetime(rownames(dtcomp2))
        dtcomp2 <- dtcomp2[,index_r]
        
        dtcomp2$dateheure <- as_datetime(rownames(dtcomp2))
        if(parametre == "pluie"){
          ajout_sup <- setNames(aggregate(dtcomp2[,1], by = list(substr(dtcomp2$dateheure,1,10)), 'sum', na.rm = T), c("date", "val"))
        }else{
          ajout_sup <- setNames(aggregate(dtcomp2[,1], by = list(substr(dtcomp2$dateheure,1,10)), 'mean', na.rm = T), c("date", "val"))
        }
        if(parametre == "temp"){
          ajout_sup_min <- setNames(aggregate(dtcomp2[,1], by = list(substr(dtcomp2$dateheure,1,10)), 'min', na.rm = T), c("date", "val"))
          #ajout_sup_min[ajout_sup_min$val == "Inf",]$val <- NA
          ajout_sup_max <- setNames(aggregate(dtcomp2[,1], by = list(substr(dtcomp2$dateheure,1,10)), 'max', na.rm = T), c("date", "val"))
          #ajout_sup_max[ajout_sup_max$val == "-Inf",]$val <- NA
          
          ajout_sup <- setNames(inner_join(ajout_sup, ajout_sup_min, "date"), c("date", "val", "val1"))
          ajout_sup <- setNames(inner_join(ajout_sup, ajout_sup_max, "date"), c("date", "val", "val1", "val2"))
        }
      }
    }
    if(parametre == "temp"){
      data <- getdt(lieu, Daily, paste(dmin,"00:00:00"), paste(dmax, "00:00:00"))
      data$date <- as_datetime(rownames(data))
      data <- data[,index_d]
      colnames(data) <- c("val","val1", "val2", "date")
      data[is.na(data$val),]$val <- (data[is.na(data$val),]$val1+data[is.na(data$val),]$val2)/2
      #colnames(data)[4] <- "dateheure"
    }else{
      data <- getdt(lieu, Daily, paste(dmin,"00:00:00"), paste(dmax, "00:00:00"))
      data$date <- as_datetime(rownames(data))
      data <- data[,index_d]
      colnames(data) <- c("val","date")
      #colnames(data)[2] <- "dateheure"
    }
    
    if(exists('ajout_inf')){
      data <- rbind(data, ajout_inf)
    }
    if(exists('ajout_sup')){
      data <- rbind(data, ajout_sup)
    }
    data$date<- as_datetime(substr(data$date,1,10))
    
    if(parametre == "temp"){
      colnames(data)[4] <- "dateheure"
    }else{
      colnames(data)[2] <- "dateheure"
    }
    return(data)
  }
  
  trend <- function(lieu, tt){
    datatrend <- tt
    miny <- as.numeric(min(substr(datatrend$dateheure,1,4), na.rm = T))
    mind <- min(datatrend$dateheure, na.rm = T)
    
    if(mind > as_datetime(paste(miny,"03-01",sep = ""))){
      mindate <- as_datetime(paste((miny+1),"01-01",sep = ""))
      datatrend <- datatrend[datatrend$dateheure >= mindate,]
    }
    
    if(as.numeric(substr(max(datatrend$dateheure,na.rm = T),6,7)) < 10){
      yearlim <- as.numeric(substr(max(datatrend$dateheure,na.rm = T),1,4))
      datatrend <- datatrend[datatrend$dateheure < as_datetime(paste(yearlim,"-01-01",sep = "")),]
      datatrend <<- datatrend
    }
    try(tr <- setNames(aggregate(datatrend$val, by = list(substr(datatrend$dateheure,1,4)), 'mean'),c("annee","valeur")))
    try(mod <- lm(data = tr, valeur~as.numeric(annee)))
    
    try(val <- mod$coefficients[[2]])
    try(if(val < 0){
      sign <- "-"
      output$trendicon <- renderUI(
        tags$i(class = "fa-solid fa-arrow-trend-down fa-beat", style = "font-size:10vw;
           background:rgba(36, 47, 57,0);color:yellow;position:absolute;bottom:8%;
           left:25%;opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;")
      )
    }else{
      if(val > 0){
        sign <- "+"
        output$trendicon <- renderUI(
          tags$i(class = "fa-solid fa-arrow-trend-up fa-beat", style = "font-size:10vw;
           background:rgba(36, 47, 57,0);color:yellow;position:absolute;bottom:8%;
           left:25%;opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;")
        )
      }else{
        sign <- ""
        output$trendicon <- renderUI(
          tags$i(class = "fa-solid fa-equals fa-beat", style = "font-size:10vw;
           background:rgba(36, 47, 57,0);color:yellow;position:absolute;bottom:8%;
           left:25%;opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;")
        )
      }
    })
    
    output$trendtitle <- renderUI(
      tags$p(class = "trendtitle", paste("Trend at",lieu, min(substr(datatrend$dateheure,1,4), na.rm = T)," - ",max(substr(datatrend$dateheure,1,4), na.rm = T)))
    )
    print(paste(sign,round(abs(val),3), "°C per year",sep = ""))
  }
  graphtemp <- function(lieu, tt){
    datatemp <- tt
    
    try(dtmean <- xts(datatemp$val, order.by = datatemp$dateheure))
    try(dtmin <- xts(datatemp$val1, order.by = datatemp$dateheure))
    try(dtmax <- xts(datatemp$val2, order.by = datatemp$dateheure))
    
    try(weekly <- round(apply.weekly(dtmean, 'mean'),1))
    try(dtmin <- na.omit(dtmin))
    try(weeklymin <- apply.weekly(dtmin, 'min'))
    try(dtmax <- na.omit(dtmax))
    try(weeklymax <- apply.weekly(dtmax, 'max'))
    
    try(aggdata <- fortify(weekly))
    try(aggdatamin <- fortify(weeklymin))
    try(aggdatamax <- fortify(weeklymax))
    
    try(colnames(aggdata) <- c("dateheure","val"))
    try(colnames(aggdatamin) <- c("dateheure","val1"))
    try(colnames(aggdatamax) <- c("dateheure","val2"))
    
    try(datatemp <- inner_join(aggdata,aggdatamin, 'dateheure'))
    try(datatemp <- inner_join(datatemp, aggdatamax, 'dateheure'))
    try(datatemp <- datatemp[!is.na(datatemp$val) | !is.na(datatemp$val1) | !is.na(datatemp$val2),])
    try(datatemp <<- datatemp)
    
    try(highchart(type = "stock")%>%
          hc_legend(enabled = TRUE, 
                    layout = "horizontal", 
                    align = "left", 
                    verticalAlign = "top",
                    itemStyle = list(fontSize = "0.8vw"))%>%
          hc_add_series(name = paste("mean"),datatemp,"spline", 
                        hcaes(as.Date(datatemp$dateheure, 'utc'),
                              round(datatemp$val,1)),color = "orange",lineWidth = 1.4)%>%
          hc_add_series(name = paste("min-max"),datatemp,"areasplinerange", 
                        hcaes(as.Date(datatemp$dateheure, 'utc'), 
                              low = datatemp$val1, 
                              high = datatemp$val2),color = "white", lineWidth = 1)%>%
          hc_title(
            text = paste("Temperature (°C) at",lieu),
            margin = -25,
            align = "center",
            style = list(color = "white",fontWeight = 'bold',fontSize = "1vw", useHTML = TRUE)
          )%>%
          hc_xAxis(type='datetime', labels = list(style = list(fontSize = "0.8vw", color = "white")), maxZoom = 3000*3600*1000)%>%
          hc_yAxis(title = list(text = "Temperature °C", style = list(color = "white", fontSize = "1vw")),
                   opposite = F,labels = list(style = list(fontSize = "0.8vw", color = "white")))%>%
          hc_rangeSelector(enabled = FALSE)%>%
          hc_plotOptions(
            splint = list(color = "#3677d1"),
            areasplinerange = list(opacity = 0.2,fillColor = "rgba(255, 255, 255, 1)"),
            series = list(dataGrouping = list(enabled = TRUE,
                                              forced = TRUE,
                                              units = list(list('day',c(7,15,30,60,120))),
                                              groupPixelWidth = 10))
          )%>%
          hc_navigator(
            maskFill = "rgba(255, 255, 255, 0.1)",
            outlineColor = "none",
            outlineWidth = 1,
            height = 10,
            series = list(
              color = "red",
              lineWidth = 1,
              type = "areaspline", # you can change the type
              fillColor = "rgba(255, 0, 0, 0.2)"
            ),
            handles = list(
              backgroundColor = "rgba(255, 255, 255, 0.2)",
              borderColor = "white"
            )
          )%>%
          hc_credits(
            enabled = TRUE,
            text = "Highcharts.com, Data sources : Meteostat",
            href = "https://www.highcharts.com/"
          )%>%
          highcharter::hc_exporting(
            allowHTML = TRUE,
            sourceWidth = 1000,
            scale = 1,
            enabled = TRUE,
            chartOptions = list(
              navigator = list(enabled = FALSE),
              scrollbar = list(enabled = FALSE),
              xAxis = list(
                labels = list(
                  style = list(
                    fontSize = "15px"
                  )
                )
              ),
              legend = list(
                itemStyle = list(
                  fontSize = "12px"
                )
              ),
              yAxis = list(
                labels = list(
                  style = list(
                    fontSize = "15px"
                  )
                ),
                title = list(
                  style = list(
                    fontSize = "15px"
                  )
                )
              ),
              title = list(
                style = list(
                  fontSize = "15px"
                )
              )
            ),
            buttons = list(
              contextButton = list(
                align = 'right',
                y = 0,
                symbol = 'menu',
                symbolStroke = "yellow",
                theme = list(fill = 'rgba(255, 255, 255,0.2)',
                             states = list(
                               hover = list(fill = 'rgba(255, 255, 255,0.2)'),
                               select = list(fill = 'rgba(255, 255, 255,0.2)')
                             )),
                menuItems = c('downloadPNG', 'downloadJPEG')
              )
            )
          )%>%
          hc_boost(
            enabled = T
          )%>%
          hc_chart(
            zoomType = "x",
            borderColor = "gold",
            borderRadius = 5,
            borderWidth = 0
          )%>%
          hc_add_theme(hc_theme_db())%>%
          hc_scrollbar(
            barBackgroundColor = "gray",
            barBorderRadius = 7,
            barBorderWidth = 0,
            buttonBackgroundColor = "gray",
            buttonBorderWidth = 0,
            buttonArrowColor = "white",
            buttonBorderRadius = 7,
            rifleColor = "white",
            trackBackgroundColor = "rgb(36, 47, 57)",
            trackBorderWidth = 1,
            trackBorderColor = "silver",
            trackBorderRadius = 7
          ))
  }
  
  graphspiral <- function(lieu, dataset){
    
    dt <- dataset
    try(dt <- dt[!is.na(dt$val) | !is.na(dt$val1) | !is.na(dt$val2),])
    
    try(liste_annee <- unique(substr(dt$dateheure,1,4)))
    liste_mois <- c("01","02","03","04","05","06","07","08","09","10","11","12")
    nom_mois <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
    
    try(bmin <- min(as.Date(st[st$name == lieu,]$daily_start), as.Date(st[st$name == lieu,]$hourly_start), na.rm = T))
    try(if(as_datetime(bmin) <= as_datetime("1990-01-01")){
      bmin <- "1990-01-01"
    }else{
      bmin <- bmin
    })
    
    try(dthist <- getdata(lieu, bmin, "2020-01-01", "temp"))
    try(refs <- setNames(aggregate(dthist$val, by = list(substr(dthist$dateheure, 6,7)), 'mean'), c("mois","moyenne")))
    
    try(lan <- rep(liste_annee, each = 12))
    try(lmois <- rep(liste_mois, length(liste_annee)))
    try(nmois <- rep(nom_mois, length(liste_annee)))
    try(lref <- round(rep(refs$moyenne, length(liste_annee)),2))
    try(tot <- setNames(as.data.frame(cbind(lan,lmois,nmois,lref)),c("annee","num_mois","nom_mois","ref_hist")))
    try(tot$date <- paste(tot$annee,"-",tot$num_mois, sep = ""))
    
    try(vals <- setNames(aggregate(dt$val, by = list(substr(dt$dateheure, 1,7)), 'mean', na.rm = T),c("date","moy_mois")))
    try(tot <- inner_join(tot, vals, "date"))
    try(tot$calc <- as.numeric(tot$moy_mois) - as.numeric(tot$ref_hist))
    
    try(t_diff <- setNames(tot[,c(1,3,7)], c("year","month", "t_diff")))
    try(t_diff$year <- as.numeric(t_diff$year))
    
    
    try(next_jan <- t_diff %>%
          filter(month == "Jan") %>%
          mutate(year = year - 1,
                 month = "next_Jan"))
    
    try(t_data <- bind_rows(t_diff, next_jan) %>%
          mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
                 month_number = as.numeric(month)))
    
    try(annotation <- t_data %>%
          slice_max(year) %>%
          slice_max(month_number))
    
    try(temp_lines <- tibble(
      x = 12,
      y = c(2.0, 4.0),
      labels = c("+2.0\u00B0C", "+4.0\u00B0C")
    ))
    
    try(month_labels <- tibble(
      x = 1:12,
      labels = month.abb,
      y = 5.6
    ))
    
    output$spiraltitle <- renderUI(
      tags$p(class = "spiraltitle","Temperature changes at Paris-Orly (monthly anomaly) (monthly average 1990 - 2020")
    )
    
    try(p1 <- t_data %>%
          filter(year != max(year) & month != "Next_Jan")%>%
          ggplot(aes(x=month_number, y=t_diff, group=year, color=year)) +
          geom_col(data = month_labels, aes(x = x+0.5, y = 4.4), fill = "#21252b", inherit.aes = F,
                   width = 1)+
          geom_col(data = month_labels, aes(x = x+0.5, y = min(t_data$t_diff)), fill = "#21252b", width = 1,
                   inherit.aes = F)+
          geom_line(linewidth = 1.1, alpha = 0.5) +
          geom_point(data =annotation, aes(x = month_number, y = t_diff, color = year),
                     inherit.aes = F, size = 2)+
          geom_hline(yintercept = c(2.0,4.0), color="white", linewidth = 1.2, linetype = 2) +
          geom_label(data = temp_lines, aes(x = x, y = y, label = labels),
                     color = "white", fill = "#21252b", label.size = 0,fontface = "bold",
                     inherit.aes = F, size = 7.5)+
          geom_text(data = month_labels, aes(x = x, y = y, label = labels),
                    inherit.aes = F, color = "white", size = 8,
                    angle = seq(360-360/12,0,length.out = 12))+ 
          #geom_text(aes(x = 1, y = min(t_data$t_diff), label = max(t_data$year)), size = 7, color = "white",
          #         face = "bold")+
          scale_x_continuous(breaks=1:12,
                             labels=month.abb, expand = c(0,0),
                             sec.axis = dup_axis(name = NULL, labels=NULL)) +
          scale_y_continuous(breaks = seq(-2, 2, 0.2),
                             sec.axis = dup_axis(name = NULL, labels=NULL)) +
          scale_color_gradient(low = "yellow", high = "red",breaks = seq(min(t_data$year), max(t_data$year), round(length(unique(t_data$year))/5,0)), 
                               guide = guide_colorbar(frame.colour = "#242f39",
                                                      frame.linewidth = 1))+
          #scale_color_viridis_c(breaks = seq(min(t_data$year), max(t_data$year), round(length(unique(t_data$year))/5,0)), 
          #                     guide = guide_colorbar(frame.colour = "black",
          #                                           frame.linewidth = 1))+
          coord_polar(start = 2*pi/12)+
          labs(x = NULL,
               y = NULL,
               title = paste("Temperature changes at",lieu,"\n(Monthly anomaly)\n(Monthly average",substr(bmin,1,4),"- 2020)"),
               group = "year") +
          theme(
            panel.background = element_rect(fill="#242f39", linewidth=1),
            plot.background = element_rect(fill = "#242f39", color = "#242f39"),
            panel.grid = element_blank(),
            axis.text = element_text(color = "white", size = 17,face = "bold"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(color = "white", hjust = 0.5, vjust = -7.5, size = 18,face = "bold"),
            #plot.title = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.title = element_blank(),
            legend.background = element_rect(fill = NA),
            legend.margin=margin(l = -20, unit='pt'),
            legend.text = element_text(color = "white", size = 15,face = "bold"),
            legend.key.height = unit(50, "pt")
          ))
    
    try(tot$date <- as_datetime(paste(tot$date,"-01",sep = "")))
    
    try(p2 <- ggplot(tot, aes(x = date, y = calc))+geom_line(color="red",alpha = 0.5)+geom_point(shape = 21,aes(fill = paste("Monthly anomaly (ref",substr(bmin,1,4),"- 2020)")), size = 1.5,alpha = 0.5)+
          geom_smooth(aes(colour = "Loess trend (span = .1)"), se = F, span = 0.1,size = 1.5)+
          scale_x_datetime(breaks = scales::pretty_breaks(n = 10))+
          scale_colour_manual(values = 'white')+
          scale_fill_manual(values = 'yellow')+
          theme(
            panel.background = element_rect(fill="#242f39", size=1),
            plot.background = element_rect(fill = "#242f39", color = "#242f39"),
            panel.grid = element_line(size = 0.025),
            axis.title = element_text(color = "white", size = 15, face = "bold"),
            axis.text = element_text(color = "white", size  = 14, face = "bold"),
            legend.background = element_blank(),
            legend.position = c(0.42,-0.095),
            legend.text = element_text(size = 15, face = "bold", color = "white"),
            legend.key = element_rect(fill = "#242f39", color = NA),
            legend.box = "horizontal"
          )+labs(x = "", y = "Monthly T° anomaly (°C)", colour = "", fill = "")+
          guides(fill = guide_legend(override.aes = list(size = 5,alpha = 1))))
    return(list(p1,p2))
  }
  
  bplot <- function(lieu, tt){
    
    dat <- tt
    try(my <- min(as.numeric(substr(dat$dateheure,1,4))))
    try(My <- max(as.numeric(substr(dat$dateheure,1,4))))
    sp <- " "
    try(dat$mois <- substr(month(dat$dateheure, label = TRUE,locale = "en_US.utf8"),1,3))
    try(hcboxplot(
      outliers = FALSE,
      x = dat$val,
      var = dat$mois,
      name = "",
      pointWidth = 10
    )%>%
      hc_title(text = "",style = list(fontSize = "24px",color = 'red')) %>%
      hc_yAxis(
        title = list(text = paste("Monthly temperature (°C) at ",lieu,sp,my,"-",My,sep = ""), style = list(fontSize = "0.7vw", color = "white"), margin = 0),
        gridLineWidth = 0.8,labels = list(style = list(fontSize = "0.6vw",color = "white")))%>%
      hc_xAxis(categories = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
               gridLineWidth = 0.8,labels = list(style = list(color = "white",fontSize = "0.6vw")))%>%
      hc_chart(events = list(
        load = JS("function() {
        var chart = this;
        var points = this.series[0].points,
                color,
                length = points.length;

            Highcharts.each(points, function(point, i){
                color = 'rgba(200,' + Math.floor(i * 200 / length) + ', 50,0.3)'
                point.update({ fillColor: color }, false)
            });
            var points = this.series[0].points,
                color,
                length = points.length;

            Highcharts.each(points, function(point, i){
                color = 'rgb(200,' + Math.floor(i * 200 / length) + ', 50)'
                point.update({ color: color }, false)
            });
          this.redraw();
      }")
      ),
      spacingTop = 2,
      spacingLeft = 0,
      spacingRight = 0,
      spacingBottom = 0)%>%
      hc_plotOptions(
        boxplot = list(
          boxwidth = 0
        )
      )%>%
      highcharter::hc_exporting(
        sourceWidth = 500,
        allowHTML = TRUE,
        scale = 1,
        enabled = TRUE,
	chartOptions = list(
		xAxis = list(title = list(style = list(fontSize = "12px")),labels = list(style = list(fontSize = "12px"))),
		yAxis = list(title = list(style = list(fontSize = "12px")),labels = list(style = list(fontSize = "12px")))
	),
        buttons = list(
          contextButton = list(
            align = 'right',
            y = 0,
            symbol = 'menu',
            symbolStroke = "yellow",
            theme = list(fill = 'rgba(255, 255, 255,0.2)',
                         states = list(
                           hover = list(fill = 'rgba(255, 255, 255,0.2)'),
                           select = list(fill = 'rgba(255, 255, 255,0.2)')
                         )),
            menuItems = c('downloadPNG', 'downloadJPEG')
          )
        )
      )%>%
      hc_add_theme(hc_theme_db())
    )
  }
  
  graphpluv <- function(lieu,plu){
    
    dataplu <<- plu
    
    try(dtsum <- xts(dataplu$val, order.by = dataplu$dateheure))
    
    try(weekly <- round(apply.weekly(dtsum, 'sum', na.rm = T),1))
    
    try(aggdata <- fortify(weekly))
    
    try(colnames(aggdata) <- c("dateheure","val"))
    try(aggdata$val <- round(aggdata$val,1))
    try(dataplu <- aggdata)
    try(dataplu <- dataplu[!is.na(dataplu$val),])
    try(dataplu <<- dataplu)
    
    try(highchart(type = "stock")%>%
          hc_legend(enabled = TRUE, 
                    layout = "horizontal", 
                    align = "left", 
                    verticalAlign = "top",
                    itemStyle = list(fontSize = "0.8vw"))%>%
          hc_add_series(name = paste("total precipitation (mm)"),dataplu,"spline", 
                        hcaes(as.Date(dataplu$dateheure, 'utc'),
                              round(dataplu$val,1)),color = "cyan",lineWidth = 1.4)%>%
          hc_title(
            text = paste("Total weekly precipitation (mm) at",lieu),
            margin = -25,
            align = "center",
            style = list(color = "white",fontWeight = 'bold',fontSize = "1vw", useHTML = TRUE)
          )%>%
          hc_xAxis(type='datetime', labels = list(style = list(fontSize = "0.8vw", color = "white")), maxZoom = 3000*3600*1000)%>%
          hc_yAxis(title = list(text = "Total precipitation (mm)", style = list(color = "white", fontSize = "1vw")),
                   opposite = F,labels = list(style = list(fontSize = "0.8vw", color = "white")))%>%
          hc_rangeSelector(enabled = FALSE)%>%
          hc_plotOptions(
            splint = list(color = "#3677d1"),
            areasplinerange = list(opacity = 0.2,fillColor = "rgba(255, 255, 255, 1)")
            #series = list(dataGrouping = list(enabled = TRUE,
            #                                 forced = TRUE,
            #                                units = list(list('day',c(7,15,30,60,120))),
            #                               groupPixelWidth = 10))
          )%>%
          hc_navigator(
            maskFill = "rgba(255, 255, 255, 0.1)",
            outlineColor = "none",
            outlineWidth = 1,
            height = 10,
            series = list(
              color = "yellow",
              lineWidth = 1,
              type = "areaspline", # you can change the type
              fillColor = "rgba(255, 0, 0, 0.2)"
            ),
            handles = list(
              backgroundColor = "rgba(255, 255, 255, 0.2)",
              borderColor = "white"
            )
          )%>%
          hc_credits(
            enabled = TRUE,
            text = "Highcharts.com, Data sources : Meteostat",
            href = "https://www.highcharts.com/"
          )%>%
          highcharter::hc_exporting(
            allowHTML = TRUE,
            sourceWidth = 800,
            scale = 1,
            enabled = TRUE,
            chartOptions = list(
              navigator = list(enabled = FALSE),
              scrollbar = list(enabled = FALSE),
              xAxis = list(
                labels = list(
                  style = list(
                    fontSize = "15px"
                  )
                )
              ),
              legend = list(
                itemStyle = list(
                  fontSize = "12px"
                )
              ),
              yAxis = list(
                labels = list(
                  style = list(
                    fontSize = "15px"
                  )
                ),
                title = list(
                  style = list(
                    fontSize = "15px"
                  )
                )
              ),
              title = list(
                style = list(
                  fontSize = "15px"
                )
              )
            ),
            buttons = list(
              contextButton = list(
                align = 'right',
                y = 0,
                x = -50,
                symbol = 'menu',
                symbolStroke = "yellow",
                theme = list(fill = 'rgba(255, 255, 255,0.2)',
                             states = list(
                               hover = list(fill = 'rgba(255, 255, 255,0.2)'),
                               select = list(fill = 'rgba(255, 255, 255,0.2)')
                             )),
                menuItems = c('downloadPNG', 'downloadJPEG')
              )
            )
          )%>%
          hc_boost(
            enabled = T
          )%>%
          hc_chart(
            backgroundColor = "rgba(36, 47, 57,0.6)",
            zoomType = "x",
            borderColor = "cyan",
            borderRadius = 5,
            borderWidth = 0
          )%>%
          hc_add_theme(hc_theme_db(opacity = 0.5))%>%
          hc_scrollbar(
            barBackgroundColor = "gray",
            barBorderRadius = 7,
            barBorderWidth = 0,
            buttonBackgroundColor = "gray",
            buttonBorderWidth = 0,
            buttonArrowColor = "white",
            buttonBorderRadius = 7,
            rifleColor = "white",
            trackBackgroundColor = "rgb(36, 47, 57)",
            trackBorderWidth = 1,
            trackBorderColor = "silver",
            trackBorderRadius = 7
          ))
  }
  
  graphvent <- function(lieu, vent){
    
    datavent <<- vent
    
    try(dtmean <- xts(datavent$val, order.by = datavent$dateheure))
    
    try(weekly <- round(apply.weekly(dtmean, 'mean', na.rm = T),1))
    
    try(aggdata <- fortify(weekly))
    
    try(colnames(aggdata) <- c("dateheure","val"))
    
    try(datavent <- aggdata)
    try(datavent <- datavent[!is.na(datavent$val),])
    try(datavent <<- datavent)
    
    try(highchart(type = "stock")%>%
          hc_legend(enabled = TRUE, 
                    layout = "horizontal", 
                    align = "left", 
                    verticalAlign = "top",
                    itemStyle = list(fontSize = "0.8vw"))%>%
          hc_add_series(name = paste("Wind speed km/h"),datavent,"spline", 
                        hcaes(as.Date(datavent$dateheure, 'utc'),
                              round(datavent$val,1)),color = "#51c24e",lineWidth = 1.4)%>%
          hc_title(
            text = paste("Weekly average wind speek (km/h) at",lieu),
            margin = -25,
            align = "center",
            style = list(color = "white",fontWeight = 'bold',fontSize = "1vw", useHTML = TRUE)
          )%>%
          hc_xAxis(type='datetime', labels = list(style = list(fontSize = "0.8vw", color = "white")), maxZoom = 3000*3600*1000)%>%
          hc_yAxis(title = list(text = "Wind speed (km)", style = list(color = "white", fontSize = "1vw")),
                   opposite = F,labels = list(style = list(fontSize = "0.8vw", color = "white")))%>%
          hc_rangeSelector(enabled = FALSE)%>%
          hc_plotOptions(
            splint = list(color = "#3677d1"),
            areasplinerange = list(opacity = 0.2,fillColor = "rgba(255, 255, 255, 1)")
            #series = list(dataGrouping = list(enabled = TRUE,
            #                                 forced = TRUE,
            #                                units = list(list('day',c(7,15,30,60,120))),
            #                               groupPixelWidth = 10))
          )%>%
          hc_navigator(
            maskFill = "rgba(255, 255, 255, 0.1)",
            outlineColor = "none",
            outlineWidth = 1,
            height = 10,
            series = list(
              color = "red",
              lineWidth = 1,
              type = "areaspline", # you can change the type
              fillColor = "rgba(255, 0, 0, 0.2)"
            ),
            handles = list(
              backgroundColor = "rgba(255, 255, 255, 0.2)",
              borderColor = "white"
            )
          )%>%
          hc_credits(
            enabled = TRUE,
            text = "Highcharts.com, Data sources : Meteostat",
            href = "https://www.highcharts.com/"
          )%>%
          highcharter::hc_exporting(
            navigator = list(enabled = FALSE),
            scrollbar = list(enabled = FALSE),
            allowHTML = TRUE,
            sourceWidth = 800,
            scale = 1,
            enabled = TRUE,
            chartOptions = list(
              xAxis = list(
                labels = list(
                  style = list(
                    fontSize = "15px"
                  )
                )
              ),
              legend = list(
                itemStyle = list(
                  fontSize = "12px"
                )
              ),
              yAxis = list(
                labels = list(
                  style = list(
                    fontSize = "15px"
                  )
                ),
                title = list(
                  style = list(
                    fontSize = "15px"
                  )
                )
              ),
              title = list(
                style = list(
                  fontSize = "15px"
                )
              )
            ),
            buttons = list(
              contextButton = list(
                align = 'right',
                y = 0,
                x = -50,
                symbol = 'menu',
                symbolStroke = "yellow",
                theme = list(fill = 'rgba(255, 255, 255,0.2)',
                             states = list(
                               hover = list(fill = 'rgba(255, 255, 255,0.2)'),
                               select = list(fill = 'rgba(255, 255, 255,0.2)')
                             )),
                menuItems = c('downloadPNG', 'downloadJPEG')
              )
            )
          )%>%
          hc_boost(
            enabled = T
          )%>%
          hc_chart(
            backgroundColor = "rgba(36, 47, 57,0.6)",
            zoomType = "x",
            borderColor = "#20781e",
            borderRadius = 5,
            borderWidth = 0
          )%>%
          hc_add_theme(hc_theme_db())%>%
          hc_scrollbar(
            barBackgroundColor = "gray",
            barBorderRadius = 7,
            barBorderWidth = 0,
            buttonBackgroundColor = "gray",
            buttonBorderWidth = 0,
            buttonArrowColor = "white",
            buttonBorderRadius = 7,
            rifleColor = "white",
            trackBackgroundColor = "rgb(36, 47, 57)",
            trackBorderWidth = 1,
            trackBorderColor = "silver",
            trackBorderRadius = 7
          ))
    
  }
}  

