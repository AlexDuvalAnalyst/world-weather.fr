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


fluidPage(useShinyjs(),
          #tags$style(HTML("::placeholder { /* Chrome, Firefox, Opera, Safari 10.1+ */
          #                color: white;
          #               opacity: 1; /* Firefox */}")),
          tags$head(
            tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight: bold;
        position:absolute;
        top:25vh;
        width:80vh;
        font-size:2em;
        background-color:rgba(255,255,255,0.7);
        border-radius:7px;
        box-shadow: 0 6px 4px 0 rgba(0,0,0,0.3);
        padding:20px;
        z-index:3000;
      }
    ")),
            tags$style(
              type="text/css",
              "#graphtrend img {max-width: 100%; width: 100%; height: 100%}"
            ),
            tags$style(
              type="text/css",
              "#graphspiral img {max-width: 100%; width: 100%; height: 100%}"
            )
          ),
          theme = "main.css",
          class = "wrapper",
          tags$script(src = "https://kit.fontawesome.com/54af662477.js"),
          tags$div(
            class = "left-menu",
            tags$div(
              class = "gmap",
              actionButton(title = "See all stations available",'map', class = "btnmap", '', icon = icon("earth-americas", class="fa-solid fa-earth-americas", 
                                                                                                         style = "font-size: 1.8vw;text-shadow: 0px 5px 5px rgba(0,0,0,0.9);
                                                            position:absolute;top:1.2vh;right:1.5vh;"))
            ),
            tags$div(
              class = "pluv",
              actionButton(title = "Precipitation datas",'plu',class = "btnpluv",'', icon = icon("droplet", class="fa-solid fa-droplet", 
                                                                                                 style = "font-size: 1.8vw;text-shadow: 0px 5px 5px rgba(0,0,0,0.9);
                                                           position:absolute;top:1.2vh;right:1.9vh;"))
            ),
            tags$div(
              class = "wind",
              actionButton(title = "Wind speed datas",'win',class = "btnwind",'', icon = icon("wind", class="fa-solid fa-wind",
                                                                                              style = "font-size: 1.8vw;text-shadow: 0px 5px 5px rgba(0,0,0,0.9);
                                                           position:absolute;top:1.2vh;right:1.5vh;"))
            ),
            tags$div(
              class = "infos",
              actionButton(title = "More informations",'inf',class = "btninfos",'', icon = icon("circle-question", class="fa-solid fa-circle-question",
                                                                                                style = "font-size: 1.8vw;text-shadow: 0px 5px 5px rgba(0,0,0,0.9);
                                                           position:absolute;top:0vh;right:1.5vh;"))
            )
          ),
          tags$div(class = "headerpage",
                   tags$i(class="fa-solid fa-atom fa-spin", style = "font-size:5vh;position:absolute;right:90px;bottom:2.2vh;opacity:0.5;
                   --fa-animation-duration: 10s;z-index:3000"),
                   tags$img(src = "worldweather.png", style = "height:4.5vh;position:absolute;top:2.5vh;left:1.3vw;"),
                   shinyjs::hidden(tags$div(id = "control-trigger",style = "position:absolute;",
                                            numericInput(inputId = "trigger",label = "trigger-go",value = 1))),
                   tags$div(class = "recherche",
                            selectizeInput(
                              inputId = "recherche",
                              label = "",
                              multiple = FALSE,
                              choices = c("Search Bar" = "", character(0)),
                              options = list(
                                loadThrottle=200, loadingClass="",
                                create = F,
                                placeholder = "Search for a location..",
                                highlight = FALSE,
                                maxItems = "1",
                                onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                onInitialize = I('function() {
        var selectize = this;
        this.$control.on("keydown", function() {
          if(selectize.getValue() !== ""){
            selectize.setValue("", true);
          }
        });
      }')
                              )
                            ),
                            actionButton('valid',class = "btn1",'', icon = icon("paper-plane", class="fa-sharp fa-solid fa-paper-plane"))),
                   tags$div(class = "slide",
                            sliderInput("range", "",
                                        min = as.Date("2000-01-01"), max = as.Date("2023-01-01"),
                                        value = c(as.Date("2009-01-01"),as.Date("2022-10-01")),
                                        ticks = FALSE)
                   ),
                   tags$div(class = "btngo",
                            actionButton('go',class = "btn2",'GO!'))),
          tags$div(
            class = "card-graph",
            withSpinner(highchartOutput("graphtemp",height = '53vh'),hide.ui =FALSE,type=6,color = "yellow")
          ),
          tags$div(
            class = "card-spiral",
            withSpinner(plotOutput("graphspiral", height = "53vh", width = "100%"),hide.ui =FALSE,type=6,color = "yellow"),
            downloadButton(class = 'dwlspr', 'downloadspiral','')
          ),
          tags$div(
            class = "card-norm",
            withSpinner(plotOutput("graphtrend", height = "27.5vh", width = "100%"),hide.ui =FALSE,type=6,color = "yellow"),
            downloadButton(class = 'dwltrd','downloadtrend','')
          ),
          tags$div(
            class = "card-position",
            leafletOutput("map", width = "100%", height = "100%")
          ),
          tags$div(
            class = "card-text",
            uiOutput("trendtitle"),
            tags$div(class = "trendtext", textOutput("trendtext")),
            uiOutput("trendicon")
          ),
          tags$div(
            class = "card-bp",
            withSpinner(highchartOutput("pbp",height = '27vh'),hide.ui =FALSE,type=6,color = "yellow"),
          ),
          
          shinyjs::hidden(tags$div(id = "toggle-map",class = "card-map",
                                   actionButton('exmap', 'X',class = "exitmap"),
                                   leafletOutput("globalmap", width = "100%", height = "100%"))),
          
          shinyjs::hidden(tags$div(id = "toggle-pluv",class = "card-pluv",
                                   actionButton('explu', 'X',class = "exitmap"),
                                   withSpinner(highchartOutput("graphpluv",height = '68vh'),hide.ui =FALSE,type=6,color = "#17a1a3")
          )),
          
          shinyjs::hidden(tags$div(id = "toggle-wind",class = "card-wind",
                                   actionButton('exwin', 'X',class = "exitmap"),
                                   withSpinner(highchartOutput("graphvent",height = '68vh'),hide.ui =FALSE,type=6,color = "#20781e")
          )),
          tags$div(id = "toggle-info",class = "card-infos",
                   actionButton('exinf', 'X',class = "exitmap"),
                   tags$h1(style = "text-align:center;font-size:2em;","Welcome to World weather !"),
                   br(),
                   br(),
                   "Search your location and see the result !",
                   br(),
                   br(),
                   "World weater is a web application that allows statistical analyses on weather data, such as
                           tempeture (mainly), precipitation, or wind speed (more features may be added in the future).",
                   br(),
                   "The main goal is to give an easy analysis access of all the weater data around the world (including historical data),
                           and put climate change into perspective at local scales.",
                   br(),
                   br(),
                   "Temperature plot : The main temperature plot is built with ",tags$a(href ="https://www.highcharts.com/","Highcharts"),
                   ", and is displaying weekly mean temperatures (aggregated by month when time period is too important) + min and max values.",
                   br(),
                   "Same goes for precipitation and wind speed data.",
                   br(),
                   br(),
                   "Trend : The global trend is calculated by performing linear regression on the yearly temperatures means
                           (full year from 1st January to december 31).",
                   br(),
                   br(),
                   "Monthly temperatures boxplot (also built with ",tags$a(href ="https://www.highcharts.com/","Highcharts"),") are calculated with monthly means of the selected period of time
                           (E.g. January temperatures means aggregated from 2000 to 2020).",
                   br(),
                   br(),
                   "Temperature changes and anomalies are calculated at a monthly scale, and correspond to the difference between a given monthly average
                           temperature, and the historical average calculated on a 30 years period (1990 - 2020 when possible). The spiral visualization has been
                           adapted from 'Riffomonas Project' youtube channel.",
                   br(),
                   br(),
                   tags$h1(style = "text-align:center;font-size:1.2vw;","*** Even though some stations may be suggested when typing in search bar, data may not be available ***"),
                   
          ),
          tags$div(class = "footer",
                   tags$div(class = "liencontact",actionLink("contact", "Contact", class = "contact")),
                   shinyjs::hidden(tags$div(id = "toggle-contact",class = "textcontact",
                                            "Mail : alexandre76370@hotmail.fr",
                                            br(),
                                            "Website : ",tags$a(href = "https://alexdataproject.com/","https://alexdataproject.com/", target = "_blank"),
                                            actionButton('excontact', 'X',class = "exitmap"),
                   )),
                   tags$div(class = "liensource",actionLink("source", "Data sources",class = "source")),
                   shinyjs::hidden(tags$div(id = "toggle-source",class = "textsource",
                                            tags$h1(style = "text-align:center;font-size:2em;","Meteostat"),
                                            br(),
                                            "All of the weather and climate data comes from ",
                                            tags$a(href = "https://meteostat.net/fr/", "Meteostat"),
                                            " database, which collects data from multiple governmental / national meteorogical
                                     offices (E.g. NOAA, see all ",tags$a(href = "https://dev.meteostat.net/sources.html", target = "_blank","here"),
                                            ").",
                                            br(),
                                            "The ",tags$a(href = "https://dev.meteostat.net/python/", target = "_blank","meteostat python library"), 
                                            "is used to retrieve all the data.",
                                            actionButton('exsource', 'X',class = "exitmap"),
                   )),
                   tags$div(class = "liendisclaimer",actionLink("disclaimer", "Disclaimer",class = "disclaimer")),
                   shinyjs::hidden(tags$div(id = "toggle-disclaimer",class = "textdisclaimer",
                                            br(),
                                            "The statistical and graphical analysis produced by 'World weather' is free to use, however
                                    no responsibility shall be taken, regarding the accuracy, adequacy, validity, reliability
                                    of any elements on this application usage. You shall use this application at your own risk and
                                    you may contact 'alexandre76370@hotmail.fr' for more informations.",
                                            
                                            actionButton('exdisclaimer', 'X',class = "exitmap"),
                   )))
)
