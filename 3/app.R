################################################################################
## PASO 3️⃣ TALLER ##############################################################
################################################################################

library(shiny)
library(shinydashboard)
#library(shinyWidgets)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)
library(reactable)
#library(rsconnect) 

datos_contaminacion <- read_csv("Contaminantes_Sevilla.csv") %>% 
  st_as_sf(coords = c("y", "x")) %>% 
  st_set_crs(4326)

################################################################################
## COMENZAMOS LA APP AQUÍ ######################################################
################################################################################

header <- shinydashboard::dashboardHeader(title = tags$img(src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/LOGO_redondas_negro_fondoBlanco.png',
                                                           height='40', width='210'))


sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(h4("Menú desplegable", style="margin:0px; margin-left:10px; padding: 0px;"), 
                             tabName = "menu1", 
                             startExpanded = TRUE,
                             shinydashboard::menuSubItem("A", tabName = "A_id", icon = icon("users")), 
                             shinydashboard::menuSubItem("B", tabName = "B_id", icon = icon("map-location-dot"))
    )))


## BODY ########################################################################

body <- shinydashboard::dashboardBody(
  fluidRow( 
    
    shinydashboard::tabItems(
      
      
      shinydashboard::tabItem("A_id", ## Id que relacione la pestaña con la barra lateral (sidebar)
                              h2("Contaminación en Sevilla", style="margin:15px;"),
                              
                              h3("Por estaciones de vigilancia", style="margin:15px;"),
                              
                              ## Primer recuadro de la estructura del dashboard (Siguiendo el ppt):
                              
                              shinydashboard::box( width = 12, height = 120, style="font-size: 18px;", 
                                                   infoBoxOutput("infobox_municipio", width = 4),
                                                   # Se asigna un id a la caja de información y una anchura (3x4)
                                                   infoBoxOutput("infobox_media_periodo", width = 4),
                                                   infoBoxOutput("infobox_ultimo_registro", width = 4)),
                              
                              ## Segundo recuadro:
                              
                              shinydashboard::box( width = 12, height = 700, style="font-size: 18px;", 
                                                   fluidRow( 
                                                     column(width = 4, style="padding:0px 30px;", 
                                                            selectInput("select_contaminante", 
                                                                        label = "Selecciona un contaminante:", 
                                                                        choices = unique(datos_contaminacion$Contaminante), 
                                                                        selected = "PM10"),
                                                            multiple = FALSE),
                                                     
                                                     column(width = 4, style="padding:0px 30px;", 
                                                            selectizeInput("select_estaciones", 
                                                                           label = "Selecciona estaciones:", 
                                                                           choices = unique(datos_contaminacion$D_ESTACION),
                                                                           selected = c(unique(datos_contaminacion$D_ESTACION)),
                                                                           multiple = TRUE)),
                                                     
                                                     column(width = 4, style="padding:0px 30px;", 
                                                            sliderInput("slider_fecha", 
                                                                        label = "Selecciona periodo:", 
                                                                        min = min(datos_contaminacion$F_FECHA), 
                                                                        max = max(datos_contaminacion$F_FECHA), 
                                                                        value = c(min(datos_contaminacion$F_FECHA), max(datos_contaminacion$F_FECHA))),
                                                     ),
                                                     
                                                     fluidRow( 
                                                       column(width = 12, style="padding:0px 30px;", 
                                                              plotlyOutput("grafico_lineas", height = 500)))
                                                   )),
                              
                              ## Tercer recuadro:
                              
                              shinydashboard::box( width = 12, 
                                                   h3("Haz click en una estación del mapa:"),
                                                   column(width = 6, 
                                                          leaflet::leafletOutput("mapa_leaflet", height = 500)),
                                                   column(width = 6,
                                                          reactableOutput("tabla_reactable", height = 500))
                              )
      ),
      shinydashboard::tabItem("B_id", ## Id que relacione la pestaña con la barra lateral (sidebar)
                              h2("Esta es la pestaña B", style="margin:15px;"),
                              
                              h3("Está vacía pero puedes probar a añadirle lo que quieras:)", style="margin:15px;"))
    )
  )
)

ui <- dashboardPage(title = "Shiny Sevilla", skin = "black", header, sidebar, body) 


