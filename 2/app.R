################################################################################
## PASO 2️⃣ TALLER ##############################################################
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

## HEADER ######################################################################

## Si quisiéramos poner solamente texto en el título del dashboard basta con 
## añadir texto al argumento 'title'.

# header <- shinydashboard::dashboardHeader(title = "Contaminación Sevilla")

## Si en lugar de texto nos interesa añadir una imagen (por ejemplo un logo),
## tendremos que usar etiquetas de html, en este caso la etiqueta <img>.

## Para evitar deformaciones y adaptar la imagen al espacio del dashboard,
## definimos los argumentos 'height' y 'width'.

header <- shinydashboard::dashboardHeader(title = tags$img(src='https://raw.githubusercontent.com/DataMirai/ShinyCarreterasInundaciones/refs/heads/main/LOGO_redondas_negro_fondoBlanco.png',
                                                           height='40', width='210'))

## En caso de querer usar una imagen local, añadirla dentro de la carpeta del Shiny,
## no de la del proyecto.

## SIDEBAR #####################################################################

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(h4("Menú desplegable", style="margin:0px; margin-left:10px; padding: 0px;"), 
                             tabName = "menu1", # Es necesario un ID que relacione el elemento con el resto de la app.
                             startExpanded = TRUE,
                             shinydashboard::menuSubItem("A", tabName = "A_id", icon = icon("users")), 
                             # Al añadir Subitems, el menú se vuelve desplegable
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
                              
                              shinydashboard::box( width = 12, height = 120, style="font-size: 18px;"),
                              
                              ## Segundo recuadro:
                              
                              shinydashboard::box( width = 12, height = 700, style="font-size: 18px;"),
                              
                              ## Tercer recuadro:
                              
                              shinydashboard::box( width = 12, 
                                                   h3("Haz click en una estación del mapa:")
                              )),
      shinydashboard::tabItem("B_id", ## Id que relacione la pestaña con la barra lateral (sidebar)
                              h2("Esta es la pestaña B", style="margin:15px;"),
                              
                              h3("Está vacía pero puedes probar a añadirle lo que quieras:)", style="margin:15px;"))
    )
  )
)


