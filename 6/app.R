## Cargamos librerías

library(shiny)
library(shinydashboard)
#library(shinyWidgets)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)
library(reactable)
#library(rsconnect) #En caso de querer subir el Shiny a un servidor.



## Subimos los datos que usaremos. En este caso el dataset ya está limpio, pero
## si hubiera que hacer cambios se podrían hacer aquí mismo (no recomendable si 
## son cambios grandes, para ello abrir un script a parte.)

## ⚠️¡ATENCIÓN!⚠️ No usar la función setwd() si la intención es subir la app a un 
## servidor. Dará error.

datos_contaminacion <- read_csv("Contaminantes_Sevilla.csv") %>% 
  st_as_sf(coords = c("y", "x")) %>% 
  st_set_crs(4326)

## Si el archivo estuviera dentro de una carpeta (siempre dentro de la carpeta 
## del proyecto):

## datos_contaminacion <- read_csv("/nombre_carpeta/Contaminantes_Sevilla.csv")

################################################################################
## COMENZAMOS LA APP AQUÍ ######################################################
################################################################################
## Siguiendo la lógica de {shinydashboard}, necesitamos definir 3 objectos:
## header, sidebar y body.


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

## SIDEBAR #####################################################################

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

server <- function(input, output, session) {
  
  dataset_filtrado <- reactive({
    datos_contaminacion %>% 
      filter(datos_contaminacion$F_FECHA >= min(input$slider_fecha) & datos_contaminacion$F_FECHA <= max(input$slider_fecha),
             datos_contaminacion$Contaminante == input$select_contaminante,
             datos_contaminacion$D_ESTACION %in% c(input$select_estaciones))
  })
  
  
  output$infobox_municipio <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox("Municipio", "Sevilla", icon = icon("city"), color="light-blue", fill = TRUE)
  })   
  
  output$infobox_media_periodo <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox("Media periodo", round(mean(dataset_filtrado()$Registro_contaminante, na.rm = TRUE),2), icon = icon("city"), color="green")
  })   
  
  output$infobox_ultimo_registro <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox("Último registro", max(dataset_filtrado()$F_FECHA), icon = icon("city"), color="yellow")
  })
  
  output$grafico_lineas <- renderPlotly({
    
    plot <- dataset_filtrado() %>% 
      ggplot(aes(x = F_FECHA, y = Registro_contaminante, color = D_ESTACION), group = D_ESTACION)+
      geom_line()+
      theme_bw()+
      labs(title = paste("Cantidad", input$select_contaminante,"en aire por estación de vigilancia"),
           x = "Fecha",
           y = "μg/m3",
           color = "Estaciones: ") +
      theme(legend.position = "bottom")
    
    ggplotly(plot)
    
  })
  
  output$mapa_leaflet <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("Stadia.AlidadeSmoothDark") %>%
      setView(-5.98818, 37.3905, zoom = 11) %>%
      addCircleMarkers(data = datos_contaminacion,
                       radius = 10,
                       color = "lightblue",
                       opacity = 0.02,
                       fillOpacity = 0.01,
                       popup = ~datos_contaminacion$D_ESTACION,
                       layerId = ~id) ## Es mejor si el ID es un número
  })
  
  
  observe({
    event <- input$mapa_leaflet_marker_click
    if (is.null(event))
      return(
        output$tabla_reactable <- renderReactable({
          datos_tabla_sin_filtros <- datos_contaminacion %>% 
            select(Estación = D_ESTACION, Fecha = F_FECHA, Contaminante, `Valor Registro` = Registro_contaminante) %>% 
            mutate(`Valor Registro` = round(`Valor Registro`, 2))
          
          reactable(datos_tabla_sin_filtros, 
                    searchable = TRUE, 
                    striped = TRUE,
                    columns = list(
                      geometry = colDef(show =F))
          )
        })
      ) 
    
    datos_tabla_con_filtros <- datos_contaminacion %>% 
      filter(id == event$id) %>% 
      select(Estación = D_ESTACION, Fecha = F_FECHA, Contaminante, `Valor Registro` = Registro_contaminante) %>% 
      mutate(`Valor Registro` = round(`Valor Registro`, 2))
    
    output$tabla_reactable <- renderReactable({
      reactable(datos_tabla_con_filtros, 
                searchable = TRUE, 
                striped = TRUE,
                columns = list(
                  geometry = colDef(show =F))
      )
    })
  })
  
  
  
}

shinyApp(ui, server)