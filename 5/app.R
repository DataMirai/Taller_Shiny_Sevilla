
################################################################################
## PASO 5️⃣ TALLER ##############################################################
################################################################################

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
