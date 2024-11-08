
################################################################################
## PASO 4️⃣ TALLER ##############################################################
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
    shinydashboard::infoBox("Media periodo", round(mean(dataset_filtrado()$Registro_contaminante, na.rm = TRUE),2), 
                            icon = icon("city"), color="green")
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
  
}
