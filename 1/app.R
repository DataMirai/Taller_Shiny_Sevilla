
################################################################################
## PASO 1️⃣ TALLER ##############################################################
################################################################################

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

## Siguiendo la lógica de {shinydashboard}, necesitamos definir 3 objetos:
## header, sidebar y body.

header <- shinydashboard::dashboardHeader()

sidebar <- shinydashboard::dashboardSidebar()

body <- shinydashboard::dashboardBody()

