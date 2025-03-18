library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

lesContratsEt<- c()
lesNomsEt<-c()

# Charger les fichiers ui et server
source("ui.R")
source("server.R")

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
