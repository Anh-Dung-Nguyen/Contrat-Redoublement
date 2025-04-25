library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

#lesRedoublants <- data.frame(Nom = character(),
#                 Prénom = character(),
#                 stringsAsFactors = FALSE)
lesContratsEt<- c()
lesNomsEt<-c()

# Charger les fichiers ui et server
source("ui.R")
source("server.R")
source("contrat_notes.R")

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
