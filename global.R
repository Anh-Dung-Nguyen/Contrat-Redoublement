library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

notes_etudiants <- list()

# Charger les fichiers ui et server
source("ui.R")
source("contrat_notes.R")

source("server.R")

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
