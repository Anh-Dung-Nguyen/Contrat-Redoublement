library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

ui <- fluidPage(
  titlePanel("Génération des contrats de redoublants"), 
  sidebarLayout(
    sidebarPanel(width = 4, style = "height: 100vh; overflow-y: auto;",
                 fileInput("fichier_en_tete_jury", "Ajouter le fichier d'en-tête jury", accept = ".xslx"), 
                 fileInput("fichierjury","Ajouter le fichier de jury", accept = ".xlsx"),
                 uiOutput("dynamic_select"),
                 actionButton("Bouton1", "Génération du contrat de l'étudiant sélectionné",  style = "white-space: normal; word-wrap: break-word; width: 100%;"),
                 actionButton("Bouton2", "Génération de l'ensemble des contrats des redoublants",   style = "white-space: normal; word-wrap: break-word; width: 100%;margin-top: 15px;")
    ),
    mainPanel(
      textOutput("text")
    )
  )
)