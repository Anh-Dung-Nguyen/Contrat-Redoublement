library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(DT)
library(rhandsontable) 


ui <- fluidPage(
  titlePanel("Génération des contrats de redoublants"), 
  sidebarLayout(
    sidebarPanel(width = 4, style = "height: 100vh; overflow-y: auto;",
                 fileInput("fichier_en_tete_jury", "Ajouter le fichier d'en-tête jury", accept = ".xlsx"), 
                 fileInput("fichierjury","Ajouter le fichier de jury", accept = ".xlsx"),
                 uiOutput("dynamic_select"),
                 actionButton("Bouton1", "Génération du tableau de notes de l'étudiant sélectionné",  style = "white-space: normal; word-wrap: break-word; width: 100%;"),
                 actionButton("Bouton2","Génération du contrat de l'étudiant sélectionné",  style = "white-space: normal; word-wrap: break-word; width: 100%;;margin-top: 15px;" ),
                 actionButton("Bouton3", "Génération de l'ensemble des contrats des redoublants",   style = "white-space: normal; word-wrap: break-word; width: 100%;margin-top: 15px;"),
                 actionButton("Bouton4","Génération du fichier de bilan", style = "white-space: normal; word-wrap: break-word; width: 100%;margin-top: 15px;")
                 
    ),
    mainPanel(
      textOutput("text"),
      rHandsontableOutput("table_notes")
    )
  )
)