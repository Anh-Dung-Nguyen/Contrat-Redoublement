library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(DT)

server <- function(input, output, session) {
  
  # Traitement du fichier de jury pour récupérer les données Nom et Prénom des étudiants ayant pour Résultat REDOUBLE
  data_reactive <- reactiveVal(NULL)
  observeEvent(input$fichier_en_tete_jury, {
    req(input$fichier_en_tete_jury)
    file <- input$fichier_en_tete_jury$datapath
    data <- read_excel(file)
    filtered_data <- data %>%
      filter(`Decision finale` == "Red") %>%
      select(Nom, Prénom)
    data_reactive(filtered_data)
  })
  
  # Mise à jour dynamique de la liste déroulante
  output$dynamic_select <- renderUI({
    data <- data_reactive()
    req(data)  
    choices <- paste(toupper(data$Nom), data$Prénom)  
    selectInput("select_value", "Choisir un étudiant redoublant", 
                choices = unique(choices)) 
  })
  
  # A modifier pour afficher le contrat et pas le nom de l'étudiant sélectionné
  output$text <- renderText({
    req(input$select_value)
    data <- data_reactive()
    req(data)
    generation("Nom1", "Prenom1")
    output$table <- renderDT(datatable(notes_etudiant))
    paste("Vous avez sélectionné : ", input$select_value)
  })
}