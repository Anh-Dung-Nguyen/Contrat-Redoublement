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
    paste("Vous avez sélectionné : ", input$select_value)
  })
  
  selected_student <- reactive({
    data <- data_reactive()
    req(data, input$select_value)
    
    # Extraire le nom et prénom depuis input$select_value
    selected <- strsplit(input$select_value, " ")[[1]]
    nom <- selected[1]
    prenom <- selected[2]
    
    # Utiliser match() pour trouver l'index du nom et prénom dans le dataframe
    index <- match(paste(nom, prenom), paste(data$Nom, data$Prénom))
    req(index)  # S'assurer que l'index est trouvé
    
    list(nom = data$Nom[index], prenom = data$Prénom[index])
  })
  
  # Créer une valeur réactive pour stocker les notes de l'étudiant sélectionné
  notes_reactives <- reactiveVal(NULL)
  
  observeEvent(input$select_value, {
    student <- selected_student()
    req(student)
    
    # Générer les notes et les stocker dans la valeur réactive
    notes <- generation_df("Nom1", "Prenom1")
    notes_reactives(notes)
  })
  
  
  
  # Afficher les notes dans un tableau dynamique
  output$table_notes <- renderDT({
    req(notes_reactives())  # Vérifier que notes_reactives() n'est pas NULL
    datatable(notes_reactives(), options = list(pageLength = 10))
  })
}
