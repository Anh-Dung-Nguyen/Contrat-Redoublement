library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(DT)

server <- function(input, output, session) {
  
  # Cette variable sert a stocker la liste des étudiants redoublants
  # Elle est reactive car le menu deroulant change des que cette liste est modifiée
  lesRedoublants_reactive <- reactiveVal(NULL)
  
  notes_reactives <- reactiveVal(NULL)
  
  #selected student est aussi une variable reactive
  # en effet le df affiché sera maj des que l'etudiant selectionne est modifié
  selected_student <- reactive({
    req(input$select_value)
    #print("affichage de l'étudiant selectionné")
    #print(input$select_value)
    lesRedoublants <- lesRedoublants_reactive()
    # Extraire le nom et prénom depuis input$select_value
    selected <- strsplit(input$select_value, " ")[[1]]
    nom <- selected[1]
    prenom <- selected[2]
    #cat("Nom:", nom, "\n")
    #cat("Prénom:", prenom, "\n")
    
    # Utiliser match() pour trouver l'index du nom et prénom dans le dataframe
    # le calcul de l'index ne fonctionnait pas
    #index <- match(paste(nom, prenom), paste(lesRedoublants$Nom, lesRedoublants$Prénom))
    index<-which(toupper(lesRedoublants$Nom)==nom & lesRedoublants$Prénom==prenom)
    #cat("Index trouvé:", index, "\n")
    req(index)  # S'assurer que l'index est trouvé
    # attention mettre message d'erreur si non trouvé
    
    res<-list(nom = lesRedoublants$Nom[index], prenom = lesRedoublants$Prénom[index])
    #print("fin d'execution de selected_student")
    res
  })
  
  
  observeEvent(input$fichier_en_tete_jury, {
    print("debut du traitement du fichier_en_tete_jury")
    file <- input$fichier_en_tete_jury$datapath
    data <- read_excel(file)
    print(head(data))
    filtered_data <- data %>%
      filter(`Decision finale` == "Red") %>%
      select(Nom, Prénom)
    lesRedoublants_reactive(filtered_data)
    print("fin du traitement du fichier_en_tete_jury")
  })
  
  # Mise à jour dynamique de la liste déroulante
  output$dynamic_select <- renderUI({
    lesRedoublants <- lesRedoublants_reactive()
    req(nrow(lesRedoublants) > 0)
    
    choices <- paste(toupper(lesRedoublants$Nom), lesRedoublants$Prénom)  
    #print("affichage des choix")
    #print(choices)
    selectInput("select_value", "Choisir un étudiant redoublant", 
                choices = choices)
  })
  
  # A modifier pour afficher le contrat et pas le nom de l'étudiant sélectionné
  output$text <- renderText({
    req(input$select_value)
    paste("Vous avez sélectionné : ", input$select_value)
  })
  

   observeEvent(input$Bouton1, {
     print("generation du contrat d'un étudiant")
     student<-selected_student()
     req(student)  # Vérifier que selected_student() n'est pas NULL
     #print("affichage de l'étudiant sélectionné")
     #print(student)
     
     # Générer les notes et les stocker dans la valeur réactive
     notes <- generation_df_notes("Nom2", "Prenom2")
     print("affichage du tableau de notes")
     print(notes)
     notes_reactives(notes)
   })
  
   
    #Afficher les notes dans un tableau dynamique
    output$table_notes <- renderDT({
      print("test de l'existence du tableau de notes")
      req(notes_reactives())  # Vérifier que notes_reactives() n'est pas NULL
      print("ok le tableau de notes existe, on peut l'afficher")
      datatable(notes_reactives(), options = list(pageLength = 10))
    })
}
