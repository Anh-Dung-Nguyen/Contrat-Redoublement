library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(rhandsontable) 
library(writexl)

source("contrat_notes.R")
fichiers <- list.files("Liste+Bilan contrat", pattern = "*.R", full.names = TRUE)


server <- function(input, output, session) {
  
  lesRedoublants_reactive <- reactiveVal(NULL)
  notes_reactives <- reactiveVal(NULL)
  
  selected_student <- reactive({
    req(input$select_value)
    lesRedoublants <- lesRedoublants_reactive()
    selected <- strsplit(input$select_value, " ")[[1]]
    nom <- selected[1]
    prenom <- selected[2]
    
    index <- which(toupper(lesRedoublants$Nom) == nom & toupper(lesRedoublants$Prénom) == toupper(prenom))
    req(length(index) > 0)
    
    res <- list(
      nom = lesRedoublants$Nom[index],
      prenom = lesRedoublants$Prénom[index],
      id = lesRedoublants$`Code apprenant`[index]
    )
    res
  })
  
  observeEvent(input$fichier_en_tete_jury, {
    print("debut du traitement du fichier_en_tete_jury")
    file <- input$fichier_en_tete_jury$datapath
    data <- read_excel(file)
    print(head(data))
    filtered_data <- data %>%
      filter(`Decision finale` == "Red") %>%
      select(Nom, Prénom, `Code apprenant`)
    lesRedoublants_reactive(filtered_data)
    print("fin du traitement du fichier_en_tete_jury")
  })
  
  output$dynamic_select <- renderUI({
    lesRedoublants <- lesRedoublants_reactive()
    req(nrow(lesRedoublants) > 0)
    
    choices <- paste(toupper(lesRedoublants$Nom), lesRedoublants$Prénom)
    selectInput("select_value", "Choisir un étudiant redoublant",
                choices = choices)
  })
  
  output$text <- renderText({
    req(input$select_value)
    paste("Vous avez sélectionné : ", input$select_value)
  })
  
  observeEvent(input$Bouton1, {
    print("generation du contrat d'un étudiant")
    student <- selected_student()
    req(student)
    
    nom <- student$nom
    prenom <- student$prenom
    id <- student$id
    cle <- paste(nom, prenom)
    
    print(paste("Nom:", nom, "- Prénom:", prenom, "- ID:", id))
    
    if (is.null(notes_etudiants[[cle]])) {
      notes <- generation_df_notes(id)
      notes_etudiants[[cle]] <<- notes
    } else {
      notes <- notes_etudiants[[cle]]
    }
    
    if ("EcValRepasse" %in% names(notes)) {
      notes$EcValRepasse <- as.character(notes$EcValRepasse)
    }
    notes_reactives(notes)
    print("affichage du tableau de notes")
  })
  
  output$table_notes <- renderRHandsontable({
    print("test de l'existence du tableau de notes")
    req(notes_reactives())
    print("ok le tableau de notes existe, on peut l'afficher")
    
    notes <- notes_reactives()
    table <- rhandsontable(notes, readOnly = TRUE)
    
    if ("EcValRepasse" %in% names(notes)) {
      table <- hot_col(table, "EcValRepasse", readOnly = FALSE)
    }
    
    table
  })
  
  observeEvent(input$table_notes, {
    req(input$table_notes)
    student <- selected_student()
    nom <- student$nom
    prenom <- student$prenom
    
    updated_df <- hot_to_r(input$table_notes)
    
    if ("EcValRepasse" %in% names(updated_df)) {
      updated_df$EcValRepasse <- as.character(updated_df$EcValRepasse)
    }
    
    notes_reactives(updated_df)
    notes_etudiants[[paste(nom, prenom)]] <<- updated_df
    print(paste("sauvegarde pour : ", nom, prenom))
    print(head(updated_df))
  })
  
  observeEvent(input$Bouton2, {
    req(input$select_value)
    req(notes_reactives())
    
    student <- selected_student()
    nom <- student$nom
    prenom <- student$prenom
    id <- student$id
    
    print(paste("Étudiant sélectionné : ", prenom, nom, "- ID:", id))
    print("Données réactives du tableau :")
    print(notes_reactives())
    
    notes_modifiees <- notes_etudiants[[paste(nom, prenom)]]
    fichier_sortie <- paste0("contrat_", nom, "_", prenom, ".docx")
    
    tryCatch({
      doc <- read_docx()
      doc <- generation(id, doc, notes_etudiant = notes_modifiees)
      print(doc, target = fichier_sortie)
      showNotification(paste("Contrat généré pour", prenom, nom), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lors de la génération du contrat : ", e$message), type = "error")
    })
  })
  observeEvent(input$Bouton3, {
    print("Appel de la fonction listeContratFunction()")
    tryCatch({
      listeContratFunction()
      showNotification("L'a liste des contrats a été générée'ensemble des contrats des redoublants a été généré.", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur dans listeContratFunction : ", e$message), type = "error")
    })
  })
  
  observeEvent(input$Bouton4, {
    print("Appel de la fonction bilanFunction()")
    tryCatch({
      bilanFunction()
      showNotification("Le bilan a été généré.", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur dans bilanFunction : ", e$message), type = "error")
    })
  })
  
  
  
  
}
