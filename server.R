library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(rhandsontable) 
library(writexl)

server <- function(input, output, session) {
  
  # Cette variable sert a stocker la liste des étudiants redoublants
  # Elle est reactive car le menu deroulant change des que cette liste est modifiée
  lesRedoublants_reactive <- reactiveVal(NULL)
  
  notes_reactives <- reactiveVal(NULL)
  
  #selected student est aussi une variable reactive
  # en effet le df affiché sera maj des que l'etudiant selectionne est modifié
  selected_student <- reactive({
    req(input$select_value)
    lesRedoublants <- lesRedoublants_reactive()
    etudiant <- lesRedoublants %>% filter(ID == as.numeric(input$select_value))
    req(nrow(etudiant) == 1)
    list(nom = etudiant$Nom, prenom = etudiant$Prénom, id = etudiant$ID)
  })
  
  observeEvent(input$fichier_en_tete_jury, {
    print("debut du traitement du fichier_en_tete_jury")
    file <- input$fichier_en_tete_jury$datapath
    data <- read_excel(file)
    print(head(data))
    filtered_data <- data %>%
      filter(`Decision finale` == "Red") %>%
      mutate(ID = row_number()) %>%
      select(ID, Nom, Prénom)
    lesRedoublants_reactive(filtered_data)
    print("fin du traitement du fichier_en_tete_jury")
  })
  
  # Mise à jour dynamique de la liste déroulante
  output$dynamic_select <- renderUI({
    lesRedoublants <- lesRedoublants_reactive()
    req(nrow(lesRedoublants) > 0)
    
    noms_affiches <- paste(toupper(lesRedoublants$Nom), lesRedoublants$Prénom)
    ids <- lesRedoublants$ID
    
    choix <- setNames(ids, noms_affiches) 
    selectInput("select_value", "Choisir un étudiant redoublant",
                choices = choix)
  })
  
  observeEvent(input$Bouton1, {
    print("generation du contrat d'un étudiant")
    student <- selected_student()
    req(student)  # Vérifier que selected_student() n'est pas NULL

    nom <- student$nom
    prenom <- student$prenom
    cle <- paste(nom,prenom)
    
    print(paste("Nom:", nom, "- Prénom:", prenom))

    # Générer les notes et les stocker dans la valeur réactive
    if(is.null(notes_etudiants[[cle]])){
      notes <- generation_df_notes(student$id)
      notes_etudiants[[cle]] <<- notes
    }else {
      notes <- notes_etudiants[[cle]]
    }
    
    if ("EcValRepasse" %in% names(notes)) {
      notes$EcValRepasse <- as.character(notes$EcValRepasse)
    }
    notes_reactives(notes)
    print("affichage du tableau de notes")
  })
  
  
  # Afficher les notes dans un tableau dynamique
  output$table_notes <- renderRHandsontable({
    print("test de l'existence du tableau de notes")
    req(notes_reactives())  # Vérifier que notes_reactives() n'est pas NULL
    print("ok le tableau de notes existe, on peut l'afficher")
    
    student <- selected_student()
    nom <- student$nom
    prenom <- student$prenom
    
    notes <- notes_reactives()

    # On rend toutes les colonnes readonly par défaut...
    table <- rhandsontable(notes, readOnly = TRUE)
    
    # ... sauf la colonne "EcValRepasse" si elle existe
    if ("EcValRepasse" %in% names(notes)) {
      table <- hot_col(table, "EcValRepasse", readOnly = FALSE)
    }
    
    table
  })
  
  # Mettre à jour la valeur réactive lorsque l'utilisateur modifie le tableau
  observeEvent(input$table_notes, {
    req(input$table_notes)
    
    student <- selected_student()
    nom <- student$nom
    prenom <- student$prenom
    
    # Récupérer les données modifiées dans le tableau
    updated_df <- hot_to_r(input$table_notes)
    
    if ("EcValRepasse" %in% names(updated_df)) {
      updated_df$EcValRepasse <- as.character(updated_df$EcValRepasse)
    }
    
    notes_reactives(updated_df)
    # Mettre à jour la liste des notes de l'étudiant
    notes_etudiants[[paste(nom, prenom)]] <<- updated_df
    print(paste("sauvegarde pour : ",nom,prenom))
    print(head(updated_df))
    print(head(notes_reactives()))
  })
  
  
  observeEvent(input$Bouton3, {
    req(input$select_value)
    req(notes_reactives())  # assure-toi que notes_reactives() n'est pas NULL
    
    student <- selected_student()
    nom <- student$nom
    prenom <- student$prenom
    
    # Vérification des données de l'étudiant et de la valeur réactive du tableau
    print(paste("Étudiant sélectionné : ", prenom, nom))
    print("Données réactives du tableau :")
    print(notes_reactives())
    
    # Récupérer les données modifiées
    notes_modifiees <- notes_etudiants[[paste(nom, prenom)]]  
    
    fichier_sortie <- paste0("contrat_", nom, "_", prenom, ".docx")
    
    # Appeler la fonction de génération du contrat
    tryCatch({
      doc <- read_docx()
      doc <- generation(student$id, doc, notes_etudiant = notes_modifiees)
      print(doc, target = fichier_sortie)
      showNotification(paste("Contrat généré pour", prenom, nom), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lors de la génération du contrat : ", e$message), type = "error")
    })
  })
  
  
}
