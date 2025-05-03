to_Entete <- function(file_path, sheet_name, data_to_write) {
  # Charger la librairie et le fichier Excel
  if (!require("openxlsx")) install.packages("openxlsx")
  library(openxlsx)
  
  wb <- loadWorkbook(file_path) # Charger le fichier Excel
  
  # Lire les données existantes (facultatif, ici pour conserver la structure)
  existing_data <- read.xlsx(wb, sheet = sheet_name)
  
  # Écrire les nouvelles données dans la feuille spécifiée
  for (i in 1:nrow(data_to_write)) {
    writeData(wb, sheet = sheet_name, x = data_to_write[i, c("ID", "Nom", "Prénom")], startRow = i + 1, startCol = 1, colNames = FALSE, rowNames = FALSE)
  }
  
  # Sauvegarder le fichier modifié
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

remplir_decision_finale <- function(file_path, sheet_name = "EnteteJury") {
  if (!require("openxlsx")) install.packages("openxlsx")
  library(openxlsx)
  
  # Charger le fichier Excel et lire les données
  wb <- loadWorkbook(file_path)
  data <- read.xlsx(wb, sheet = sheet_name)
  
  # Générer les décisions aléatoires (même nombre que lignes de données)
  set.seed(123)
  decisions <- sample(c("Passe", "Red", "Exclu"), size = nrow(data), replace = TRUE)
  
  # Écrire les décisions dans la colonne R (18), à partir de la ligne 2 (en dessous des en-têtes)
  writeData(wb, sheet = sheet_name, x = data.frame(Décision_finale = decisions),
            startCol = 18, startRow = 2, colNames = FALSE)
  
  # Sauvegarder le fichier
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

to_Entete("./Liste+Bilan contrat/EnteteJury.xlsx", "EnteteJury", non_valides)
remplir_decision_finale("./Liste+Bilan contrat/EnteteJury.xlsx")
