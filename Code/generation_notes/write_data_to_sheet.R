write_data_to_sheet <- function(file_path, sheet_name, data_to_write) {
  # Charger la librairie et le fichier Excel
  if (!require("openxlsx")) install.packages("openxlsx")
  library(openxlsx)
  
  wb <- loadWorkbook(file_path) # Charger le fichier Excel
  
  # Lire les données existantes (facultatif, ici pour conserver la structure)
  existing_data <- read.xlsx(wb, sheet = sheet_name)
  
  # Écrire les nouvelles données dans la feuille spécifiée
  for (i in 1:nrow(data_to_write)) {
    writeData(wb, sheet = sheet_name, x = data_to_write[i, ], startRow = i + 3, startCol = 0, colNames = FALSE, rowNames = FALSE)
  }
  
  # Sauvegarder le fichier modifié
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

write_data_to_sheet("/home/nguyen-anh-dung/fichier_vide.xlsx", "S3", resumes_s3)
write_data_to_sheet("/home/nguyen-anh-dung/fichier_vide.xlsx", "S4", resumes_s4)