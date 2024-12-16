# Charger la bibliothèque openxlsx
library(openxlsx)

# Créer un nouveau classeur Excel
wb <- createWorkbook()
addWorksheet(wb, "S3")

# Définir les données pour les colonnes 'Nom' et 'Prénom'
noms <- paste0("Nom", 1:25)
prenoms <- paste0("Prenom", 1:25)
data <- data.frame(Nom = noms, Prénom = prenoms)

# Écrire les données initiales dans la feuille
writeData(wb, "S3", "Nom", startCol = 1, startRow = 3, colNames = FALSE)
writeData(wb, "S3", "Prénom", startCol = 2, startRow = 3, colNames = FALSE)
writeData(wb, "S3", data, startCol = 1, startRow = 4, colNames = FALSE)

# Définir les titres des colonnes et groupes
lesUE <- c("FONDA", "EXP", "ORT", "HUMA", "OPT")
lesEC <- list(
  FONDA = c("Bilan", "ALG", "ANA", "INFO", "MECA"),
  EXP = c("Bilan", "ACSA", "CHIM", "ELEC", "PHYS", "THEN"),
  ORT = c("Bilan", "ADS", "PPI", "RIE", "STAG", "TSE"),
  HUMA = c("Bilan", "ANGL", "COMM", "EPS", "LV2"),
  OPT = c("Bilan", "LV3", "PLV", "AHN")
)
lesEC_details <- list(
  FONDA = c("Résultat", "ECTS", "Note", "ALG", "ANA", "INFO", "MECA"),
  EXP = c("Résultat", "ECTS", "Note", "ACSA", "CHIM", "ELEC", "PHYS", "THEN"),
  ORT = c("Résultat", "ECTS", "Note", "ADS", "PPI", "RIE", "STAG", "TSE"),
  HUMA = c("Résultat", "ECTS", "Note", "ANGL", "COMM", "EPS", "LV2"),
  OPT = c("Résultat", "ECTS", "Note", "LV3", "PLV", "AHN")
)

# Variables de positionnement initial
startCol <- 3
rowTitles <- 1
rowEC <- 2
rowDetails <- 3

# Boucle pour ajouter les titres, les sous-titres et les détails
for (i in seq_along(lesUE)) {
  ue <- lesUE[i]
  ec <- lesEC[[ue]]
  details <- lesEC_details[[ue]]
  numCols <- length(details)
  
  # Écrire le titre de l'UE et fusionner les cellules correspondantes
  writeData(wb, "S3", ue, startCol = startCol, startRow = rowTitles, colNames = FALSE)
  mergeCells(wb, "S3", cols = startCol:(startCol + numCols - 1), rows = rowTitles)
  
  # Écrire les sous-titres EC
  for (j in seq_along(ec)) {
    if (j == 1) {
      writeData(wb, "S3", ec[j], startCol = startCol, startRow = rowEC, colNames = FALSE)
      mergeCells(wb, "S3", cols = startCol:(startCol + 2), rows = rowEC)
    } else {
      writeData(wb, "S3", ec[j], startCol = startCol + j + 1, startRow = rowEC, colNames = FALSE)
    }
  }
  
  # Écrire les détails
  for (k in seq_along(details)) {
    writeData(wb, "S3", details[k], startCol = startCol + k - 1, startRow = rowDetails, colNames = FALSE)
  }
  
  # Mise à jour de la position de la colonne de départ
  startCol <- startCol + numCols
}

# Appliquer des styles de remplissage et de bordure
styles <- list(
  FONDA = createStyle(fgFill = "#CCFFCC", border = "TopBottomLeftRight"),
  EXP = createStyle(fgFill = "#CCFFFF", border = "TopBottomLeftRight"),
  ORT = createStyle(fgFill = "#FFFFCC", border = "TopBottomLeftRight"),
  HUMA = createStyle(fgFill = "#E5D9F5", border = "TopBottomLeftRight"),
  OPT = createStyle(fgFill = "#FFCCCC", border = "TopBottomLeftRight")
)

# Appliquer les styles pour chaque section
startCol <- 3
for (ue in lesUE) {
  ec <- lesEC[[ue]]
  numCols <- length(ec)
  addStyle(wb, "S3", styles[[ue]], rows = 2:28, cols = startCol:(startCol + numCols - 1), gridExpand = TRUE)
  startCol <- startCol + numCols
}

# Appliquer les bordures pour toutes les cellules
border_style <- createStyle(border = "TopBottomLeftRight")
addStyle(wb, "S3", border_style, rows = 1:28, cols = 1:38, gridExpand = TRUE)

# Ajuster la largeur des colonnes
setColWidths(wb, "S3", cols = 1:38, widths = 10)

# Sauvegarder le fichier Excel
saveWorkbook(wb, "tableau_structure_optimise.xlsx", overwrite = TRUE)

print("Fichier Excel optimisé créé : tableau_structure_optimise.xlsx")
