# Charger la bibliothèque openxlsx
library(openxlsx)

# Créer un nouveau classeur Excel
wb <- createWorkbook()

# Ajouter une feuille
addWorksheet(wb, "S3")

# Définir les données pour les colonnes 'Nom' et 'Prénom'
noms <- paste0("Nom", 1:25)
prenoms <- paste0("Prenom", 1:25)
data <- data.frame(Nom = noms, Prénom = prenoms)

# Écrire les données initiales dans la feuille
writeData(wb, "S3", "Nom", startCol = 1, startRow = 3, colNames = FALSE)
writeData(wb, "S3", "Prénom", startCol = 2, startRow = 3, colNames = FALSE)
writeData(wb, "S3", data, startCol = 1, startRow = 4, colNames = FALSE)

# Définir les titres des colonnes et groupes (texte)
lesUE<-c("FONDA","EXP","ORT","HUMA","OPT")

lesEC_FONDAS3 <- c("Bilan","ALG","ANA","INFO","MECA")
lesEC_EXPS3 <- c("Bilan","ACSA","CHIM","ELEC","PHYS","THEN")
lesEC_ORTS3 <- c("Bilan", "ADS","PPI","RIE","STAG","TSE")
lesEC_HUMA_TC <- c("Bilan","ANGL","COMM","EPS","LV2")
lesEC_OPTS3 <- c("Bilan","LV3","PLV","AHN")

lesEC <- c(lesEC_FONDAS3, lesEC_EXPS3, lesEC_ORTS3, lesEC_HUMA_TC, lesEC_OPTS3)

lesEC_FONDAS3_1 <- c("Résultat","ECTS","Note","ALG","ANA","INFO","MECA")
lesEC_EXPS3_1 <- c("Résultat","ECTS","Note","ACSA","CHIM","ELEC","PHYS","THEN")
lesEC_ORTS3_1 <- c("Résultat","ECTS","Note", "ADS","PPI","RIE","STAG","TSE")
lesEC_HUMA_TC_1 <- c("Résultat","ECTS","Note","ANGL","COMM","EPS","LV2")
lesEC_OPTS3_1 <- c("Résultat","ECTS","Note","LV3","PLV","AHN")

lesEC_1 <- c(lesEC_FONDAS3_1, lesEC_EXPS3_1, lesEC_ORTS3_1, lesEC_HUMA_TC_1, lesEC_OPTS3_1)

# Écrire les titres des groupes principaux
writeData(wb, "S3", lesUE[1], startCol = 3, startRow = 1, colNames = FALSE)
mergeCells(wb, "S3", cols = 3:9, rows = 1)
writeData(wb, "S3", lesUE[2], startCol = 10, startRow = 1, colNames = FALSE)
mergeCells(wb, "S3", cols = 10:17, rows = 1)
writeData(wb, "S3", lesUE[3], startCol = 18, startRow = 1, colNames = FALSE)
mergeCells(wb, "S3", cols = 18:25, rows = 1)
writeData(wb, "S3", lesUE[4], startCol = 26, startRow = 1, colNames = FALSE)
mergeCells(wb, "S3", cols = 26:32, rows = 1)
writeData(wb, "S3", lesUE[5], startCol = 33, startRow = 1, colNames = FALSE)
mergeCells(wb, "S3", cols = 33:38, rows = 1)

writeData(wb, "S3", lesEC_FONDAS3[1], startCol = 3, startRow = 2, colNames = FALSE)
mergeCells(wb, "S3", cols = 3:5, rows = 2)
writeData(wb, "S3", lesEC_FONDAS3[2], startCol = 6, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3[3], startCol = 7, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3[4], startCol = 8, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3[5], startCol = 9, startRow = 2, colNames = FALSE)

writeData(wb, "S3", lesEC_EXPS3[1], startCol = 10, startRow = 2, colNames = FALSE)
mergeCells(wb, "S3", cols = 10:12, rows = 2)
writeData(wb, "S3", lesEC_EXPS3[2], startCol = 13, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3[3], startCol = 14, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3[4], startCol = 15, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3[5], startCol = 16, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3[6], startCol = 17, startRow = 2, colNames = FALSE)

writeData(wb, "S3", lesEC_ORTS3[1], startCol = 18, startRow = 2, colNames = FALSE)
mergeCells(wb, "S3", cols = 18:20, rows = 2)
writeData(wb, "S3", lesEC_ORTS3[2], startCol = 21, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3[3], startCol = 22, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3[4], startCol = 23, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3[5], startCol = 24, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3[6], startCol = 25, startRow = 2, colNames = FALSE)

writeData(wb, "S3", lesEC_HUMA_TC[1], startCol = 26, startRow = 2, colNames = FALSE)
mergeCells(wb, "S3", cols = 26:28, rows = 2)
writeData(wb, "S3", lesEC_HUMA_TC[2], startCol = 29, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC[3], startCol = 30, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC[4], startCol = 31, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC[5], startCol = 32, startRow = 2, colNames = FALSE)

writeData(wb, "S3", lesEC_OPTS3[1], startCol = 33, startRow = 2, colNames = FALSE)
mergeCells(wb, "S3", cols = 33:35, rows = 2)
writeData(wb, "S3", lesEC_OPTS3[2], startCol = 36, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_OPTS3[3], startCol = 37, startRow = 2, colNames = FALSE)
writeData(wb, "S3", lesEC_OPTS3[4], startCol = 38, startRow = 2, colNames = FALSE)

writeData(wb, "S3", lesEC_FONDAS3_1[1], startCol = 3, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3_1[2], startCol = 4, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3_1[3], startCol = 5, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3_1[4], startCol = 6, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3_1[5], startCol = 7, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3_1[6], startCol = 8, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_FONDAS3_1[7], startCol = 9, startRow = 3, colNames = FALSE)

writeData(wb, "S3", lesEC_EXPS3_1[1], startCol = 10, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3_1[2], startCol = 11, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3_1[3], startCol = 12, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3_1[4], startCol = 13, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3_1[5], startCol = 14, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3_1[6], startCol = 15, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3_1[7], startCol = 16, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_EXPS3_1[8], startCol = 17, startRow = 3, colNames = FALSE)

writeData(wb, "S3", lesEC_ORTS3_1[1], startCol = 18, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3_1[2], startCol = 19, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3_1[3], startCol = 20, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3_1[4], startCol = 21, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3_1[5], startCol = 22, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3_1[6], startCol = 23, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3_1[7], startCol = 24, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_ORTS3_1[8], startCol = 25, startRow = 3, colNames = FALSE)

writeData(wb, "S3", lesEC_HUMA_TC_1[1], startCol = 26, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC_1[2], startCol = 27, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC_1[3], startCol = 28, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC_1[4], startCol = 29, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC_1[5], startCol = 30, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC_1[6], startCol = 31, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_HUMA_TC_1[7], startCol = 32, startRow = 3, colNames = FALSE)

writeData(wb, "S3", lesEC_OPTS3_1[1], startCol = 33, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_OPTS3_1[2], startCol = 34, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_OPTS3_1[3], startCol = 35, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_OPTS3_1[4], startCol = 36, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_OPTS3_1[5], startCol = 37, startRow = 3, colNames = FALSE)
writeData(wb, "S3", lesEC_OPTS3_1[6], startCol = 38, startRow = 3, colNames = FALSE)

# Appliquer les couleurs pour chaque section
green_fill <- createStyle(fgFill = "#CCFFCC", border = "TopBottomLeftRight")
blue_fill <- createStyle(fgFill = "#CCFFFF", border = "TopBottomLeftRight")
yellow_fill <- createStyle(fgFill = "#FFFFCC", border = "TopBottomLeftRight")
purple_fill <- createStyle(fgFill = "#E5D9F5", border = "TopBottomLeftRight")

# Appliquer les couleurs aux groupes
addStyle(wb, "S3", green_fill, rows = 2:28, cols = 3:10, gridExpand = TRUE)
addStyle(wb, "S3", blue_fill, rows = 2:28, cols = 11:17, gridExpand = TRUE)
addStyle(wb, "S3", yellow_fill, rows = 2:28, cols = 18:25, gridExpand = TRUE)
addStyle(wb, "S3", purple_fill, rows = 2:28, cols = 26:31, gridExpand = TRUE)

# Appliquer bordures pour toutes les cellules
border_style <- createStyle(border = "TopBottomLeftRight")
addStyle(wb, "S3", border_style, rows = 1:28, cols = 1:38, gridExpand = TRUE)

# Ajuster la largeur des colonnes
setColWidths(wb, "S3", cols = 1:38, widths = 10)

# Sauvegarder le fichier Excel
saveWorkbook(wb, "tableau_structure.xlsx", overwrite = TRUE)

print("Fichier Excel créé : tableau_structure.xlsx")
