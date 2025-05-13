source("./Code/generation_bilan/RecupererNomPrenom.R")
source("./Code/generation_bilan/filterRedoublement.R")
library(docxtractr)
library(dplyr)
library(openxlsx)

# Load the Word document

doc <- read_docx("./Fichiers/contrat_reference.docx")
# List all tables in the document
tables <- docx_extract_all_tbls(doc)

bilanS3<-tables[[1]][1:18,1:3]
bilanS4<-tables[[1]][19:34,1:3]

vector_croixS3<-c()
vector_croixS4<-c()
for (i in indice_redoublements_vector){
  name_of_contrat<-paste0("./Fichiers/ListeContrat/contrat_notes_",names_surnames_vector_unique[i],".docx")
  doc <- read_docx(name_of_contrat)
  tables <- docx_extract_all_tbls(doc)
  for (i in 1:18){
    if (as.character(tables[[1]][i,6])=="X"||as.character(tables[[1]][i,5])=="X"){
      vector_croixS3<-c(vector_croixS3,"X")
    }else{
      vector_croixS3<-c(vector_croixS3,"")
    }
  }
  for (i in 19:34){
    if (as.character(tables[[1]][i,6])=="X"||as.character(tables[[1]][i,5])=="X"){
      vector_croixS4<-c(vector_croixS4,"X")
    }else{
      vector_croixS4<-c(vector_croixS4,"")
    }
  }
  bilanS3 <- cbind(bilanS3,vector_croixS3)
  bilanS4 <- cbind(bilanS4,vector_croixS4)
  vector_croixS3<-c()
  vector_croixS4<-c()
  
}
# Ensure unique column names before renaming
colnames(bilanS3) <- make.names(colnames(bilanS3), unique = TRUE)
# Get columns to rename
cols_to_rename <- grep("^vector_croixS3", colnames(bilanS3), value = TRUE)
# Rename them using the vector of new names
colnames(bilanS3)[match(cols_to_rename, colnames(bilanS3))] <- names_surnames_vector_unique[indice_redoublements_vector]

# Ensure unique column names before renaming
colnames(bilanS4) <- make.names(colnames(bilanS4), unique = TRUE)
# Get columns to rename
cols_to_rename <- grep("^vector_croixS4", colnames(bilanS4), value = TRUE)
# Rename them using the vector of new names
colnames(bilanS4)[match(cols_to_rename, colnames(bilanS4))] <- names_surnames_vector_unique[indice_redoublements_vector]

bilan<-rbind(bilanS3,bilanS4)
write.xlsx(bilanS3, file = "./Fichiers/Bilans/bilanContratsS3.xlsx", rowNames = FALSE)
write.xlsx(bilanS4, file = "./Fichiers/Bilans/bilanContratsS4.xlsx", rowNames = FALSE)
write.xlsx(bilan, file = "./Fichiers/Bilans/bilanContrats.xlsx", rowNames = FALSE)



