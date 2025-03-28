source("contrat_notes_fonctions.R")
source("RecupererNomPrenom.R")
library(readxl)


for (i in 1:length(names_vector)){
  doc <- read_docx()
  
  #génération du contrat
  generation(names_vector[i],surnames_vector[i],doc)
  
  name_of_contrat<-paste0("./ListeContrats+Bilan/contrat_notes_",names_vector[i],"_",surnames_vector[i],".docx")
  
  # pour sauvegarder le document
  print(doc, target = name_of_contrat)
  
}
