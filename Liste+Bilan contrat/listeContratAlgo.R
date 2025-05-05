source("contrat_notes.R")
source("RecupererNomPrenom.R")
source("filterRedoublement.R")
for (j in indice_redoublements_vector){
  doc <- read_docx()
  name_of_contrat<-paste0("./ListeContrats+Bilan/contrat_notes_",names_surnames_vector_unique[j],".docx")
  j <- as.character(j)
  #génération du contrat
  generation(j,doc)
  # pour sauvegarder le document
  print(doc, target = name_of_contrat)
}


