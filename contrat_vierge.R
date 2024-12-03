library(systemfonts)
library(textshaping)
library(ragg)
library(officer)
library(magrittr)
library(dplyr)
library(flextable)

# Créer un nouvel objet de document Word
doc <- read_docx()

#créer un format de titre vert
#titre_vert <- fp_text(color = "green", bold = TRUE)

# Ajouter un titre
doc <- doc %>% 
  body_add_par("CONTRAT D'ETUDES 2 STPI", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Nom et prénom de l'étudiant·e :", style = "Normal") %>%
  body_add_par("Semestre : ☐ 3    ☐ 4 \t Année complète : ☐", style = "Normal") %>%
  body_add_par("Motif de l'établissement du présent contrat :", style = "Normal") %>%
  body_add_par("\t☐ SHN \t☐ Redoublement \t☐ Redoublement de Cas de Force Majeure", style = "Normal") %>%
  body_add_par("\t☐ Autre (à préciser) : Dossier médical ", style = "Normal")%>%
  body_add_par("\tDétails du contrat d'études :") %>%
  body_add_par("")

# Créer un tableau de données
data <- data.frame(
  Ue = c(rep(lesUE[1],length(lesEC_FONDAS3)), rep(lesUE[2],length(lesEC_EXPS3)), rep(lesUE[3],length(lesEC_ORTS3)), rep(lesUE[4],length(lesEC_HUMA)), rep(lesUE[5],length(lesEC_OPTS3))),
  Ec = c(lesEC_FONDAS3, lesEC_EXPS3, lesEC_ORTS3, lesEC_HUMA, lesEC_OPTS3),
  CodeEC = c(rep("?",26)),
  EcAnnee = c(rep("",26)),
  EcValRepasse = c(rep("",26)),
  EcAVal = c(rep("",26))
  )


#création flextable
ft <- flextable(data)

#renommer les colonnes
ft <- ft %>%
  bold(part = "header")%>%
  autofit()%>%
  
  set_header_labels(
    Ue = "UE (code UE)",
    Ec = "EC",
    CodeEC = "Code EC",
    EcAnnee = "EC 2023-2024 (Moyenne obtenue)",
    EcValRepasse = "EC validé mais repassé en 2024-2025",
    EcAVal = "EC à valider en 2024-2025"
  )%>%
  
  merge_at(i = 1:length(lesEC_FONDAS3), j = 1) %>%
  merge_at(i = length(lesEC_FONDAS3)+1:length(lesEC_EXPS3), j = 1) %>%
  merge_at(i = 12:17, j = 1) %>%     #c'est très long d'écrire la longueur à chaque fois, comment faire?
  merge_at(i = 18:22, j = 1) %>%
  merge_at(i = 23:26, j = 1) %>%
  
  border_outer(border = fp_border(color = "black", width = 2, style = "solid"))
  
  

doc <- doc %>% body_add_flextable(value = ft)

# Sauvegarder le document
print(doc, target = "gene_contrat_vierge.docx")
