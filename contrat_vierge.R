library(systemfonts)
library(textshaping)
library(ragg)
library(officer)
library(magrittr)
library(dplyr)
library(flextable)
library(readxl)

# création d'un nouvel objet de document Word
doc <- read_docx()

# création des tableaux de données
data <- data.frame(
  LesUE[1],
  LesUE[2],
  LesUE[3],
  Moyennes = c(rep("",length(LesUE[1]))),
  EcValRepasse = c(rep("",length(LesUE[1]))),
  EcAVal = c(rep("",length(LesUE[1])))
  )

signature <- data.frame(
  c1 = c("Date :\n\n\n\n\n"),
  c2 = c("Date :\n\n\n\n\n"),
  c3 = c("\n\n\n\n\n ")
)


#création des flextable
ft <- flextable(data)
sign <- flextable(signature)


#flextable avec la signature à la fin du document word
sign <- sign %>%
  bold(part = "header")%>%
  set_header_labels(
    c1 = "Signature de l'étudiant.e précédée de la mention «lu et approuvé»",
    c2 = "Signature de la Directrice du Département : Carole Daiguebonne",
    c3 = "Cachet de l'établissement"
  )%>%
  width(j = c(1,2,3), width = c(2.5,2.5,2.5))%>%
  border_outer(border = fp_border(color = "black", width = 1, style = "solid")) %>%
  border_inner_h(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
  border_inner_v(border = fp_border(color = "grey", width = 0.5, style = "solid"))


#flextable avec ue, ec, code EC, moyenne, à repasser, à valider
ft <- ft %>%
  bold(part = "header")%>%
  
  #pour changer les noms des colonnes
  set_header_labels(
    Ue = "UE (code UE)",
    Ec = "EC",
    CodeEC = "Code EC",
    Moyennes = "EC 2023-2024 (Moyenne obtenue)",
    EcValRepasse = "EC validé mais repassé en 2024-2025",
    EcAVal = "EC à valider en 2024-2025"
  )%>%
  
  #pour concaténer plusieurs lignes et avoir 1 case par UE
  merge_at(i = 1:length(lesEC_FONDAS3), j = 1) %>%
  merge_at(i = length(lesEC_FONDAS3)+1:length(lesEC_EXPS3), j = 1) %>%
  merge_at(i = 10:13, j = 1) %>%     #c'est très long d'écrire la longueur à chaque fois, comment faire?
  merge_at(i = 14:17, j = 1) %>%
  merge_at(i = 19:21, j = 1) %>%
  merge_at(i = 22:27, j = 1) %>%
  merge_at(i = 28:30, j = 1) %>%
  merge_at(i = 31:34, j = 1) %>%
  
  #pour choisir la largeur des colonnes
  width(j = c(1,2,3,4,5), width = c(1.5,1.7,1.5,1,1))%>%
  
  #pour mettre les bords en gras
  border_outer(border = fp_border(color = "black", width = 1, style = "solid"))%>%
  
  #pour mettre les lignes horizontales et verticales en gris pour pouvoir les visialiser sur document imprimé
  border_inner_h(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
  border_inner_v(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
  
  #pour mettre la ligne horizontale qui sépare le S3 du S4 en gras
  hline(i = nb_EC_S3, border = fp_border(width = 1.5, color = "black"))

#pour choisir la police, couleur, taille du titre
titre <- fpar(
  ftext("CONTRAT D'ÉTUDES 2 STPI", fp_text(font.size = 12, bold = TRUE, font.family = "Arial", color = "cornflowerblue")),
fp_p = fp_par(text.align = "center")
  )


#pour remplir le document Word avec du texte et les flextables
doc <- doc %>% 
  body_add_par("") %>%
  body_add_fpar(titre) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("Nom et prénom de l'étudiant·e :", style = "Normal") %>%
  body_add_par("Semestre : ☐ 3    ☐ 4 \t\t Année complète : ☐", style = "Normal") %>%
  body_add_par("Motif de l'établissement du présent contrat :", style = "Normal") %>%
  body_add_par("\t☐ SHN \t☐ Redoublement \t☐ Redoublement de Cas de Force Majeure", style = "Normal") %>%
  body_add_par("\t☐ Autre (à préciser) : Dossier médical ", style = "Normal")%>%
  body_add_par("\tDétails du contrat d'études :") %>%
  body_add_par("")%>%
  body_add_flextable(value = ft)%>%
  body_add_par("")%>%
  body_add_par("")%>%
  body_add_par("L’élève s’engage à respecter le présent contrat signé.", style="Normal")%>%
  body_add_par("")%>%
  body_add_flextable(value = sign)


#pour sauvegarder le document
print(doc, target = "contrat_vierge.docx")
