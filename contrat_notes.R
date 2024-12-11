library(systemfonts)
library(textshaping)
library(ragg)
library(officer)
library(magrittr)
library(dplyr)
library(flextable)
library(readxl)

print(format(Sys.Date(), "%Y"))

# création d'un nouvel objet de document Word
doc <- read_docx()


#récupération du fichier de jury avec les moyennes de tous les élèves pour chaque EC
fichier_jury <- "~/Bureau/3A/EP/jury.xlsx"

#pour lire les données de la 2ème feuille de fichier exel
toutes_notes_S3 <- read_excel(fichier_jury, sheet = 2)

#pour récupérer la ligne correspondant à l'élève dont on veut créer le contrat
notes_S3 <- toutes_notes_S3[toutes_notes_S3[[1]] == "Nom1" & toutes_notes_S3[[2]] == "Prenom1", c(6:10, 14:17, 21:25, 29:32)]

#pour extraire les valeurs et les filtrer pour ne garder que les non-NA
valeurs_notes_S3 <- unlist(notes_S3)
val_notes_S3_sans_NA <- valeurs_notes_S3[!is.na(valeurs_notes_S3)]


# création des tableaux de données
data <- data.frame(
  Ue = c(rep(lesUE[1],length(lesEC_FONDAS3)), rep(lesUE[2],length(lesEC_EXPS3)), rep(lesUE[3],length(lesEC_ORTS3)), rep(lesUE[4],length(lesEC_HUMA_S3)), lesUE[5],rep(lesUE[1],length(lesEC_FONDAS4)), rep(lesUE[2],length(lesEC_EXPS4)), rep(lesUE[3],length(lesEC_ORTS4)), rep(lesUE[4],length(lesEC_HUMA_S4))),
  Ec = c(lesEC_FONDAS3, lesEC_EXPS3, lesEC_ORTS3, lesEC_HUMA_S3, lesEC_Stage, lesEC_FONDAS4, lesEC_EXPS4, lesEC_ORTS4, lesEC_HUMA_S4),
  CodeEC = c(lesCodes_FONDAS3, lesCodes_EXPS3, lesCodes_ORTS3, lesCodes_HUMAS3, lesCodes_Stage, lesCodes_FONDAS4, lesCodes_EXPS4, lesCodes_ORTS4, lesCodes_HUMAS4),
  Moyennes = c(val_notes_S3_sans_NA,rep("",16)),
  EcValRepasse = c(rep("",34)),
  EcAVal = c(rep("",34))
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
    c2 = direction_departement,
    c3 = "Cachet de l'établissement"
  )%>%
  width(j = c(1,2,3), width = c(2.5,2.5,2.5))%>%
  border_outer(border = fp_border(color = "black", width = 1, style = "solid"))


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
print(doc, target = "gene_contrat_vierge.docx")
