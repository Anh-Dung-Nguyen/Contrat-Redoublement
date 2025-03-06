library(systemfonts)
library(textshaping)
library(ragg)
library(officer)
library(magrittr)
library(dplyr)
library(flextable)
library(readxl)

# renvoie les notes de l'élève nom prenom du semestre associé à la feuille de l'excel fichier
notes_from_jury <- function(fichier, feuille, colonnes, nom, prenom){
  semestre <- read_excel(fichier, sheet = feuille, col_types = "text")
  notes <- semestre[semestre[[1]] == nom & semestre[[2]] == prenom, colonnes]
  return(unlist(notes[rowSums(is.na(notes)) != ncol(notes), ]))
}

# renvoie un vecteur contenant des croix si l'ec n'est pas validé
croix <- function(notes){
  croix <- c()
  i <- 1
  for (note in notes){
    if (as.integer(note) < 10 || is.na(as.integer(note))){ #il faut aussi gérer si c'est en validation ms jsp comment c'est écrit dans le tableur
      croix[i] <- "X"
    }else{
      croix[i] <- ""
    }
    i = i+1
  }
  return(croix)
}


# création d'un nouvel objet de document Word
doc <- read_docx()

# récupération du fichier jury
fichier_jury <- "~/Bureau/3A/EP_perso/jury.xlsx"

# récupération des notes de l'étudiant·e
notes_S3 <- notes_from_jury(fichier_jury, 2, col_S3, "Nom1", "Prenom1")
notes_S4 <- notes_from_jury(fichier_jury, 3, col_S4, "Nom1", "Prenom1")

#pour mettre des croix si l'EC n'est pas validé ou n'a pas de note
croix_S3 <- croix(notes_S3)
croix_S4 <- croix(notes_S4)

# création des tableaux de données
data <- data.frame(
  LesUE[1],
  LesUE[2],
  LesUE[3],
  Moyennes = c(notes_S3,notes_S4),
  EcValRepasse = c(rep("",length(LesUE[1]))),
  EcAVal = c(croix_S3, croix_S4)
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
    c1 = "Signature de l'étudiant·e précédée de la mention «lu et approuvé»",
    c2 = direction_departement,
    c3 = "Cachet de l'établissement"
  )%>%
  width(j = c(1,2,3), width = c(2.5,2.5,2.5))%>%
  border_outer(border = fp_border(color = "black", width = 1, style = "solid")) %>%
  border_inner_h(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
  border_inner_v(border = fp_border(color = "grey", width = 0.5, style = "solid"))


#flextable avec ue, ec, code EC, moyenne, à repasser, à valider
ft <- ft %>%
  bold(part = "header")%>%
  
  #p changement des noms des colonnes
  set_header_labels(
    Ue = "UE (code UE)",
    Ec = "EC",
    CodeEC = "Code EC",
    Moyennes = "EC 2023-2024 (Moyenne obtenue)",
    EcValRepasse = "EC validé mais repassé en 2024-2025",
    EcAVal = "EC à valider en 2024-2025"
  )%>%
  
  # concaténation de lignes pour avoir 1 case par UE
  merge_at(i = 1:nb_EC_UE_S3[1], j = 1) %>%
  merge_at(i = nb_EC_UE_S3[1]+1:nb_EC_UE_S3[1], j = 1) %>%
  # je comprends pas : 
# > nb_EC_UE_S3[1]+1:nb_EC_UE_S3[2]
#   [1]  6  7  8  9 10 11 12 13 14
# > nb_EC_UE_S3[1]+1:nb_EC_UE_S3[1]
#   [1]  6  7  8  9 10
  
  #merge_at(i = nb_EC_UE_S3[2]+1:nb_EC_UE_S3[3], j = 1) %>%
  # merge_at(i = nb_EC_UE_S3[3]+1:nb_EC_UE_S3[4], j = 1) %>%
  # 
  # merge_at(i = nb_EC_UE_S3[5]+1:nb_EC_UE_S4[1], j = 1) %>%
  # merge_at(i = nb_EC_UE_S4[1]+1:nb_EC_UE_S4[2], j = 1) %>%
  # merge_at(i = nb_EC_UE_S4[2]+1:nb_EC_UE_S4[3], j = 1) %>%
  # merge_at(i = nb_EC_UE_S4[3]+1:nb_EC_UE_S4[4], j = 1) %>%

  # choix de la largeur des colonnes
  width(j = c(1,2,3,4,5), width = c(1.5,1.7,1.5,1,1))%>%
  
  # pour mettre les bords en gras
  border_outer(border = fp_border(color = "black", width = 1, style = "solid"))%>%
  
  # pour mettre les lignes horizontales et verticales en gris pour pouvoir les visialiser sur document imprimé
  border_inner_h(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
  border_inner_v(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
  
  # pour mettre la ligne horizontale qui sépare le S3 du S4 en gras
  hline(i = nb_EC_S3, border = fp_border(width = 1.5, color = "black"))
  

# choix de la police, couleur, taille du titre
titre <- fpar(
  ftext("CONTRAT D'ÉTUDES 2 STPI", fp_text(font.size = 12, bold = TRUE, font.family = "Arial", color = "cornflowerblue")),
fp_p = fp_par(text.align = "center")
  )


# pour remplir le document Word avec du texte et les flextables
doc <- doc %>% 
  body_add_par("") %>%
  body_add_fpar(titre) %>%
  body_add_par("") %>%
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


# pour sauvegarder le document
print(doc, target = "~/Bureau/3A/EP_perso/contrat_notes_6_mars.docx")
