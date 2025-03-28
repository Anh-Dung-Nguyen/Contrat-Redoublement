library(systemfonts)
library(textshaping)
library(ragg)
library(officer)
library(magrittr)
library(dplyr)
library(flextable)
library(readxl)
library(cli)



# j'ai ajoute cette ligne pour que le fichier MCC soit chargé, si vous avez
# decomposé votre code avec server, ui, global, il suffit de la mettre dans
# global.R


source("MCC1.R")


# renvoie les notes de l'élève nom prenom du semestre associé à la feuille de l'excel fichier
# XXXX ne faudrait il pas aussi recuperer les validations des différentes UE ?
notes_from_jury <- function(fichier, feuille, colonnes, nom, prenom){
  semestre <- read_excel(fichier, sheet = feuille, col_types = "text")
  notes <- semestre[semestre[[1]] == nom & semestre[[2]] == prenom, colonnes]
  return(unlist(notes[rowSums(is.na(notes)) != ncol(notes), ]))
}

# renvoie un vecteur contenant des croix si l'ec n'est pas validé
# XXXX
# attentin j'ai l'impression que vous n'avez pas traité les matieres qui 
# sont sans note mais uniquement avec validation
# XXXX


croix <- function(notes){
  croix <- c()
  i <- 1
  for (note in notes){
    if (as.integer(note) < 10 || is.na(as.integer(note))){ 
      #il faut aussi gérer si c'est en validation ms jsp comment c'est écrit dans le tableur
      # XXXXX
      # c'est simple dans ta feuille, il y a une premiere colonne appelee "Résultat"
      # si la valeur dans cette colonne est VALIDE ou "VALID COMP" alors toutes les 
      # EC de l'UE sont validées sinon il faut regarder la ntoe de chaque EC
      # XXXXXX
      croix[i] <- "X"
    }else{
      croix[i] <- ""
    }
    i = i+1
  }
  return(croix)
}

# JE VOIS PAS OU C'EST, LesUE[1] corresponds à la colonne contenant toutes les UES, 
#LesUE[2] contient les EC, LesUE[3] contient les codes des EC

# attention ici tu as end ure le fait d'avoir 3 UE
# il faut que ceci s'adapte automtaquement en fonction du fichier MCC
# d'ailleurs pour le MCC de cette année, il y a 4 UE
# solution on peut facilement ajouter des colonnes a un dataframe dans un for
# cf dernier tp de proba

# XXXX en faire une petite fonction : build_notes_etudiant
# création des tableaux de données
build_notes_etudiant <- function(notes_S3, notes_S4, croix_S3, croix_S4){
  return(
    data.frame(
      LesUE[1],
      LesUE[2],
      LesUE[3],
      Moyennes = c(notes_S3,notes_S4),
      EcValRepasse = c(rep("",length(LesUE[1]))),
      EcAVal = c(croix_S3, croix_S4)
    ))
}

# ideme : petite fonction build_signature
build_signature <- function(){
  return(
    signature <- data.frame(
      c1 = c("Date :\n\n\n\n\n"),
      c2 = c("Date :\n\n\n\n\n"),
      c3 = c("\n\n\n\n\n ")
    )
  )
}


#flextable avec la signature à la fin du document word
build_ft_signature <- function(sign){
  return(
    sign %>%
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
  )
}


#flextable avec ue, ec, code EC, moyenne, à repasser, à valider
build_ft_notes <- function(ft){
  return(
    ft %>%
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
      
      # XXXXX
      # ok nb_EC_UE defini dans MCC
      # par contre meme chose le 7 doit etre déduit des constantes du MCC et il 
      # faudrait une boucle pour que ton code soit adaptable a tout changement
      # du fichier MCC
      # XXXX
      # concaténation de lignes pour avoir 1 case par UE
      merge_at(i = 1:nb_EC_UE[1], j = 1) %>%
      merge_at(i = (nb_EC_UE[1]+1):nb_EC_UE[2], j = 1) %>%
      merge_at(i = (nb_EC_UE[2]+1):nb_EC_UE[3], j = 1) %>%
      merge_at(i = (nb_EC_UE[3]+1):nb_EC_UE[4], j = 1) %>%
      merge_at(i = (nb_EC_UE[4]+1):nb_EC_UE[5], j = 1) %>%
      merge_at(i = (nb_EC_UE[5]+1):nb_EC_UE[6], j = 1) %>%
      merge_at(i = (nb_EC_UE[6]+1):nb_EC_UE[7], j = 1) %>%
      merge_at(i = (nb_EC_UE[7]+1):nb_EC_UE[8], j = 1) %>%
      merge_at(i = (nb_EC_UE[7]+1):nb_EC_UE[8], j = 1) %>%
      merge_at(i = (nb_EC_UE[8]+1):nb_EC_UE[9], j = 1) %>%
      
      # choix de la largeur des colonnes
      width(j = c(1,2,3,4,5), width = c(1.5,1.7,1.5,1,1))%>%
      
      # pour mettre les bords en gras
      border_outer(border = fp_border(color = "black", width = 1, style = "solid"))%>%
      
      # pour mettre les lignes horizontales et verticales en gris pour pouvoir les visialiser sur document imprimé
      border_inner_h(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
      border_inner_v(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
      
      # pour mettre la ligne horizontale qui sépare le S3 du S4 en gras
      hline(i = nb_EC_S3, border = fp_border(width = 1.5, color = "black"))
  )
}


# choix de la police, couleur, taille du titre
titre <- function(){
  return(
    fpar(
      ftext("CONTRAT D'ÉTUDES 2 STPI", fp_text(font.size = 12, bold = TRUE, font.family = "Arial", color = "cornflowerblue")),
      fp_p = fp_par(text.align = "center")
    )
  )
}


# pour remplir le document Word avec du texte et les flextables
ecriture <- function(ft_notes, ft_sign){
  return(
    doc %>% 
    body_add_par("") %>%
    body_add_fpar(titre()) %>%
    body_add_par("") %>%
    body_add_par("Nom et prénom de l'étudiant·e :", style = "Normal") %>%
    body_add_par("Semestre : ☐ 3    ☐ 4 \t\t Année complète : ☐", style = "Normal") %>%
    body_add_par("Motif de l'établissement du présent contrat :", style = "Normal") %>%
    body_add_par("\t☐ SHN \t☐ Redoublement \t☐ Redoublement de Cas de Force Majeure", style = "Normal") %>%
    body_add_par("\t☐ Autre (à préciser) : Dossier médical ", style = "Normal")%>%
    body_add_par("\tDétails du contrat d'études :") %>%
    body_add_par("")%>%
    body_add_flextable(value = ft_notes)%>%
    body_add_par("")%>%
    body_add_par("")%>%
    body_add_par("L’élève s’engage à respecter le présent contrat signé.", style="Normal")%>%
    body_add_par("")%>%
    body_add_flextable(value = ft_sign)
  )
}



#génération du contrat
generation <- function(nom, prenom, doc){
  
  # récupération du fichier jury
  fichier_jury <- "juryAD.xlsx"
  
  # récupération des notes de l'étudiant·e
  notes_S3 <- notes_from_jury(fichier_jury, 2, col_S3, nom, prenom)
  notes_S4 <- notes_from_jury(fichier_jury, 3, col_S4, nom, prenom)
  
  #pour mettre des croix si l'EC n'est pas validé ou n'a pas de note
  croix_S3 <- croix(notes_S3)
  croix_S4 <- croix(notes_S4)
  
  #création des dataframes
  notes_etudiant <- build_notes_etudiant(notes_S3, notes_S4, croix_S3, croix_S4)
  signature <- build_signature()
  
  #création des flextable
  ft_notes <- flextable(notes_etudiant)
  ft_sign <- flextable(signature)
  
  ft_notes <- build_ft_notes(ft_notes)
  ft_sign <- build_ft_signature(ft_sign)
  
  doc <- ecriture(ft_notes, ft_sign)
}






