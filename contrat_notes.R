library(systemfonts)
library(textshaping)
library(ragg)
library(officer)
library(magrittr)
library(dplyr)
library(flextable)
library(readxl)


# j'ai ajoute cette ligne pour que le fichier MCC soit chargé, si vous avez
# decomposé votre code avec server, ui, global, il suffit de la mettre dans
# global.R


source("MCC_24-25.R")


# renvoie les notes de l'élève nom prenom du semestre associé à la feuille de l'excel fichier
notes_from_jury <- function(fichier, feuille, colonnes, nom, prenom){
  semestre <- read_excel(fichier, sheet = feuille, col_types = "text")
  notes <- semestre[semestre[[1]] == nom & semestre[[2]] == prenom, colonnes]
  return(unlist(notes[rowSums(is.na(notes)) != ncol(notes), ]))
}

#renvoie les validations ou non de chaque UE pour un semestre
valide_UE <- function(fichier, feuille, col_val, nom, prenom){
  semestre <- read_excel(fichier, sheet = feuille, col_types = "text")
  validations <- semestre[semestre[[1]] == nom & semestre[[2]] == prenom, col_val]
  return(unlist(validations[rowSums(is.na(validations)) != ncol(validations), ]))
}

##### comment c'est écrit quand l'EC est en validation?? majusucle, minuscule?
# renvoie un vecteur contenant des croix si l'ec n'est pas validé
croix <- function(notes,validation){
  croix <- c()
  i <- 1
  for (j in 1:length(validation)){
    if (validation[j] == "NON VALIDE"){
      while(i <= nb_EC_UE[j]){
        if (as.integer(notes[i]) < 10 || is.na(as.integer(notes[i]))){
          croix[i] <- "X"
        }else{
          croix[i] <- ""
        }
        i = i+1
      }
    }else{
      while(i <= nb_EC_UE[j]){
        croix[i] <- ""
        i = i+1
      }
    }
    }
  return(croix)
}


# création des tableaux de données
build_notes_etudiant <- function(notes, croix){
  return(
    data.frame(
      LesUE[1],
      LesUE[2],
      LesUE[3],
      Moyennes = notes,
      EcValRepasse = c(rep("",length(LesUE[1]))),
      EcAVal = croix
    ))
}

build_signature <- function(){
  return(
      data.frame(
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

# concaténation de lignes pour avoir 1 case par UE
merge_UE <- function(ft){
  ft <- ft %>% merge_at(i = 1:nb_EC_UE[1], j = 1)
  for (c in 1:(length(nb_EC_UE)-1)) {
    ft <- ft %>% merge_at(i = (nb_EC_UE[c]+1):nb_EC_UE[c+1], j = 1)
  }
  return(ft)
}



#flextable avec ue, ec, code EC, moyenne, à repasser, à valider
build_ft_notes <- function(ft){
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
      
      # choix de la largeur des colonnes
      width(j = c(1,2,3,4,5), width = c(1.5,1.7,1.5,1,1))%>%
      
      # pour mettre les bords en gras
      border_outer(border = fp_border(color = "black", width = 1, style = "solid"))%>%
      
      # pour mettre les lignes horizontales et verticales en gris pour pouvoir les visialiser sur document imprimé
      border_inner_h(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
      border_inner_v(border = fp_border(color = "grey", width = 0.5, style = "solid")) %>%
      
      # pour mettre la ligne horizontale qui sépare le S3 du S4 en gras
      hline(i = nb_EC_S3, border = fp_border(width = 1.5, color = "black"))
    
    # concaténation de lignes pour avoir 1 case par UE
    ft <- ft %>% merge_UE()
  return (ft)
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


### séparer génération dataframe et génération du doc
#génération du contrat
generation <- function(nom, prenom, doc){
  
  # récupération du fichier jury
  fichier_jury <- "/home/yaelle/Bureau/3A/EP_perso/jury.xlsx"
  
  # récupération des notes de l'étudiant·e
  notes_S3 <- notes_from_jury(fichier_jury, 2, col_S3, nom, prenom)
  notes_S4 <- notes_from_jury(fichier_jury, 3, col_S4, nom, prenom)
  notes <- c(notes_S3, notes_S4)
  
  #récupération des validations des UE de l'étudiant·e
  val_S3 <- valide_UE(fichier_jury, 2, col_val_S3, nom, prenom)
  val_S4 <- valide_UE(fichier_jury, 3, col_val_S4, nom, prenom)
  val <- c(val_S3, val_S4)
  
  #pour mettre des croix si l'EC n'est pas validé ou n'a pas de note
  croix <- croix(notes, val)

  #création des dataframes
  notes_etudiant <<- build_notes_etudiant(notes, croix)
  
  #return (notes_etudiant)
  
  signature <- build_signature()
  
  #création des flextable
  ft_notes <- flextable(notes_etudiant)
  ft_sign <- flextable(signature)
  
  ft_notes <- build_ft_notes(ft_notes)
  ft_sign <- build_ft_signature(ft_sign)
  
  doc <- ecriture(ft_notes, ft_sign)
}






# création d'un nouvel objet de document Word
doc <- read_docx()

#génération du contrat
generation("Nom1","Prenom1",doc)

# pour sauvegarder le document
print(doc, target = "./contrat_notes_29_mars.docx")
