# install.packages("systemfonts")
# install.packages("textshaping")
#install.packages("ragg")
#install.packages("officer")
# install.packages("magrittr")
# install.packages("dplyr")
#install.packages("gdtools")
#install.packages("flextable")


# Charger les packages
library(systemfonts)
library(textshaping)
library(ragg)
library(officer)
library(magrittr)  # ou
library(dplyr)
library(flextable)


# Créer un nouvel objet de document Word
doc <- read_docx()

# Ajouter un titre
doc <- doc %>% 
  body_add_par("24 octobre 2024", style = "heading 1")

# Ajouter un paragraphe
doc <- doc %>% body_add_par("Paragraphe 1", style = "Normal") #s'ecrit OK
# saut de page : doc <- doc %>% body_add_break()
doc <- doc %>% body_add_par("", style = "Normal")     # Saut de ligne
doc <- doc %>% body_add_par("Paragraphe 2", style = "Normal")

# Créer un tableau de données
table_data <- data.frame(
  Colonne1 = c("A", "A", "B", "C"),
  Colonne2 = c(1, 2, 3, 4)
)

#créer la flextable
ft <- flextable(table_data)

ft <- ft %>% 
  merge_at(i = 1:2, j = 1) %>%  #fusionner 2 lignes d'une seul colonne
  bg(part = "header", bg = "lightgrey") %>%   #ajoute une couleur de fond
  color(part = "header", color = "orange") %>% #modif couleur du texte
  autofit() %>%   #largeur des colonnes en fonction du titre
  
  #bordures exterieures noires et continues
  border_outer(border = fp_border(color = "black", width = 2, style = "solid")) %>%
  #bordure interieures oranges en pointillés 
  border_inner(border = fp_border(color = "orange", width = 1, style = "dashed"))



doc <- doc %>% body_add_flextable(value = ft)

# Ajouter une image (optionnel)
# doc <- doc %>%
#   body_add_img(src = "chemin_de_ton_image.png", width = 5, height = 4)

# Sauvegarder le document
print(doc, target = "generation_word.docx")
