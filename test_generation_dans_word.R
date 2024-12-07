library(officer)
library(flextable)
library(magrittr)

data <- data.frame(
  Nom = c("Bob","Clément","Alice"),
  Age = c(45,7,7),
  Sexe= c("Homme","Homme","Femme")
)

tableau<-flextable(data)
tableau<-set_table_properties(tableau,width = 0.75,layout="autofit")
tableau<-color(tableau,j="Age",color="gold")
tableau<-align(x=tableau,align="center",j=c("Nom","Age","Sexe"),part="all")
tableau<-border_inner(x=tableau,border=fp_border(color="green",width=1),part="all")
tableau<-border_outer(x=tableau,border=fp_border(color="green",width=3),part="all")
tableau<-merge_at(tableau,i=1:2,j=3)

tableau
doc <- read_docx()

doc <- body_add_par(doc,value = "Voici un magnifique tableau:")
doc <- body_add_flextable(doc,value=tableau)

  
print(doc, target="/home/marion-vallin-brams/R/Etude Pratique/tableau.docx")
