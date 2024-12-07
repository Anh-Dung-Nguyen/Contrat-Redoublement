#Code de Huy

lesUE<-c("FONDA","EXP","ORT","HUMA","OPT")

tableau_lesUE<-data.frame(ListDeUE=lesUE)

# -----------------   definition des constantes pour l'UE FONDA ----------------


#  MCC : Partie devant être définie à la main
lesEC_FONDAS3<-c("ALG","ANA","INFO","MECA")
lesTypeEC_FONDAS3<-c("Note","Note","Note","Note")
lesNotesEC_FONDAS3 <- list(
  list("DS1", "DS2",NA),
  list("DS1", "DS2",NA),
  list("DS1", "DS2",NA),
  list("DS1", "DS2", "CC")
)

dataframenotesECFONDAS3 <- do.call(rbind, lapply(lesNotesEC_FONDAS3, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECFONDAS3) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

lesECTSFONDAS3<-c(2.0,3.0,2.0,2.0) # alg, info, ana, meca

tableauNomUE_Fonda3<-data.frame(UE=rep("FondaS3",4))
tableauEC_Fonda31<-data.frame(EC=lesEC_FONDAS3,TypeEC=lesTypeEC_FONDAS3)
tableauEC_Fonda32<-data.frame(ECTS=lesECTSFONDAS3)
tableauEC_Fonda3<-cbind(tableauNomUE_Fonda3,tableauEC_Fonda31,dataframenotesECFONDAS3,tableauEC_Fonda32)

lesEC_FONDAS4<-c("Geometrie","INFO","PROBA")
lesTypeEC_FONDAS4<-c("Note","Note","Note")


lesNotesEC_FONDAS4 <- list(
  list("DS1", "DS2",NA),
  list("DS", "CC",NA),
  list("DS1", "DS2",NA)
)

dataframenotesECFONDAS4 <- do.call(rbind, lapply(lesNotesEC_FONDAS4, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECFONDAS4) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

lesECTSFONDAS4<-c(3.5,3,3.5) # géometrie, info, proba


tableauNomUE_Fonda4<-data.frame(UE=rep("FondaS4",3))
tableauEC_Fonda41<-data.frame(EC=lesEC_FONDAS4,TypeEC=lesTypeEC_FONDAS4)
tableauEC_Fonda42<-data.frame(ECTS=lesECTSFONDAS4)
tableauEC_Fonda4<-cbind(tableauNomUE_Fonda4,tableauEC_Fonda41,dataframenotesECFONDAS4,tableauEC_Fonda42)
# -----------------   defintion des constantes pour l'UE EXP ----------------


lesECTSEXPS3<-c(3,3.5,1.5,1.5,1.5) # acsa, chimie, electro, TP Phys, thermo

lesECTSEXPS4<-c(3.5,2,2,2,1.5,1) # chimie, electro, meca, ondes, TP Phys, SI


lesEC_EXPS3<-c("ACSA","Chimie","Electro","TP Phys","Thermo")
lesTypeEC_EXPS3<-c("Note","Note","Note","Note","Note")
lesNotesEC_EXPS3 <- list(
  
  list("DS", "TP",NA),         # ACSA
  list("DS", "DS","TP"),    # Chimie
  list("DS", "CC",NA),         # Electro
  list("TP",NA,NA),               # TP Phys
  list("DS",NA,NA)                # Thermo
)

dataframenotesECTSEXPS3 <- do.call(rbind, lapply(lesNotesEC_EXPS3, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSEXPS3) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

tableauNomUE_EXP3<-data.frame(UE=rep("EXPS3",5))
tableauEC_EXP31<-data.frame(EC=lesEC_EXPS3,TypeEC=lesTypeEC_EXPS3)
tableauEC_EXP32<-data.frame(ECTS=lesECTSEXPS3)
tableauEC_EXP3<-cbind(tableauNomUE_EXP3,tableauEC_EXP31,dataframenotesECTSEXPS3,tableauEC_EXP32)


lesEC_EXPS4<-c("Chimie","Electro","Meca","Ondes","TP Phys","SI")
lesTypeEC_EXPS4<-c("Note","Note","Note","Note","Note","Note")
lesNotesEC_EXPS4 <- list(
  
  list("DS", "TP",NA),         # Chimie
  list("DS", "CC",NA),         # Electro
  list("DS", "CC",NA),         # Meca
  list("DS",NA,NA),               # Ondes
  list("TP",NA,NA),               # Phys
  list("FI",NA,NA)                # SI
)
dataframenotesECTSEXPS4 <- do.call(rbind, lapply(lesNotesEC_EXPS4, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSEXPS4) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

tableauNomUE_EXP4<-data.frame(UE=rep("EXPS4",6))
tableauEC_EXP41<-data.frame(EC=lesEC_EXPS4,TypeEC=lesTypeEC_EXPS4)
tableauEC_EXP42<-data.frame(ECTS=lesECTSEXPS4)
tableauEC_EXP4<-cbind(tableauNomUE_EXP4,tableauEC_EXP41,dataframenotesECTSEXPS4,tableauEC_EXP42)

# -----------------   defintion des constantes pour l'UE ORT ----------------

# attention on peut remarquer que ORTS3 n'a de crédits ECTS que pour TSE et
# le stage, en effet PPI et RIE apparaissent au S3 pour des raisons d'assurance
# mais ne possèdent aucune note.
# pour etre cohérent ne devrait on pas faire c(1,0,0,4) ?
# c'est pire car il y a aussi ADS qui a 0 crédit...

lesECTSORTS3<-c(0,0,0,4,1) # ADS,stage, TSE
# attention comme stage seule EC avec note, porte tous le poids

lesECTSORTS4<-c(0.5,0.5,2) # PPI, RIE,TSE
# attention PPI seul avec note, porte les 3 ects

# une fois le fichier csv lu, les colonne PPI et RIE seront supprimée
# du coup lesEC__ORTS3 sera redéfini sans ces deux colonnes et lesTypeEC_ORTS3
# sera aussi redéfini.
lesEC_ORTS3<-c("ADS","PPI","RIE","Stage","TSE")


lesTypeEC_ORTS3<-c("Validation","Validation","Validation","Note","Validation")
lesNotesEC_ORTS3 <- list(
  
  list(NA,NA,NA),         # ADS
  list(NA,NA,NA),         # PPI
  list(NA,NA,NA),         # RIE
  list("FI",NA,NA),     # Stage
  list(NA,NA,NA)          # TSE
)
dataframenotesECTSORTS3 <- do.call(rbind, lapply(lesNotesEC_ORTS3, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSORTS3) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

tableauNomUE_ORT3<-data.frame(UE=rep("ORTS3",5))
tableauEC_SORT31<-data.frame(EC=lesEC_ORTS3,TypeEC=lesTypeEC_ORTS3)
tableauEC_SORT32<-data.frame(ECTS=lesECTSORTS3)
tableauEC_SORT3<-cbind(tableauNomUE_ORT3,tableauEC_SORT31,dataframenotesECTSORTS3,tableauEC_SORT32)




lesEC_ORTS4<-c("PPI","RIE","TSE")
lesTypeEC_ORTS4<-c("Note","Validation","Validation")
lesNotesEC_ORTS4 <- list(
  
  list("FI",NA,NA),     # PPI
  list(NA,NA,NA),         # RIE
  list(NA,NA,NA)          # TSE
)

dataframenotesECTSORTS4 <- do.call(rbind, lapply(lesNotesEC_ORTS4, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSORTS4) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

tableauNomUE_ORT4<-data.frame(UE=rep("ORTS4",3))
tableauEC_SORT41<-data.frame(EC=lesEC_ORTS4,TypeEC=lesTypeEC_ORTS4)
tableauEC_SORT42<-data.frame(ECTS=lesECTSORTS4)
tableauEC_SORT4<-cbind(tableauNomUE_ORT4,tableauEC_SORT41,dataframenotesECTSORTS4,tableauEC_SORT42)



# -----------------   defintion des constantes pour l'UE HUMA ----------------

lesEC_HUMA_TC<-c("ANG","C&C","EPS","LV2")
lesTypeEC_HUMA_TC<-c("Note","Note","Note","Note")
lesNotesEC_HUMA <- list(
  
  list("DS","CC",NA),     # ANG
  list("DS",NA,NA),         # C&C
  list("DS",NA,NA),          # EPS
  list("DS","CC",NA)
)

dataframenotesECTSHUMA <- do.call(rbind, lapply(lesNotesEC_HUMA, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSHUMA) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")
tableauNomUE_HUMA3<-data.frame(UE=rep("HUMA3",4))
lesECTSHUMAS3<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2
tableauNomUE_HUMA4<-data.frame(UE=rep("HUMA4",4))
lesECTSHUMAS4<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2

tableauEC_HUMA31<-data.frame(EC=lesEC_HUMA_TC,TypeEC=lesTypeEC_HUMA_TC)
tableauEC_HUMA32<-data.frame(ECTS=lesECTSHUMAS3)
tableauEC_HUMA3<-cbind(tableauNomUE_HUMA3,tableauEC_HUMA31,dataframenotesECTSHUMA,tableauEC_HUMA32)

tableauEC_HUMA41<-data.frame(EC=lesEC_HUMA_TC,TypeEC=lesTypeEC_HUMA_TC)
tableauEC_HUMA42<-data.frame(ECTS=lesECTSHUMAS4)
tableauEC_HUMA4<-cbind(tableauNomUE_HUMA4,tableauEC_HUMA41,dataframenotesECTSHUMA,tableauEC_HUMA42)






#--------- definition des constantes pour les options ----------------
# Attention cette UE est tres particuliere car les colonnes concernees ne
# sont pas consécutives. Il faudra donc les traiter séparément.
# c'est pour cela que l'on a des définitions pour LV3 (lesEC et lesTypeEC) pour
# lire le bloc LV3. On pourrait se contenter d'avoir une seule défnition car
# la structure est exactement la meme au S3 et au S4.

lesEC_OPTS3<-c("Bilan","LV3","PLV","AHN")

lesECTS_OPTS3<-c(1,1,1)
nbOptions_S3<-length(lesEC_OPTS3)-1
lesTypeEC_OPTS3<-c("BilanOption","Note","Note","ECTS")
colNotesOPTS3<-c(3,4)
coefOPTS3<-c(1,1)

lesEC_LV3<-c("Allemand 3","Chinois 3","Espagnol 3","FOS 3","Italien 3","Japonais 3","Portugais 3","Russe 3","Soutien Anglais 3")
lesTypeEC_LV3<-rep("Note",9)
tableau_EC_LV3<-data.frame(ListLV3=lesEC_LV3,Evaluation=lesTypeEC_LV3)

lesEC_OPTS4<-c("Bilan","LV3","NPU","AHN","SHN")
lesECTS_OPTS4<-c(1,1,1,1)
nbOptions_S4<-length(lesEC_OPTS4)-1
lesTypeEC_OPTS4<-c("BilanOption","Note","Validation","ECTS","ECTS")
colNotesOPTS4<-c(3)
coefOPTS4<-c(1)

# -----------------   defintion des constantes globales ----------------




LesUE<-rbind(tableauEC_EXP3,tableauEC_EXP4,tableauEC_Fonda3,tableauEC_Fonda4,tableauEC_SORT3,tableauEC_SORT4,tableauEC_HUMA3,tableauEC_HUMA4)

#-------------------- Mise en page Marion (type maquette) --------------------------------
library(officer)
library(flextable)
library(magrittr)

tableau_EXP3<-flextable(tableauEC_EXP3)
tableau_EXP3<-set_table_properties(tableau_EXP3,width = 1,layout="autofit")
tableau_EXP3<-merge_at(tableau_EXP3,i=1:5,j=1)

tableau_Fonda3<-flextable(tableauEC_Fonda3)
tableau_Fonda3<-delete_part(tableau_Fonda3, part="header")
tableau_Fonda3<-set_table_properties(tableau_Fonda3,width = 1,layout="autofit")
tableau_Fonda3<-merge_at(tableau_Fonda3,i=1:4,j=1)

tableau_HUMA3<- flextable(tableauEC_HUMA3)
tableau_HUMA3<-delete_part(tableau_HUMA3, part="header")
tableau_HUMA3<- set_table_properties(tableau_HUMA3,width = 1,layout="autofit")
tableau_HUMA3<-merge_at(tableau_HUMA3,i=1:4,j=1)

tableau_SORT3<-flextable(tableauEC_SORT3)
tableau_SORT3<-delete_part(tableau_SORT3, part="header")
tableau_SORT3<-set_table_properties(tableau_SORT3,width = 1,layout="autofit")
tableau_SORT3<-merge_at(tableau_SORT3,i=1:5,j=1)

tableau_EXP4<- flextable(tableauEC_EXP4)
tableau_EXP4<-set_table_properties(tableau_EXP4,width = 1,layout="autofit")
tableau_EXP4<-merge_at(tableau_EXP4,i=1:6,j=1)

tableau_Fonda4<- flextable(tableauEC_Fonda4)
tableau_Fonda4<-delete_part(tableau_Fonda4, part="header")
tableau_Fonda4<-set_table_properties(tableau_Fonda4,width = 1,layout="autofit")
tableau_Fonda4<-merge_at(tableau_Fonda4,i=1:3,j=1)

tableau_HUMA4<-flextable(tableauEC_HUMA4)
tableau_HUMA4<-delete_part(tableau_HUMA4, part="header")
tableau_HUMA4<- set_table_properties(tableau_HUMA4,width = 1,layout="autofit")
tableau_HUMA4<-merge_at(tableau_HUMA4,i=1:4,j=1)

tableau_SORT4<-flextable(tableauEC_SORT4)
tableau_SORT4<-delete_part(tableau_SORT4, part="header")
tableau_SORT4<-set_table_properties(tableau_SORT4,width = 1,layout="autofit")
tableau_SORT4<-merge_at(tableau_SORT4,i=1:3,j=1)


doc <- read_docx()
doc<-body_add_par(doc,"Matières suivies au S3 :")
doc<- body_add_flextable(doc, value=tableau_EXP3)
doc<- body_add_flextable(doc, value=tableau_Fonda3)
doc<- body_add_flextable(doc, value=tableau_HUMA3)
doc<- body_add_flextable(doc, value=tableau_SORT3)
doc<- body_add_par(doc,"",style="Normal")
doc<-body_add_par(doc,"Matières suivies au S4 :")
doc<- body_add_flextable(doc, value=tableau_EXP4)
doc<- body_add_flextable(doc, value=tableau_Fonda4)
doc<- body_add_flextable(doc, value=tableau_HUMA4)
doc<- body_add_flextable(doc, value=tableau_SORT4)

print(doc, target="/home/marion-vallin-brams/R/Etude Pratique/testContratRedoublement_typemaquette.docx")

#------------------------Mise en page Marion (type contrat)-------------
  
