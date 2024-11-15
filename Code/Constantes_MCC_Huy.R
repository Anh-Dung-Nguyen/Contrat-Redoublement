lesUE<-c("FONDA","EXP","ORT","HUMA","OPT")

tableau_lesUE<-data.frame(ListDeUE=lesUE)

# -----------------   definition des constantes pour l'UE FONDA ----------------


#  MCC : Partie devant être définie à la main
lesEC_FONDAS3<-c("Bilan","ALG","ANA","INFO","MECA")
lesTypeEC_FONDAS3<-c("Bilan","Note","Note","Note","Note")
lesNotesEC_FONDAS3 <- list(
  list(NA,NA,NA),
  list("DS1", "DS2",NA),
  list("DS1", "DS2",NA),
  list("DS1", "DS2",NA),
  list("DS1", "DS2", "CC")
)

dataframenotesECFONDAS3 <- do.call(rbind, lapply(lesNotesEC_FONDAS3, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECFONDAS3) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

lesECTSFONDAS3<-c(0,2,3,2,2) # alg, info, ana, meca
coefFONDAS3<-sum(lesECTSFONDAS3)
vecteurcoeffFONDA3<-c(coefFONDAS3,NA,NA,NA,NA)
dataframecoeffFONDA3<-data.frame(coeffTotal=vecteurcoeffFONDA3)

tableauEC_Fonda31<-data.frame(list_EC_Fondas3=lesEC_FONDAS3,TypeEC=lesTypeEC_FONDAS3)
tableauEC_Fonda32<-data.frame(ECTSFONDAS3=lesECTSFONDAS3)
tableauEC_Fonda3<-cbind(tableauEC_Fonda31,dataframenotesECFONDAS3,tableauEC_Fonda32,dataframecoeffFONDA3)

lesEC_FONDAS4<-c("Bilan","Geometrie","INFO","PROBA")
lesTypeEC_FONDAS4<-c("Bilan","Note","Note","Note")
# Définir lesNotesEC_FONDAS3 correctement
lesNotesEC_FONDAS4 <- list(
  list(NA,NA),
  list("DS1", "DS2"),
  list("DS", "CC"),
  list("DS1", "DS2")
)

dataframenotesECFONDAS4 <- do.call(rbind, lapply(lesNotesEC_FONDAS4, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECFONDAS3) <- c("Évaluation 1", "Évaluation 2")

lesECTSFONDAS4<-c(0,3.5,3,3.5) # géometrie, info, proba
coefFONDAS4<-sum(lesECTSFONDAS4)
vecteurcoeffFONDA4<-c(coefFONDAS4,NA,NA,NA)
dataframecoeffFONDA4<-data.frame(coeffTotal=vecteurcoeffFONDA4)

tableauEC_Fonda41<-data.frame(list_EC_Fondas4=lesEC_FONDAS4,TypeEC=lesTypeEC_FONDAS4)
tableauEC_Fonda42<-data.frame(ECTSFONDAS4=lesECTSFONDAS4)
tableauEC_Fonda4<-cbind(tableauEC_Fonda41,dataframenotesECFONDAS4,tableauEC_Fonda42,dataframecoeffFONDA4)
# -----------------   defintion des constantes pour l'UE EXP ----------------


lesECTSEXPS3<-c(0,3,3.5,1.5,1.5,1.5) # acsa, chimie, electro, TP Phys, thermo
coefEXPS3<-sum(lesECTSEXPS3)
lesECTSEXPS4<-c(0,3.5,2,2,2,1.5,1) # chimie, electro, meca, ondes, TP Phys, SI
coefEXPS4<-sum(lesECTSEXPS4)

lesEC_EXPS3<-c("Bilan","ACSA","Chimie","Electro","TP Phys","Thermo")
lesTypeEC_EXPS3<-c("Bilan","Note","Note","Note","Note","Note")
lesNotesEC_EXPS3 <- list(
  list(NA,NA,NA),
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
vecteurcoeffEXP3<-c(coefEXPS3,NA,NA,NA,NA,NA)
dataframecoeffEXP3<-data.frame(coeffTotal=vecteurcoeffEXP3)

tableauEC_EXP31<-data.frame(list_EC_EXPS3=lesEC_EXPS3,TypeEC=lesTypeEC_EXPS3)
tableauEC_EXP32<-data.frame(ECTSEXPS3=lesECTSEXPS3)
tableauEC_EXP3<-cbind(tableauEC_EXP31,dataframenotesECTSEXPS3,tableauEC_EXP32,dataframecoeffEXP3)


lesEC_EXPS4<-c("Bilan","Chimie","Electro","Meca","Ondes","TP Phys","SI")
lesTypeEC_EXPS4<-c("Bilan","Note","Note","Note","Note","Note","Note")
lesNotesEC_EXPS4 <- list(
  list(NA,NA,NA),
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
vecteurcoeffEXP4<-c(coefEXPS4,NA,NA,NA,NA,NA,NA)
dataframecoeffEXP4<-data.frame(coeffTotal=vecteurcoeffEXP4)

tableauEC_EXP41<-data.frame(list_EC_EXPS4=lesEC_EXPS4,TypeEC=lesTypeEC_EXPS4)
tableauEC_EXP42<-data.frame(ECTSEXPS4=lesECTSEXPS4)
tableauEC_EXP4<-cbind(tableauEC_EXP41,dataframenotesECTSEXPS4,tableauEC_EXP42,dataframecoeffEXP4)

# -----------------   defintion des constantes pour l'UE ORT ----------------

# attention on peut remarquer que ORTS3 n'a de crédits ECTS que pour TSE et 
# le stage, en effet PPI et RIE apparaissent au S3 pour des raisons d'assurance
# mais ne possèdent aucune note.
# pour etre cohérent ne devrait on pas faire c(1,0,0,4) ?
# c'est pire car il y a aussi ADS qui a 0 crédit...

lesECTSORTS3<-c(0,0,0,0,4,1) # ADS,stage, TSE
lesCoefsORTS3<-c(5) # attention comme stage seule EC avec note, porte tous le poids
coefORTS3<-sum(lesECTSORTS3)
lesECTSORTS4<-c(0,0.5,0.5,2) # PPI, RIE,TSE
coefORTS4<-sum(lesECTSORTS4)
lesCoefsORTS4<-c(3) # attention PPI seul avec note, porte les 3 ects

# une fois le fichier csv lu, les colonne PPI et RIE seront supprimée
# du coup lesEC__ORTS3 sera redéfini sans ces deux colonnes et lesTypeEC_ORTS3
# sera aussi redéfini. 
lesEC_ORTS3<-c("ORT Bilan","ADS","PPI","RIE","Stage","TSE")


lesTypeEC_ORTS3<-c("Bilan","Validation","Validation","Validation","Note","Validation")
lesNotesEC_ORTS3 <- list(
  list(NA),
  list(NA),         # ADS
  list(NA),         # PPI
  list(NA),         # RIE
  list("FI"),     # Stage
  list(NA)          # TSE
)
dataframenotesECTSORTS3 <- do.call(rbind, lapply(lesNotesEC_ORTS3, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSORTS3) <- c("Évaluation 1")
vecteurcoeffSORT3<-c(coefORTS3,NA,NA,NA,NA,NA)
dataframecoeffSORT3<-data.frame(coeffTotal=vecteurcoeffSORT3)

tableauEC_SORT31<-data.frame(list_EC_EXPS3=lesEC_ORTS3,TypeEC=lesTypeEC_ORTS3)
tableauEC_SORT32<-data.frame(ECTORTSS3=lesECTSORTS3)
tableauEC_SORT3<-cbind(tableauEC_SORT31,dataframenotesECTSORTS3,tableauEC_SORT32,dataframecoeffSORT3)




lesEC_ORTS4<-c("Bilan","PPI","RIE","TSE")
lesTypeEC_ORTS4<-c("Bilan","Note","Validation","Validation")
lesNotesEC_ORTS4 <- list(
  list(NA),
  list("FI"),     # PPI
  list(NA),         # RIE
  list(NA)          # TSE
)

dataframenotesECTSORTS4 <- do.call(rbind, lapply(lesNotesEC_ORTS4, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSORTS4) <- c("Évaluation 1")
vecteurcoeffSORT4<-c(coefORTS3,NA,NA,NA)
dataframecoeffSORT4<-data.frame(coeffTotal=vecteurcoeffSORT4)

tableauEC_SORT41<-data.frame(list_EC_EXPS4=lesEC_ORTS4,TypeEC=lesTypeEC_ORTS4)
tableauEC_SORT42<-data.frame(ECTORTSS4=lesECTSORTS4)
tableauEC_SORT4<-cbind(tableauEC_SORT41,dataframenotesECTSORTS4,tableauEC_SORT42,dataframecoeffSORT4)



# -----------------   defintion des constantes pour l'UE HUMA ----------------

lesEC_HUMA_TC<-c("Bilan","ANG","C&C","EPS") 
lesTypeEC_HUMA_TC<-c("Bilan","Note","Note","Note") 

lesEC_HUMA<-c(lesEC_HUMA_TC,"LV2")
type_LV2<-"Note"
lesTypeEC_HUMA<-c(lesTypeEC_HUMA_TC,type_LV2)

lesECTSHUMAS3<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2 
coefHUMAS3<-sum(lesECTSORTS3)
lesECTSHUMAS4<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2 
coefHUMAS4<-sum(lesECTSHUMAS4)

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

lesNomsDesUES3<-c("EXP","FONDA","ORT","HUMA")
lesCoefsS3<-c(coefEXPS3,coefFONDAS3,coefORTS3,coefHUMAS3)
lesECTSS3<-data.frame(NomsDesUES3=lesNomsDesUES3,Coeff=lesCoefsS3)


lesNomsDesUES4<-c("EXP","FONDA","ORT","HUMA")
lesCoefsS4<-c(coefEXPS4,coefFONDAS4,coefORTS4,coefHUMAS4)
lesECTSS4<-data.frame(NomsDesUES4=lesNomsDesUES4,Coeff=lesCoefsS4)


