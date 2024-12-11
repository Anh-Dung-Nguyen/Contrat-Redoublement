lesUE<-c("Sciences Fondamentales","Sciences Expérimentales","Orientation et Transition","Humanités", "Stage" )

# tableau_lesUE<-data.frame(ListDeUE=lesUE)

# -----------------   definition des constantes pour l'UE Sciences Fondamentales ----------------


#  MCC : Partie devant être définie à la main
lesEC_FONDAS3<-c("Algèbre 3","Analyse 3","Informatique 2","Mécanique 3")
lesCodes_FONDAS3<-c("EC-STP03-ALG","EC-STP03-ANA","EC-STP03-INFO","EC-STP03-MECA")
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

tableauNomUE_Fonda3<-data.frame(UE=rep("Sciences fondamentales (UE-STP03-SF)",4))
tableauEC_Fonda31<-data.frame(EC=lesEC_FONDAS3,CodeEC=lesCodes_FONDAS3,TypeEC=lesTypeEC_FONDAS3)
tableauEC_Fonda32<-data.frame(ECTS=lesECTSFONDAS3)
tableauEC_Fonda3<-cbind(tableauNomUE_Fonda3,tableauEC_Fonda31,dataframenotesECFONDAS3,tableauEC_Fonda32)

lesEC_FONDAS4<-c("Geometrie","Informatique 3","Probabilités")
lesCodes_FONDAS4<-c("EC-STP04-GEOM","EC-STP04-INFO","EC-STP04-PROBA")
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


tableauNomUE_Fonda4<-data.frame(UE=rep("Sciences fondamentales (UE-STP04-SF)",3))
tableauEC_Fonda41<-data.frame(EC=lesEC_FONDAS4,CodeEC=lesCodes_FONDAS4,TypeEC=lesTypeEC_FONDAS4)
tableauEC_Fonda42<-data.frame(ECTS=lesECTSFONDAS4)
tableauEC_Fonda4<-cbind(tableauNomUE_Fonda4,tableauEC_Fonda41,dataframenotesECFONDAS4,tableauEC_Fonda42)
# -----------------   defintion des constantes pour l'UE Sciences Expérimentales ----------------


lesECTSEXPS3<-c(3,3.5,1.5,1.5,1.5) # acsa, chimie, electro, TP Phys, thermo

lesECTSEXPS4<-c(3.5,2,2,2,1.5,1) # chimie, electro, meca, ondes, TP Phys, SI


lesEC_EXPS3<-c("Systemes Automatisés","Chimie 3","Electronique 1","TP Physique 3","Thermo-énergétique")
lesCodes_EXPS3<-c("EC-STP03-ACSA","EC-STP03-CHIM","EC-STP03-ELEC","EC-STP03-PHYS","EC-STP03-THEN")
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

tableauNomUE_EXP3<-data.frame(UE=rep("Sciences expérimentales (UE-STP03-SE)",5))
tableauEC_EXP31<-data.frame(EC=lesEC_EXPS3,CodeEC=lesCodes_EXPS3,TypeEC=lesTypeEC_EXPS3)
tableauEC_EXP32<-data.frame(ECTS=lesECTSEXPS3)
tableauEC_EXP3<-cbind(tableauNomUE_EXP3,tableauEC_EXP31,dataframenotesECTSEXPS3,tableauEC_EXP32)


lesEC_EXPS4<-c("Chimie 4","Electromagnétisme","Mecanique 4","Ondes","TP Physique S4","Sciences Industrielles 2")
lesCodes_EXPS4<-c("EC-STP04-CHIM","EC-STP04-ELMG","EC-STP04-MECA","EC-STP04-ONDE","EC-STP04-PHYS","EC-STP04-SI")
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

tableauNomUE_EXP4<-data.frame(UE=rep("Sciences expérimentales (UE-STP04-SE)",6))
tableauEC_EXP41<-data.frame(EC=lesEC_EXPS4,CodeEC=lesCodes_EXPS4,TypeEC=lesTypeEC_EXPS4)
tableauEC_EXP42<-data.frame(ECTS=lesECTSEXPS4)
tableauEC_EXP4<-cbind(tableauNomUE_EXP4,tableauEC_EXP41,dataframenotesECTSEXPS4,tableauEC_EXP42)

# -----------------   defintion des constantes pour l'UE Orientation et Transition ----------------

# attention on peut remarquer que ORTS3 n'a de crédits ECTS que pour TSE et
# le stage, en effet PPI et RIE apparaissent au S3 pour des raisons d'assurance
# mais ne possèdent aucune note.
# pour etre cohérent ne devrait on pas faire c(1,0,0,4) ?
# c'est pire car il y a aussi ADS qui a 0 crédit...

lesECTSORTS3<-c(0,0,0,1) # ADS,stage, TSE
# attention comme stage seule EC avec note, porte tous le poids

lesECTSORTS4<-c(0.5,0.5,2) # PPI, RIE,TSE
# attention PPI seul avec note, porte les 3 ects

# une fois le fichier csv lu, les colonne PPI et RIE seront supprimée
# du coup lesEC__ORTS3 sera redéfini sans ces deux colonnes et lesTypeEC_ORTS3
# sera aussi redéfini.
lesEC_ORTS3<-c("ADS","PPI 3","RIE","TEDS 3")
lesCodes_ORTS3<-c("EC-STP03-ADS","EC-STP03-PPI","EC-STP03-RIE","EC-STP03-TEDS")

lesTypeEC_ORTS3<-c("Validation","Validation","Validation","Note")
lesNotesEC_ORTS3 <- list(

  list(NA,NA,NA),         # ADS
  list(NA,NA,NA),         # PPI
  list(NA,NA,NA),         # RIE
  list("IO","DS",NA)          # TEDS
)
dataframenotesECTSORTS3 <- do.call(rbind, lapply(lesNotesEC_ORTS3, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSORTS3) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

tableauNomUE_ORT3<-data.frame(UE=rep("Orientation et Transition (UE-STP03-ORT)",4))
tableauEC_SORT31<-data.frame(EC=lesEC_ORTS3,CodeEC=lesCodes_ORTS3,TypeEC=lesTypeEC_ORTS3)
tableauEC_SORT32<-data.frame(ECTS=lesECTSORTS3)
tableauEC_SORT3<-cbind(tableauNomUE_ORT3,tableauEC_SORT31,dataframenotesECTSORTS3,tableauEC_SORT32)




lesEC_ORTS4<-c("PPI 4","Parcours RIE","TEDS 4")
lesCodes_ORTS4<-c("EC-STP04-PPI","EC-STP04-RIE","EC-STP04-TEDS")
lesTypeEC_ORTS4<-c("Note","Validation","Note")
lesNotesEC_ORTS4 <- list(

  list("FI",NA,NA),     # PPI
  list(NA,NA,NA),         # RIE
  list("FI",NA,NA)          # TEDS
)

dataframenotesECTSORTS4 <- do.call(rbind, lapply(lesNotesEC_ORTS4, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSORTS4) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

tableauNomUE_ORT4<-data.frame(UE=rep("Orientation et Transition (UE-STP04-ORT)",3))
tableauEC_SORT41<-data.frame(EC=lesEC_ORTS4,CodeEC=lesCodes_ORTS4,TypeEC=lesTypeEC_ORTS4)
tableauEC_SORT42<-data.frame(ECTS=lesECTSORTS4)
tableauEC_SORT4<-cbind(tableauNomUE_ORT4,tableauEC_SORT41,dataframenotesECTSORTS4,tableauEC_SORT42)



# -----------------   defintion des constantes pour l'UE HUMA ----------------

lesEC_HUMA_S3<-c("Anglais 3","Culture et Communication 3 / FLE Comm","EPS 3","Module à choix S3 (LV2-FLE)")
lesCodes_HUMAS3<-c("EC-STP03-ANGL","EC-STP03-COMM / EC-STP03-FLE-COMM","EC-STP03-EPS","EC-STP03-LV2 / EC-STP03-FLE")
lesEC_HUMA_S4<- c("Anglais 4","Culture et Communication 4 / FLE Comm","EPS 4","Module à choix S4 (LV2-FLE)")
lesCodes_HUMAS4<-c("EC-STP04-ANGL","EC-STP04-COMM / EC-STP04-FLE-COMM","EC-STP04-EPS","EC-STP04-LV2 / EC-STP04-FLE")
lesTypeEC_HUMA_TC<-c("Note","Note","Note","Note")
#définition commune puisque même notation pour les matières dans les 2 semestres
lesNotesEC_HUMA <- list(

  list("DS","CC",NA),     # ANG
  list("FI",NA,NA),         # C&C
  list("FI",NA,NA),          # EPS
  list("FI",NA,NA)            #LV2
)

dataframenotesECTSHUMA <- do.call(rbind, lapply(lesNotesEC_HUMA, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECTSHUMA) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")
tableauNomUE_HUMA3<-data.frame(UE=rep("Humanité (UE-STP03-ENS / ENS-FIRE-FR / ENS-FIRE-NFR)",4))
lesECTSHUMAS3<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2
tableauNomUE_HUMA4<-data.frame(UE=rep("Humanité (UE-STP04-ENS / ENS-FIRE-FR / ENS-FIRE-NFR)",4))
lesECTSHUMAS4<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2

tableauEC_HUMA31<-data.frame(EC=lesEC_HUMA_S3,CodeEC=lesCodes_HUMAS3,TypeEC=lesTypeEC_HUMA_TC)
tableauEC_HUMA32<-data.frame(ECTS=lesECTSHUMAS3)
tableauEC_HUMA3<-cbind(tableauNomUE_HUMA3,tableauEC_HUMA31,dataframenotesECTSHUMA,tableauEC_HUMA32)

tableauEC_HUMA41<-data.frame(EC=lesEC_HUMA_S4,CodeEC=lesCodes_HUMAS4,TypeEC=lesTypeEC_HUMA_TC)
tableauEC_HUMA42<-data.frame(ECTS=lesECTSHUMAS4)
tableauEC_HUMA4<-cbind(tableauNomUE_HUMA4,tableauEC_HUMA41,dataframenotesECTSHUMA,tableauEC_HUMA42)

# -----------------   defintion des constantes pour l'UE Stage ----------------

lesEC_Stage<-c("Stage")
lesCodes_Stage<-c("EC-STP03-STAG")
lesTypeEC_Stage<-c("Note")
lesNotesEC_Stage <- list(
  list("FI", NA ,NA)
)

dataframenotesECStage <- do.call(rbind, lapply(lesNotesEC_Stage, function(x) {
  as.data.frame(t(x), stringsAsFactors = FALSE)
}))
colnames(dataframenotesECStage) <- c("Évaluation 1", "Évaluation 2", "Évaluation 3")

lesECTSStage<-c(4.0) # stage

tableauNomUE_Stage<-data.frame(UE=rep("Stage (UE-STP03-STAG)",1))
tableauEC_Stage31<-data.frame(EC=lesEC_Stage,CodeEC=lesCodes_Stage,TypeEC=lesTypeEC_Stage)
tableauEC_Stage32<-data.frame(ECTS=lesECTSStage)
tableauEC_Stage3<-cbind(tableauNomUE_Stage,tableauEC_Stage31,dataframenotesECStage,tableauEC_Stage32)




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
LesUE<-rbind(tableauEC_Fonda3,tableauEC_EXP3,tableauEC_HUMA3,tableauEC_SORT3,tableauEC_Stage3,tableauEC_Fonda4,tableauEC_EXP4,tableauEC_HUMA4,tableauEC_SORT4)
