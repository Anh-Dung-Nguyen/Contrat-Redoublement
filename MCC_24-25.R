lesUE<-c("Sciences Fondamentales","Sciences Expérimentales","Orientation et Transition","Humanités", "Stage" )

#  MCC : Partie devant être définie à la main

# -----------------   definition des constantes pour l'UE Sciences Fondamentales ----------------

#colonnes correspondant à l'UE FONDA dans le fichier jury (début:fin)
col_FONDAS3 <- 15:18
col_FONDAS4 <- 16:18

#UE Fonda 3
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
tableauEC_Fonda3<-cbind(UE=rep("Sciences fondamentales (UE-STP03-SF)",4),
                        EC=lesEC_FONDAS3,
                        CodeEC=lesCodes_FONDAS3,
                        TypeEC=lesTypeEC_FONDAS3,
                        dataframenotesECFONDAS3,
                        ECTS=lesECTSFONDAS3)

#UE Fonda 4
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

tableauEC_Fonda4<-cbind(UE=rep("Sciences fondamentales (UE-STP04-SF)",3),
                        EC=lesEC_FONDAS4,
                        CodeEC=lesCodes_FONDAS4,
                        TypeEC=lesTypeEC_FONDAS4,
                        dataframenotesECFONDAS4,
                        ECTS=lesECTSFONDAS4)

                        
# -----------------   defintion des constantes pour l'UE Sciences Expérimentales ----------------
#colonnes correspondant à l'UE Sciences Expérimentales dans le fichier jury (début:fin)
col_EXPS3 <- (7:11)
col_EXPS4 <- (7:12)

#UE EXP 3
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

lesECTSEXPS3<-c(3,3.5,1.5,1.5,1.5) # acsa, chimie, electro, TP Phys, thermo

tableauEC_EXP3<-cbind(UE=rep("Sciences expérimentales (UE-STP03-SE)",5),
                      EC=lesEC_EXPS3,
                      CodeEC=lesCodes_EXPS3,
                      TypeEC=lesTypeEC_EXPS3
                      ,dataframenotesECTSEXPS3,
                      ECTS=lesECTSEXPS3)

#UE EXP 4
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

lesECTSEXPS4<-c(3.5,2,2,2,1.5,1) # chimie, electro, meca, ondes, TP Phys, SI

tableauEC_EXP4<-cbind(UE=rep("Sciences expérimentales (UE-STP04-SE)",6),
                      EC=lesEC_EXPS4,
                      CodeEC=lesCodes_EXPS4,
                      TypeEC=lesTypeEC_EXPS4,
                      dataframenotesECTSEXPS4,
                      ECTS=lesECTSEXPS4)

# -----------------   defintion des constantes pour l'UE Orientation et Transition ----------------
#colonnes correspondant à l'UE ORT dans le fichier jury (début:fin)
col_ORTS3 <- 26:29
col_ORTS4 <- 22:24

#UE ORT 3
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

lesECTSORTS3<-c(0,0,0,1) # ADS,stage, TSE

tableauEC_SORT3<-cbind(UE=rep("Orientation et Transition (UE-STP03-ORT)",4),
                       EC=lesEC_ORTS3,
                       CodeEC=lesCodes_ORTS3,
                       TypeEC=lesTypeEC_ORTS3,
                       dataframenotesECTSORTS3,
                       ECTS=lesECTSORTS3)

#UE ORT 4
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

lesECTSORTS4<-c(0.5,0.5,2) # PPI, RIE,TSE

tableauEC_SORT4<-cbind(UE=rep("Orientation et Transition (UE-STP04-ORT)",3),
                       EC=lesEC_ORTS4,
                       CodeEC=lesCodes_ORTS4,
                       TypeEC=lesTypeEC_ORTS4,
                       dataframenotesECTSORTS4,
                       ECTS=lesECTSORTS4)



# -----------------   defintion des constantes pour l'UE HUMA ----------------

#colonnes correspondant à l'UE HUMA dans le fichier jury (début:fin)
col_HUMAS3 <- 33:36
col_HUMAS4 <- 28:31

#UE HUMA 3
lesEC_HUMA_S3<-c("Anglais 3","Culture et Communication 3\nFLE Comm","EPS 3","Module à choix S3 (LV2-FLE)")
lesCodes_HUMAS3<-c("EC-STP03-ANGL","EC-STP03-COMM\nEC-STP03-FLE-COMM","EC-STP03-EPS","EC-STP03-LV2\nEC-STP03-FLE")

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

lesECTSHUMAS3<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2

tableauEC_HUMA3<-cbind(UE=rep("Humanité (UE-STP03-ENS / ENS-FIRE-FR / ENS-FIRE-NFR)",4),
                       EC=lesEC_HUMA_S3,
                       CodeEC=lesCodes_HUMAS3,
                       TypeEC=lesTypeEC_HUMA_TC,
                       dataframenotesECTSHUMA,
                       ECTS=lesECTSHUMAS3)

#UE HUMA 4
lesEC_HUMA_S4<- c("Anglais 4","Culture et Communication 4\nFLE Comm","EPS 4","Module à choix S4 (LV2-FLE)")
lesCodes_HUMAS4<-c("EC-STP04-ANGL","EC-STP04-COMM\nEC-STP04-FLE-COMM","EC-STP04-EPS","EC-STP04-LV2\nEC-STP04-FLE")
lesECTSHUMAS4<-c(1.5,1.5,1,1) # Anglais, C&C, EPS, LV2

tableauEC_HUMA4<-cbind(UE=rep("Humanité (UE-STP04-ENS / ENS-FIRE-FR / ENS-FIRE-NFR)",4),
                       EC=lesEC_HUMA_S4,
                       CodeEC=lesCodes_HUMAS4,
                       TypeEC=lesTypeEC_HUMA_TC,
                       dataframenotesECTSHUMA,
                       ECTS=lesECTSHUMAS4)

# -----------------   defintion des constantes pour l'UE Stage ----------------
#colonnes correspondant à l'UE HUMA dans le fichier jury (début:fin)
col_STAGE <- 22

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

tableauEC_Stage3<-cbind(UE=rep("Stage (UE-STP03-STAG)",1),
                        EC=lesEC_Stage,
                        CodeEC=lesCodes_Stage,
                        TypeEC=lesTypeEC_Stage,
                        dataframenotesECStage,
                        ECTS=lesECTSStage)


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
LesUE<-rbind(tableauEC_EXP3,tableauEC_Fonda3,tableauEC_Stage3,tableauEC_SORT3, tableauEC_HUMA3,tableauEC_EXP4,tableauEC_Fonda4,tableauEC_SORT4, tableauEC_HUMA4)

# -------------------constantes pour la création du contrat -------------------
#nom de le.a directeur.ice du département
direction_departement <- "Signature de la Directrice du département : Carole Daiguebonne"

#pour mettre une ligne en gras entre S3 et S4
nb_EC_S3 <- length(lesEC_EXPS3) + length(lesEC_FONDAS3) + length(lesEC_ORTS3) + length(lesEC_HUMA_S3) + length(lesEC_Stage)

#numéros des colonnes contenant les notes dans le fichier jury
col_S3 <- c(col_EXPS3, col_FONDAS3, col_STAGE, col_ORTS3, col_HUMAS3)
col_S4 <- c(col_EXPS4, col_FONDAS4, col_ORTS4, col_HUMAS4)

#numéros des colonnes contenant le résultat de chaque UE (VALIDE, NON VALIDE, VALIDE COMP)
col_val_S3 <- c(4,12,19,23,30)
col_val_S4 <- c(4,13,19,25)

#tableau permettant de merge les cases des UE
nb_EC <- function(tab){
  for(i in 2:length(tab)){
    tab[i] <- tab[i-1] + tab[i]
  }
  return(tab)
}

nb_EC_UE <- c(length(lesEC_EXPS3), length(lesEC_FONDAS3), length(lesEC_Stage), length(lesEC_ORTS3), length(lesEC_HUMA_S3), length(lesEC_EXPS4), length(lesEC_FONDAS4), length(lesEC_ORTS4), length(lesEC_HUMA_S4))

nb_EC_UE <- nb_EC(nb_EC_UE)
