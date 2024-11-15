lesUE<-c("FONDA","EXP","ORT","HUMA","OPT")

# -----------------   definition des constantes pour l'UE FONDA ----------------


#  MCC : Partie devant être définie à la main
lesEC_FONDAS3<-c("Bilan","ALG","ANA","INFO","MECA")
lesTypeEC_FONDAS3<-c("Bilan","Note","Note","Note","Note")
lesNotesEC_FONDAS3 <- list(
  list(),
  list("DS1", "DS2"),
  list("DS1", "DS2"),
  list("DS1", "DS2"),
  list("DS1", "DS2", "CC")
)
lesECTSFONDAS3<-c(2,3,2,2) # alg, info, ana, meca
coefFONDAS3<-sum(lesECTSFONDAS3)

lesEC_FONDAS4<-c("Bilan","Geometrie","INFO","PROBA")
lesTypeEC_FONDAS4<-c("Bilan","Note","Note","Note")
# Définir lesNotesEC_FONDAS3 correctement
lesNotesEC_FONDAS4 <- list(
  list(),
  list("DS1", "DS2"),
  list("DS", "CC"),
  list("DS1", "DS2")
)
lesECTSFONDAS4<-c(3.5,3,3.5) # géometrie, info, proba
coefFONDAS4<-sum(lesECTSFONDAS4)

# -----------------   defintion des constantes pour l'UE EXP ----------------


lesECTSEXPS3<-c(3,3.5,1.5,1.5,1.5) # acsa, chimie, electro, TP Phys, thermo
coefEXPS3<-sum(lesECTSEXPS3)
lesECTSEXPS4<-c(3.5,2,2,2,1.5,1) # chimie, electro, meca, ondes, TP Phys, SI
coefEXPS4<-sum(lesECTSEXPS4)

lesEC_EXPS3<-c("Bilan","ACSA","Chimie","Electro","TP Phys","Thermo")
lesTypeEC_EXPS3<-c("Bilan","Note","Note","Note","Note","Note")
lesNotesEC_EXPS3 <- list(
  list(),
  list("DS", "TP"),         # ACSA
  list("DS", "DS","TP"),    # Chimie
  list("DS", "CC"),         # Electro
  list("TP"),               # TP Phys
  list("DS")                # Thermo
)


lesEC_EXPS4<-c("Bilan","Chimie","Electro","Meca","Ondes","TP Phys","SI")
lesTypeEC_EXPS4<-c("Bilan","Note","Note","Note","Note","Note","Note")
lesNotesEC_EXPS4 <- list(
  list(),
  list("DS", "TP"),         # Chimie
  list("DS", "CC"),         # Electro
  list("DS", "CC"),         # Meca
  list("DS"),               # Ondes
  list("TP"),               # Phys
  list("FI")                # SI
)

# -----------------   defintion des constantes pour l'UE ORT ----------------

# attention on peut remarquer que ORTS3 n'a de crédits ECTS que pour TSE et 
# le stage, en effet PPI et RIE apparaissent au S3 pour des raisons d'assurance
# mais ne possèdent aucune note.
# pour etre cohérent ne devrait on pas faire c(1,0,0,4) ?
# c'est pire car il y a aussi ADS qui a 0 crédit...

lesECTSORTS3<-c(0,4,1) # ADS,stage, TSE
lesCoefsORTS3<-c(5) # attention comme stage seule EC avec note, porte tous le poids
coefORTS3<-sum(lesECTSORTS3)
lesECTSORTS4<-c(0.5,0.5,2) # PPI, RIE,TSE
coefORTS4<-sum(lesECTSORTS4)
lesCoefsORTS4<-c(3) # attention PPI seul avec note, porte les 3 ects

# une fois le fichier csv lu, les colonne PPI et RIE seront supprimée
# du coup lesEC__ORTS3 sera redéfini sans ces deux colonnes et lesTypeEC_ORTS3
# sera aussi redéfini. 
lesEC_ORTS3<-c("ORT Bilan","ADS","PPI","RIE","Stage","TSE")


lesTypeEC_ORTS3<-c("Bilan","Validation","Validation","Validation","Note","Validation")
lesNotesEC_ORTS3 <- list(
  list(),
  list(),         # ADS
  list(),         # PPI
  list(),         # RIE
  list("FI"),     # Stage
  list()          # TSE
)


lesEC_ORTS4<-c("Bilan","PPI","RIE","TSE")
lesTypeEC_ORTS4<-c("Bilan","Note","Validation","Validation")
lesNotesEC_ORTS4 <- list(
  list(),
  list("FI"),     # PPI
  list(),         # RIE
  list()          # TSE
)

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

lesEC_OPTS4<-c("Bilan","LV3","NPU","AHN","SHN")
lesECTS_OPTS4<-c(1,1,1,1)
nbOptions_S4<-length(lesEC_OPTS4)-1
lesTypeEC_OPTS4<-c("BilanOption","Note","Validation","ECTS","ECTS")
colNotesOPTS4<-c(3)
coefOPTS4<-c(1)

# -----------------   defintion des constantes globales ----------------

lesCoefsS3<-c(coefEXPS3,coefFONDAS3,coefORTS3,coefHUMAS3)
lesECTSS3<-lesCoefsS3 
lesCoefsS4<-c(coefEXPS4,coefFONDAS4,coefORTS4,coefHUMAS4)
lesECTSS4<-lesCoefsS4


