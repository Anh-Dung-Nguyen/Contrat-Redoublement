library(dplyr)

lesUE<-c("FONDA","EXP","ORT","HUMA","OPT")

tableau_lesUE<-data.frame(ListDeUE=lesUE)

# -----------------   génération des notes automatiques ----------------

generer_notes_automatique <- function(UE, EC, evaluations, poids_evaluation, ects) {
  result_list <- list()

  for (i in seq_along(EC)) {
    eval_dispo <- evaluations[[EC[i]]]
    poids_dispo <- poids_evaluation[[EC[i]]]
    eval_dispo <- eval_dispo[!is.na(eval_dispo)]
    poids_dispo <- poids_dispo[!is.na(poids_dispo)]

    if (length(eval_dispo) > 0){
      notes <- sapply(eval_dispo, function(x) sample(0:20, 1))
      moyenne <- sum(notes * poids_dispo) / sum(poids_dispo)
      validation <- ifelse(moyenne >= 10, "Valide", "Non valide")
      df_temp <- data.frame(
        UE = UE,
        EC = EC[i],
        Moyenne = moyenne,
        ECTS = ects[i],
        Validation = validation
      )
      df_temp <- cbind(df_temp, t(notes))
    } else {
      df_temp <- data.frame(
        UE = UE,
        EC = EC[i],
        Moyenne = NA,
        ECTS = ects[i],
        Validation = "Non évalué"
      )
    }

    result_list[[i]] <- df_temp
  }

  df_final <- bind_rows(result_list)
  return (df_final)
}

compensation_intra_UE <- function(df){
  df <- df %>%
    group_by(UE) %>%
    mutate(
      Moyenne_UE = sum(Moyenne * ECTS, na.rm = TRUE) / sum(ECTS[!is.na(Moyenne)], na.rm = TRUE),
      Validation = ifelse(Moyenne_UE >= 10 & Validation == "Non valide", "ValideComp", Validation)
    ) %>%
    ungroup()
  return (df)
}

compensation_inter_UE <- function(df, ue_scientifique){
  df_scientifique <- df %>% filter(UE %in% ue_scientifique)

  moyenne_ponderee_ue <- df_scientifique %>%
    group_by(UE) %>%
    summarise(
      Moyenne_UE = mean(Moyenne, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    summarise(
      Moyenne_globale = mean(Moyenne_UE, na.rm = TRUE)
    ) %>%
    pull(Moyenne_globale)

  if(moyenne_ponderee_ue >= 10){
    df <- df %>%
      mutate(
        Validation = ifelse(UE %in% ue_scientifique & Validation == "Non valide", "ValideComp", Validation)
      )
  }
  return (df)
}

# -----------------   definition des constantes pour l'UE FONDA ----------------


#  MCC : Partie devant être définie à la main
lesEC_FONDAS3<-c("ALG","ANA","INFO","MECA")
lesTypeEC_FONDAS3<-c("Note","Note","Note","Note")
lesNotesEC_FONDAS3 <- list(
  ALG = c("DS1", "DS2", NA),
  ANA = c("DS1", "DS2", NA),
  INFO = c("DS1", "DS2", NA),
  MECA = c("DS1", "DS2", "CC")
)

poids_FONDA_S3 <- list(
  ALG = c(1.5, 1.5),
  ANA = c(1.5, 1.5),
  INFO = c(1, 1),
  MECA = c(1, 3, 1)
)

lesECTSFONDAS3<-c(2.0,3.0,2.0,2.0) # alg, info, ana, meca

resultat_FONDA_S3 <- generer_notes_automatique("FONDA S3", lesEC_FONDAS3, lesNotesEC_FONDAS3, poids_FONDA_S3, lesECTSFONDAS3)

lesEC_FONDAS4<-c("GEOM","INFO","PROBA")
lesTypeEC_FONDAS4<-c("Note","Note","Note")
lesNotesEC_FONDAS4 <- list(
  GEOM = c("DS1", "DS2"),
  INFO = c("DS2", "CC"),
  PROBA = c("DS1", "DS2")
)

poids_FONDA_S4 <- list(
  GEOM = c(1.5, 1.5),
  INFO = c(2, 1),
  PROBA = c(1.5, 1.5)
)

lesECTSFONDAS4<-c(3.5,3,3.5) # géometrie, info, proba

resultat_FONDA_S4 <- generer_notes_automatique("FONDA S4", lesEC_FONDAS4, lesNotesEC_FONDAS4, poids_FONDA_S4, lesECTSFONDAS4)

# -----------------   defintion des constantes pour l'UE EXP ----------------
lesECTSEXPS3<-c(3,3.5,1.5,1.5,1.5) # acsa, chimie, electro, TP Phys, thermo
lesECTSEXPS4<-c(3.5,2,2,2,1.5,1) # chimie, electro, meca, ondes, TP Phys, SI

lesEC_EXPS3<-c("ACSA","CHIM","ELEC","PHYS","THEN")
lesTypeEC_EXPS3<-c("Note","Note","Note","Note","Note")
lesNotesEC_EXPS3 <- list(
  ACSA = c("DS2", "TP"),         # ACSA
  CHIM = c("DS1", "DS2","TP"),    # Chimie
  ELEC = c("DS2", "CC"),         # Electro
  PHYS = c("TP"),               # TP Phys
  THEN = c("DS2")                # Thermo
)

poids_EXP_S3 <- list(
  ACSA = c(5, 3),
  CHIM = c(1, 1, 1),
  ELEC = c(2, 1),
  PHYS = c(2, 1),
  THEN = c(1)
)

resultat_EXP_S3 <- generer_notes_automatique("EXP S3", lesEC_EXPS3, lesNotesEC_EXPS3, poids_EXP_S3, lesECTSEXPS3)

lesEC_EXPS4<-c("CHIM","ELMG","MECA","ONDE","PHYS","SI")
lesTypeEC_EXPS4<-c("Note","Note","Note","Note","Note","Note")
lesNotesEC_EXPS4 <- list(
  CHIM = c("DS2", "TP"),         # Chimie
  ELMG = c("DS2", "CC"),         # Electro
  MECA = c("DS2", "CC"),         # Meca
  ONDE = c("DS2"),               # Ondes
  PHYS = c("TP"),               # Phys
  SI = c("FI")                # SI
)

poids_EXP_S4 <- list(
  CHIM = c(2, 1),
  ELMG = c(2, 1),
  MECA = c(1, 1),
  ONDE = c(1),
  PHYS = c(1),
  SI = c(1)
)

resultat_EXP_S4 <- generer_notes_automatique("EXP S4", lesEC_EXPS4, lesNotesEC_EXPS4, poids_EXP_S4, lesECTSEXPS4)

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
lesEC_ORTS3<-c("ADS","PPI","RIE","STAG","TSE")
lesTypeEC_ORTS3<-c("Validation","Validation","Validation","Note","Validation")
lesNotesEC_ORTS3 <- list(
  ADS = c(NA,NA,NA),         # ADS
  PPI = c(NA,NA,NA),         # PPI
  RIE = c(NA,NA,NA),         # RIE
  STAG = c("FI",NA,NA),     # Stage
  TSE = c(NA,NA,NA)          # TSE
)

poids_ORT_S3 <- list(
  ADS = c(),
  PPI = c(),
  RIE = c(),
  STAG = c(1),
  TSE = c()
)

resultat_ORT_S3 <- generer_notes_automatique("ORT S3", lesEC_ORTS3, lesNotesEC_ORTS3, poids_ORT_S3, lesECTSORTS3)

lesEC_ORTS4<-c("PPI","RIE","TSE")
lesTypeEC_ORTS4<-c("Note","Validation","Validation")
lesNotesEC_ORTS4 <- list(
  PPI = c("FI",NA,NA),     # PPI
  RIE = c(NA,NA,NA),         # RIE
  TSE = c(NA,NA,NA)          # TSE
)

poids_ORT_S4 <- list(
  PPI = c(1),
  RIE = c(),
  TSE = c()
)

resultat_ORT_S4 <- generer_notes_automatique("ORT S4", lesEC_ORTS4, lesNotesEC_ORTS4, poids_ORT_S4, lesECTSORTS4)

# -----------------   defintion des constantes pour l'UE HUMA ----------------

lesEC_HUMA_TC<-c("ANGL","COMM","EPS","LV2")
lesTypeEC_HUMA_TC<-c("Note","Note","Note","Note")
lesNotesEC_HUMA <- list(
  ANGL = c("DS2","CC"),     # ANG
  COMM = c("DS2"),         # C&C
  EPS = c("DS2"),          # EPS
  LV2 = c("DS2","CC")
)
lesECTSHUMAS3<-c(1.5,1.5,1,1)
lesECTSHUMAS4<-c(1.5,1.5,1,1)

poids_HUMA_S3 <- list(
  ANGL = c(1.5, 1.5),
  COMM = c(1),
  EPS = c(1),
  LV2 = c(1)
)

resultat_HUMA_S3 <- generer_notes_automatique("HUMA S3", lesEC_HUMA_TC, lesNotesEC_HUMA, poids_HUMA_S3, lesECTSHUMAS3)

poids_HUMA_S4 <- list(
  ANGL = c(1.5, 1.5),
  COMM = c(1),
  EPS = c(1),
  LV2 = c(1)
)

resultat_HUMA_S4 <- generer_notes_automatique("HUMA S4", lesEC_HUMA_TC, lesNotesEC_HUMA, poids_HUMA_S4, lesECTSHUMAS4)

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

dfs3 <- bind_rows(resultat_FONDA_S3, resultat_EXP_S3, resultat_ORT_S3, resultat_HUMA_S3)
dfs3 <- compensation_intra_UE(dfs3)
dfs3 <- compensation_inter_UE(dfs3, ue_scientifique = c("FONDA S3", "EXP S3"))

dfs4 <- bind_rows(resultat_FONDA_S4, resultat_EXP_S4, resultat_ORT_S4, resultat_HUMA_S4)
dfs4 <- compensation_intra_UE(dfs4)
dfs4 <- compensation_inter_UE(dfs4, ue_scientifique = c("FONDA S4", "EXP S4"))

LesUE <- bind_rows(dfs3, dfs4)
