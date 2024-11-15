library(dplyr)

#------------------- generer_notes_automatique --------------------------
# UE :  nom d'une UE
# lesEC : nom des EC de l'UE
# evaluations : liste de liste de nom des evaluations # je ne l'ai compris
# qu'apres avoir vu la declaration des EC de fonda3
# elle possede. Si c'est 0 cela signifie que c'est une methode avec une simple
# validation

# !!! attention danger des compensations... a regarder
# !!! danger il faut prendre en compte les poids des evaluations - pour faire
# une vraie simulation
# !!! cas ou aucune evaluation, il ne faut pas calculer la moyenne

generer_notes_automatique <- function(UE, lesEC, evaluations){
  result_list <- list()
  
  for (i in seq_along(lesEC)){
    eval_dispo <- evaluations[[lesEC[i]]]
    notes <- sapply(eval_dispo, function(x) sample(0:20, 1))
    moyenne <- mean(notes)
    validation <- ifelse(moyenne >= 10, "Valide", "Non valide")
    
    df_temp <- data.frame(
      UE = UE,
      lesEC = lesEC[i],
      Moyenne = moyenne,
      Validation = validation
    )
    
    df_temp <- cbind(df_temp, t(notes))
    result_list[[i]] <- df_temp
  }
  df_final <- bind_rows(result_list)
  return (df_final)
}

#-------------------- définition des EC du S3 ---------------------------
# peut etre mettre les declarations au depart pour aider a la comprehension

# EC pour S3
EC_FONDA_S3 <- c("ALG","ANA","INFO","MECA")
evaluations_FONDA_S3 <- list(
  ALG = c("DS1", "DS2"),
  ANA = c("DS1", "DS2"),
  INFO = c("DS1", "DS2"),
  MECA = c("DS1", "DS2", "CC")
)
dfs3_fonda <- generer_notes_automatique("FONDA S3", EC_FONDA_S3, evaluations_FONDA_S3)

EC_EXP_S3 <- c("ACSA","Chimie","Electro","TP Phys","Thermo")
evaluations_EXP_S3 <- list(
  ACSA = c("DS2", "TP"),
  Chimie = c("DS1", "DS2","TP"),
  Electro = c("DS2", "CC"),
  TP_Phys = c("TP"),
  Thermo = c("DS2")
)
dfs3_exp <- generer_notes_automatique("EXP S3", EC_EXP_S3, evaluations_EXP_S3)

EC_ORT_S3 <- c("ADS","PPI","RIE","Stage","TSE")
evaluations_ORT_S3 <- list(
  ADS = c(),
  PPI = c(),
  RIE = c(),
  Stage = c("DS2"),
  TSE = c()
)
dfs3_ort <- generer_notes_automatique("ORT S3", EC_ORT_S3, evaluations_ORT_S3)

EC_HUMATC_S3 <- c("ANG","C&C","EPS")
evaluations_HUMATC_S3 <-list(
  ANG = c("DS2"),
  C_C = c("DS2"),
  EPS = c("DS2")
)
dfs3_humatc <- generer_notes_automatique("HUMATC S3", EC_HUMATC_S3, evaluations_HUMATC_S3)

EC_HUMA_S3 <- c(dfs3_humatc$EC, "LV2")
evaluations_HUMA_S3 <- list(
  ANG = evaluations_HUMATC_S3$ANG,
  C_C = evaluations_HUMATC_S3$C_C,
  EPS = evaluations_HUMATC_S3$EPS,
  LV2 = c("DS2")
)
dfs3_huma <- generer_notes_automatique("HUMA S3", EC_HUMA_S3, evaluations_HUMA_S3)

# EC pour S4
EC_FONDA_S4 <- c("Geometrie","INFO","PROBA")
evaluations_FONDA_S4 <- list(
  Geometrie = c("DS1", "DS2"),
  INFO = c("DS2", "CC"),
  PROBA = c("DS1", "DS2")
)
dfs4_fonda <- generer_notes_automatique("FONDA S4", EC_FONDA_S4, evaluations_FONDA_S4)