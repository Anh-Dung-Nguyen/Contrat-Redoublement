library(dplyr)

generer_notes_automatique <- function(UE, EC, evaluations){
  result_list <- list()
  
  for (i in seq_along(EC)){
    eval_dispo <- evaluations[[EC[i]]]
    notes <- sapply(eval_dispo, function(x) sample(0:20, 1))
    moyenne <- mean(notes)
    validation <- ifelse(moyenne >= 10, "Valide", "Non valide")
    
    df_temp <- data.frame(
      UE = UE,
      EC = EC[i],
      Moyenne = moyenne,
      Validation = validation
    )
    
    df_temp <- cbind(df_temp, t(notes))
    
    result_list[[i]] <- df_temp
  }
  
  df_final <- bind_rows(result_list)
  return (df_final)
}

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

EC_EXP_S4 <- c("Chimie","Electro","Meca","Ondes","TP Phys","SI")
evaluations_EXP_S4 <- list(
  Chimie = c("DS2", "TP"),
  Electro = c("DS2", "CC"),
  Meca = c("DS2", "CC"),
  Ondes = c("DS2"),
  TP_Phys = c("TP"),
  SI = c("TP")
)
dfs4_exp <- generer_notes_automatique("EXP S4", EC_EXP_S4, evaluations_EXP_S4)

EC_ORT_S4 <- c("PPI","RIE","TSE")
evaluations_ORT_S4 <- list(
  PPI = c("DS2"),
  RIE = c(),
  TSE = c()
)
dfs4_ort <- generer_notes_automatique("ORT S4", EC_ORT_S4, evaluations_ORT_S4)

EC_HUMATC_S4 <- c("ANG","C&C","EPS")
evaluations_HUMATC_S4 <-list(
  ANG = c("DS2"),
  C_C = c("DS2"),
  EPS = c("DS2")
)
dfs4_humatc <- generer_notes_automatique("HUMATC S4", EC_HUMATC_S4, evaluations_HUMATC_S4)

EC_HUMA_S4 <- c(dfs4_humatc$EC, "LV2")
evaluations_HUMA_S4 <- list(
  ANG = evaluations_HUMATC_S4$ANG,
  C_C = evaluations_HUMATC_S4$C_C,
  EPS = evaluations_HUMATC_S4$EPS,
  LV2 = c("DS2")
)
dfs4_huma <- generer_notes_automatique("HUMA S4", EC_HUMA_S4, evaluations_HUMA_S4)

# Affichage des dataframes
dfs3 <- bind_rows(dfs3_fonda, dfs3_exp, dfs3_ort, dfs3_huma)
dfs4 <- bind_rows(dfs4_fonda, dfs4_exp, dfs4_ort, dfs4_huma)
df <- bind_rows(dfs3, dfs4)

print (dfs3)
print (dfs4)
print (df)