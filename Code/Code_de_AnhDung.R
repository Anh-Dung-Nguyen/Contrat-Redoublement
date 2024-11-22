library(dplyr)

generer_notes_automatique <- function(UE, EC, evaluations, poids_evaluation) {
  result_list <- list()

  for (i in seq_along(EC)) {
    eval_dispo <- evaluations[[EC[i]]]
    poids_dispo <- poids_evaluation[[EC[i]]]

    if (length(eval_dispo) > 0){
      notes <- sapply(eval_dispo, function(x) sample(0:20, 1))
      moyenne <- sum(notes * poids_dispo) / sum(poids_dispo)
      validation <- ifelse(moyenne >= 10, "Valide", "Non valide")
      df_temp <- data.frame(
        UE = UE,
        EC = EC[i],
        Moyenne = moyenne,
        Validation = validation
      )
      df_temp <- cbind(df_temp, t(notes))
    } else {
        df_temp <- data.frame(
          UE = UE,
          EC = EC[i],
          Moyenne = NA,
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
      Moyenne_EC = mean(Moyenne, na.rm = TRUE),
      Validation = ifelse(Moyenne_EC >= 10 & Validation == "Non valide", "ValideComp", Validation)
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

# EC pour S3
EC_FONDA_S3 <- c("ALG","ANA","INFO","MECA")
evaluations_FONDA_S3 <- list(
  ALG = c("DS1", "DS2"),
  ANA = c("DS1", "DS2"),
  INFO = c("DS1", "DS2"),
  MECA = c("DS1", "DS2", "CC")
)

poids_FONDA_S3 <- list(
  ALG = c(1.5, 1.5),
  ANA = c(1.5, 1.5),
  INFO = c(1, 1),
  MECA = c(1, 3, 1)
)

dfs3_fonda <- generer_notes_automatique("FONDA S3", EC_FONDA_S3, evaluations_FONDA_S3, poids_FONDA_S3)

EC_EXP_S3 <- c("ACSA","Chimie","Electro","TP_Phys","Thermo")
evaluations_EXP_S3 <- list(
  ACSA = c("DS2", "TP"),
  Chimie = c("DS1", "DS2","TP"),
  Electro = c("DS2", "CC"),
  TP_Phys = c("TP"),
  Thermo = c("DS2")
)

poids_EXP_S3 <- list(
  ACSA = c(5, 3),
  Chimie = c(1, 1, 1),
  Electro = c(2, 1),
  TP_Phys = c(2, 1),
  Thermo = c(1)
)

dfs3_exp <- generer_notes_automatique("EXP S3", EC_EXP_S3, evaluations_EXP_S3, poids_EXP_S3)

EC_ORT_S3 <- c("ADS","PPI","RIE","Stage","TSE")
evaluations_ORT_S3 <- list(
  ADS = c(),
  PPI = c(),
  RIE = c(),
  Stage = c("DS2"),
  TSE = c()
)

poids_ORT_S3 <- list(
  ADS = c(),
  PPI = c(),
  RIE = c(),
  Stage = c(1),
  TSE = c()
)

dfs3_ort <- generer_notes_automatique("ORT S3", EC_ORT_S3, evaluations_ORT_S3, poids_ORT_S3)

EC_HUMA_S3 <- c("ANG","C_C","EPS", "LV2")
evaluations_HUMA_S3 <- list(
  ANG = c("DS2", "CC"),
  C_C = c("DS2"),
  EPS = c("DS2"),
  LV2 = c("DS2")
)

poids_HUMAN_S3 <- list(
  ANG = c(1.5, 1.5),
  C_C = c(1),
  EPS = c(1),
  LV2 = c(1)
)

dfs3_huma <- generer_notes_automatique("HUMA S3", EC_HUMA_S3, evaluations_HUMA_S3, poids_HUMAN_S3)

# EC pour S4
EC_FONDA_S4 <- c("Geometrie","INFO","PROBA")
evaluations_FONDA_S4 <- list(
  Geometrie = c("DS1", "DS2"),
  INFO = c("DS2", "CC"),
  PROBA = c("DS1", "DS2")
)

poids_FONDA_S4 <- list(
  Geometrie = c(1.5, 1.5),
  INFO = c(2, 1),
  PROBA = c(1.5, 1.5)
)

dfs4_fonda <- generer_notes_automatique("FONDA S4", EC_FONDA_S4, evaluations_FONDA_S4, poids_FONDA_S4)

EC_EXP_S4 <- c("Chimie","Electro","Meca","Ondes","TP_Phys","SI")
evaluations_EXP_S4 <- list(
  Chimie = c("DS2", "TP"),
  Electro = c("DS2", "CC"),
  Meca = c("DS2", "CC"),
  Ondes = c("DS2"),
  TP_Phys = c("TP"),
  SI = c("TP")
)

poids_EXP_S4 <- list(
  Chimie = c(2, 1),
  Electro = c(2, 1),
  Meca = c(1, 1),
  Ondes = c(1),
  TP_Phys = c(1),
  SI = c(1)
)

dfs4_exp <- generer_notes_automatique("EXP S4", EC_EXP_S4, evaluations_EXP_S4, poids_EXP_S4)

EC_ORT_S4 <- c("PPI","RIE","TSE")
evaluations_ORT_S4 <- list(
  PPI = c("DS2"),
  RIE = c(),
  TSE = c()
)

poids_ORT_S4 <- list(
  PPI = c(1),
  RIE = c(),
  TSE = c()
)

dfs4_ort <- generer_notes_automatique("ORT S4", EC_ORT_S4, evaluations_ORT_S4, poids_ORT_S4)

EC_HUMA_S4 <- c("ANG","C&C","EPS","LV2")
evaluations_HUMA_S4 <- list(
  ANG = c("DS2", "CC"),
  C_C = c("DS2"),
  EPS = c("DS2"),
  LV2 = c("DS2")
)

poids_HUMAN_S4 <- list(
  ANG = c(1.5, 1.5),
  C_C = c(1),
  EPS = c(1),
  LV2 = c(1)
)

dfs4_huma <- generer_notes_automatique("HUMA S4", EC_HUMA_S4, evaluations_HUMA_S4, poids_HUMAN_S4)

# Affichage des dataframes
dfs3 <- bind_rows(dfs3_fonda, dfs3_exp, dfs3_ort, dfs3_huma)
dfs3 <- compensation_intra_UE(dfs3)
dfs3 <- compensation_inter_UE(dfs3, ue_scientifique = c("FONDA S3", "EXP S3"))

dfs4 <- bind_rows(dfs4_fonda, dfs4_exp, dfs4_ort, dfs4_huma)
dfs4 <- compensation_intra_UE(dfs4)
dfs4 <- compensation_inter_UE(dfs4, ue_scientifique = c("FONDA S4", "EXP S4"))

df <- bind_rows(dfs3, dfs4)

print (dfs3)
print (dfs4)
print (df)
