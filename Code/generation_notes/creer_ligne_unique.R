source("Code/cste/Constantes_MCC.R")
source("Code/generation_notes/compensation.R")
source("Code/generation_notes/generer_notes_automatique.R")
library(dplyr)

# Calculer les données synthétiques pour chaque UE
creer_ligne_unique_s3 <- function(df) {
  # Fonction utilitaire pour extraire les données pour une UE spécifique
  extraire_donnees <- function(df, ue_name, ecs) {
    sous_df <- df %>% filter(UE == ue_name)
    validation_ue <- unique(sous_df$Validation)
    somme_ects <- sum(sous_df$ECTS, na.rm = TRUE)
    moyenne_ue <- unique(sous_df$Moyenne_UE)
    validation_ue <- ifelse(moyenne_ue >= 10, "Valide", "Non valide")
    moyennes_ec <- sapply(ecs, function(ec) {
      moyenne_ec <- sous_df %>% filter(EC == ec) %>% pull(Moyenne)
      if (length(moyenne_ec) == 0) return(NA) else return(moyenne_ec)
    })
    return(c(validation_ue, somme_ects, moyenne_ue, moyennes_ec))
  }
  
  # Créer la ligne unique avec les données demandées
  ligne_s3 <- c(
    extraire_donnees(df, lesUE[2], lesEC_EXPS3),
    extraire_donnees(df, lesUE[1], lesEC_FONDAS3),
    extraire_donnees(df, lesUE[3], lesEC_ORTS3),
    extraire_donnees(df, lesUE[4], lesEC_HUMA_S3),
    extraire_donnees(df, lesUE[5], lesEC_Stage)
  )
  
  # Ajouter des noms de colonnes clairs
  col_names_s3 <- c(
    "Validation_EXP_S3", "Somme_ECTS_EXP_S3", "Moyenne_UE_EXP_S3", "Moyenne_ACSA", "Moyenne_CHIM",
    "Moyenne_ELEC", "Moyenne_PHYS", "Moyenne_THEN",
    "Validation_FONDA_S3", "Somme_ECTS_FONDA_S3", "Moyenne_UE_FONDA_S3", "Moyenne_ALG",
    "Moyenne_ANA", "Moyenne_INFO", "Moyenne_MECA",
    "Validation_ORT_S3", "Somme_ECTS_ORT_S3", "Moyenne_UE_ORT_S3", "Moyenne_ADS",
    "Moyenne_PPI", "Moyenne_RIE", "Moyenne_STAG",
    "Validation_HUMA_S3", "Somme_ECTS_HUMA_S3", "Moyenne_UE_HUMA_S3", "Moyenne_ANGL",
    "Moyenne_COMM", "Moyenne_EPS", "Moyenne_LV2",
    "Validation_STAG_S3", "Somme_ECTS_STAG_S3", "Moyenne_UE_STAG_S3", "Moyenne_STAG"
  )
  
  # Transformer en DataFrame
  ligne_dfs3 <- as.data.frame(t(ligne_s3), stringsAsFactors = FALSE)
  
  colnames(ligne_dfs3) <- col_names_s3

  return(ligne_dfs3)
}

# Calculer les données synthétiques pour chaque UE
creer_ligne_unique_s4 <- function(df) {
  # Fonction utilitaire pour extraire les données pour une UE spécifique
  extraire_donnees <- function(df, ue_name, ecs) {
    sous_df <- df %>% filter(UE == ue_name)
    validation_ue <- unique(sous_df$Validation)
    somme_ects <- sum(sous_df$ECTS, na.rm = TRUE)
    moyenne_ue <- unique(sous_df$Moyenne_UE)
    validation_ue <- ifelse(moyenne_ue >= 10, "Valide", "Non valide")
    moyennes_ec <- sapply(ecs, function(ec) {
      moyenne_ec <- sous_df %>% filter(EC == ec) %>% pull(Moyenne)
      if (length(moyenne_ec) == 0) return(NA) else return(moyenne_ec)
    })
    return(c(validation_ue, somme_ects, moyenne_ue, moyennes_ec))
  }
  
  # Créer la ligne unique avec les données demandées
  ligne_s4 <- c(
    extraire_donnees(df, lesUE[2], lesEC_EXPS4),
    extraire_donnees(df, lesUE[1], lesEC_FONDAS4),
    extraire_donnees(df, lesUE[3], lesEC_ORTS4),
    extraire_donnees(df, lesUE[4], lesEC_HUMAS4)
  )
  
  # Ajouter des noms de colonnes clairs
  col_names_s4 <- c(
    "Validation_EXP_S4", "Somme_ECTS_EXP_S4", "Moyenne_UE_EXP_S4", "Moyenne_CHIM", "Moyenne_ELMG",
    "Moyenne_MECA", "Moyenne_ONDE", "Moyenne_PHYS", "Moyenne_SI",
    "Validation_FONDA_S4", "Somme_ECTS_FONDA_S4", "Moyenne_UE_FONDA_S4", "Moyenne_GEOM",
    "Moyenne_INFO", "Moyenne_PROBA",
    "Validation_ORT_S4", "Somme_ECTS_ORT_S4", "Moyenne_UE_ORT_S4", "Moyenne_PPI",
    "Moyenne_RIE", "Moyenne_TEDS",
    "Validation_HUMA_S4", "Somme_ECTS_HUMA_S4", "Moyenne_UE_HUMA_S4", "Moyenne_ANGL",
    "Moyenne_COMM", "Moyenne_EPS", "Moyenne_LV2"
  )
  
  # Transformer en DataFrame
  ligne_dfs4 <- as.data.frame(t(ligne_s4), stringsAsFactors = FALSE)
  
  colnames(ligne_dfs4) <- col_names_s4
  
  return(ligne_dfs4)
}