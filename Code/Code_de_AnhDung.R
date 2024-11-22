library(dplyr)

# UE -> Nom de l'Unité d'Enseignement
# EC -> Vecteur contenant les noms des Elements Constitutifs de l'UE
# evaluations -> Liste des évaluation disponibles pour chaque UE comme DS1, DS2, CC
# poids_evaluation -> liste des coefficients aux chaque évaluation de chaque EC
# ects -> Vecteur contenant les crédits ECTS de chaque EC
generer_notes_automatique <- function(UE, EC, evaluations, poids_evaluation, ects) {
  # result_list -> stocker les données relatives à chaque EC
  result_list <- list()

  # parcours de chaque EC
  for (i in seq_along(EC)) {
    # récupérer des évaluations disponibles et leurs poids
    eval_dispo <- evaluations[[EC[i]]]
    poids_dispo <- poids_evaluation[[EC[i]]]

    # Si des évaluations disponibles
    if (length(eval_dispo) > 0){
      # générer de notes aléatoires comprises entre 0 et 20 à l'aide de sapply()
      notes <- sapply(eval_dispo, function(x) sample(0:20, 1))
      # calcul de la moyenne pondérée
      moyenne <- sum(notes * poids_dispo) / sum(poids_dispo)
      # déterminer de la validation
      validation <- ifelse(moyenne >= 10, "Valide", "Non valide")
      # créer un df temporaire avec des colonnes suivantes: UE, EC, Moyenne, ECTS, Validation
      df_temp <- data.frame(
        UE = UE,
        EC = EC[i],
        Moyenne = moyenne,
        ECTS = ects[i],
        Validation = validation
      )
      # concaténation des résultats en un df unique avec cbind
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

    # ajouter le df_temp à la liste result_list
    result_list[[i]] <- df_temp
  }

  # combiner en un seul tableau
  df_final <- bind_rows(result_list)
  return (df_final)
}

# df -> un tableau avec les colonnes: UE, Moyenne, ECTS, Validation
compensation_intra_UE <- function(df){
  # regrouper par UE
  df <- df %>%
    group_by(UE) %>%
    # mutate -> créer des nouvelles colonnes
    mutate(
      # na.rm ici pour remplacer des valeurs NA par 0
      # ECTS[!is.na(Moyenne) -> tenir compte seulement avec des valeurs ECTS où la valeur moyenne n'est pas NA
      Moyenne_UE = sum(Moyenne * ECTS, na.rm = TRUE) / sum(ECTS[!is.na(Moyenne)], na.rm = TRUE),
      Validation = ifelse(Moyenne_UE >= 10 & Validation == "Non valide", "ValideComp", Validation)
    ) %>%
    ungroup()
  return (df)
}

compensation_inter_UE <- function(df, ue_scientifique){
  # isoler les lignes correspondant aux UE scientifiques spécifiées dans ue_scientifique
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
    # extraction la valeur de moyenne globale
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
EC_FONDA_S3 <- c("ALG","ANAL","INFO","MECA")
evaluations_FONDA_S3 <- list(
  ALG = c("DS1", "DS2"),
  ANAL = c("DS1", "DS2"),
  INFO = c("DS1", "DS2"),
  MECA = c("DS1", "DS2", "CC")
)

poids_FONDA_S3 <- list(
  ALG = c(1.5, 1.5),
  ANAL = c(1.5, 1.5),
  INFO = c(1, 1),
  MECA = c(1, 3, 1)
)

ECTS_FONDA_S3 <- c(2, 3, 2, 2)

dfs3_fonda <- generer_notes_automatique("FONDA S3", EC_FONDA_S3, evaluations_FONDA_S3, poids_FONDA_S3, ECTS_FONDA_S3)

EC_EXP_S3 <- c("ACSA","CHIM","ELEC","PHYS","THEN")
evaluations_EXP_S3 <- list(
  ACSA = c("DS2", "TP"),
  CHIM = c("DS1", "DS2","TP"),
  ELEC = c("DS2", "CC"),
  PHYS = c("TP"),
  THEN = c("DS2")
)

poids_EXP_S3 <- list(
  ACSA = c(5, 3),
  CHIM = c(1, 1, 1),
  ELEC = c(2, 1),
  PHYS = c(2, 1),
  THEN = c(1)
)

ECTS_EXP_S3 = c(3, 3.5, 1.5, 1.5, 1.5)

dfs3_exp <- generer_notes_automatique("EXP S3", EC_EXP_S3, evaluations_EXP_S3, poids_EXP_S3, ECTS_EXP_S3)

EC_ORT_S3 <- c("ADS","PPI","RIE","STAG","TSE")
evaluations_ORT_S3 <- list(
  ADS = c(),
  PPI = c(),
  RIE = c(),
  STAG = c("DS2"),
  TSE = c("DS2")
)

poids_ORT_S3 <- list(
  ADS = c(),
  PPI = c(),
  RIE = c(),
  STAG = c(1),
  TSE = c(1)
)

ECTS_ORT_S3 = c(0, 0, 0, 4, 1)

dfs3_ort <- generer_notes_automatique("ORT S3", EC_ORT_S3, evaluations_ORT_S3, poids_ORT_S3, ECTS_ORT_S3)

EC_HUMA_S3 <- c("ANGL","COMM","EPS", "LV2")
evaluations_HUMA_S3 <- list(
  ANGL = c("DS2", "CC"),
  COMM = c("DS2"),
  EPS = c("DS2"),
  LV2 = c("DS2")
)

poids_HUMA_S3 <- list(
  ANGL = c(1.5, 1.5),
  COMM = c(1),
  EPS = c(1),
  LV2 = c(1)
)

ECTS_HUMA_S3 = c(1.5, 1.5, 1, 1)

dfs3_huma <- generer_notes_automatique("HUMA S3", EC_HUMA_S3, evaluations_HUMA_S3, poids_HUMA_S3, ECTS_HUMA_S3)

# EC pour S4
EC_FONDA_S4 <- c("GEOM","INFO","PROBA")
evaluations_FONDA_S4 <- list(
  GEOM = c("DS1", "DS2"),
  INFO = c("DS2", "CC"),
  PROBA = c("DS1", "DS2")
)

poids_FONDA_S4 <- list(
  GEOM = c(1.5, 1.5),
  INFO = c(2, 1),
  PROBA = c(1.5, 1.5)
)

ECTS_FONDA_S4 <- c(3.5, 3, 3.5)

dfs4_fonda <- generer_notes_automatique("FONDA S4", EC_FONDA_S4, evaluations_FONDA_S4, poids_FONDA_S4, ECTS_FONDA_S4)

EC_EXP_S4 <- c("CHIM","ELMG","MECA","ONDE","PHYS","SI")
evaluations_EXP_S4 <- list(
  CHIM = c("DS2", "TP"),
  ELMG = c("DS2", "CC"),
  MECA = c("DS2", "CC"),
  ONDE = c("DS2"),
  PHYS = c("TP"),
  SI = c("TP")
)

poids_EXP_S4 <- list(
  CHIM = c(2, 1),
  ELGM = c(2, 1),
  MECA = c(1, 1),
  ONDE = c(1),
  PHYS = c(1),
  SI = c(1)
)

ECTS_EXP_S4 <- c(3.5, 2, 2, 2, 1.5, 1)

dfs4_exp <- generer_notes_automatique("EXP S4", EC_EXP_S4, evaluations_EXP_S4, poids_EXP_S4, ECTS_EXP_S4)

EC_ORT_S4 <- c("PPI","RIE","TSE")
evaluations_ORT_S4 <- list(
  PPI = c("DS2"),
  RIE = c("DS2"),
  TSE = c()
)

poids_ORT_S4 <- list(
  PPI = c(1),
  RIE = c(1),
  TSE = c()
)

ECTS_ORT_S4 = c(1, 1, 0)

dfs4_ort <- generer_notes_automatique("ORT S4", EC_ORT_S4, evaluations_ORT_S4, poids_ORT_S4, ECTS_ORT_S4)

EC_HUMA_S4 <- c("ANGL","COMM","EPS","LV2")
evaluations_HUMA_S4 <- list(
  ANGL = c("DS2", "CC"),
  COMM = c("DS2"),
  EPS = c("DS2"),
  LV2 = c("DS2")
)

poids_HUMA_S4 <- list(
  ANGL = c(1.5, 1.5),
  COMM = c(1),
  EPS = c(1),
  LV2 = c(1)
)

ECTS_HUMA_S4 = c(1.5, 1.5, 1, 1)

dfs4_huma <- generer_notes_automatique("HUMA S4", EC_HUMA_S4, evaluations_HUMA_S4, poids_HUMA_S4, ECTS_HUMA_S4)

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
