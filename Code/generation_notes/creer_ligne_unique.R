# Extraction des données S3 de LesUE_notes
library(dplyr)

# Fonction utilitaire pour filtrer et extraire les valeurs
extraire_s3 <- function(df_etud) {
  # Filtrage UE par semestre S3 (chaque nom contient STP03)
  df_S3 <- df_etud %>% filter(grepl("UE-STP03", UE))
  
  # Fonction pour extraire informations d'une UE
  extract_ue <- function(df, nom_ue, ec_order) {
    data <- df %>% filter(UE == nom_ue)
    res <- unique(data$Résultat)
    ects <- sum(data$ECTS, na.rm = TRUE)
    moy_ue <- unique(data$Moyenne_UE)
    moy_ecs <- sapply(ec_order, function(x) {
      m <- data %>% filter(CodeEC == x) %>% pull(Moyenne_EC)
      if (length(m) == 0) return(NA) else return(m)
    })
    return(c(res, ects, moy_ue, moy_ecs))
  }
  
  # Ordre demandé
  ligne <- c(
    extract_ue(df_S3, "Sciences expérimentales (UE-STP03-SE)", c("EC-STP03-ACSA","EC-STP03-CHIM","EC-STP03-ELEC","EC-STP03-PHYS","EC-STP03-THEN")),
    extract_ue(df_S3, "Sciences fondamentales (UE-STP03-SF)", c("EC-STP03-ALG","EC-STP03-ANA","EC-STP03-INFO","EC-STP03-MECA")),
    extract_ue(df_S3, "Stage (UE-STP03-STAG)", c("EC-STP03-STAG")),
    extract_ue(df_S3, "Orientation et Transition (UE-STP03-ORT)", c("EC-STP03-ADS","EC-STP03-PPI","EC-STP03-RIE","EC-STP03-TEDS")),
    extract_ue(df_S3, "Humanité (UE-STP03-ENS / ENS-FIRE-FR / ENS-FIRE-NFR)", c("EC-STP03-ANGL","EC-STP03-COMM\nEC-STP03-FLE-COMM","EC-STP03-EPS","EC-STP03-LV2\nEC-STP03-FLE"))
  )
  
  # Nom des colonnes dans le bon ordre
  noms_colonnes <- c(
    "Res_EXP", "ECTS_EXP", "Moy_EXP", "Moy_ACSA", "Moy_Chimie", "Moy_Electro", "Moy_TP", "Moy_Thermo",
    "Res_FONDA", "ECTS_FONDA", "Moy_FONDA", "Moy_Alg", "Moy_Ana", "Moy_Info", "Moy_Meca",
    "Res_STAGE", "ECTS_STAGE", "Moy_STAGE", "Moy_Stage",
    "Res_ORT", "ECTS_ORT", "Moy_ORT", "Moy_ADS", "Moy_PPI", "Moy_RIE", "Moy_TEDS",
    "Res_HUMA", "ECTS_HUMA", "Moy_HUMA", "Moy_Ang", "Moy_Comm", "Moy_EPS", "Moy_LV2"
  )
  
  result <- as.data.frame(t(ligne), stringsAsFactors = FALSE)
  colnames(result) <- noms_colonnes
  
  result$ID <- df_etud$ID[1]
  result$Nom <- df_etud$Nom[1]
  result$Prénom <- df_etud$Prénom[1]
  
  return(result)
}

extraire_s4 <- function(df_etud) {
  # Filtrage UE par semestre S4 (chaque nom contient STP04)
  df_S4 <- df_etud %>% filter(grepl("UE-STP04", UE))
  
  # Fonction pour extraire informations d'une UE
  extract_ue <- function(df, nom_ue, ec_order) {
    data <- df %>% filter(UE == nom_ue)
    res <- unique(data$Résultat)
    ects <- sum(data$ECTS, na.rm = TRUE)
    moy_ue <- unique(data$Moyenne_UE)
    moy_ecs <- sapply(ec_order, function(x) {
      m <- data %>% filter(CodeEC == x) %>% pull(Moyenne_EC)
      if (length(m) == 0) return(NA) else return(m)
    })
    return(c(res, ects, moy_ue, moy_ecs))
  }
  
  # Ordre demandé
  ligne <- c(
    extract_ue(df_S4, "Sciences expérimentales (UE-STP04-SE)", c("EC-STP04-CHIM","EC-STP04-ELMG","EC-STP04-MECA","EC-STP04-ONDE","EC-STP04-PHYS","EC-STP04-SI")),
    extract_ue(df_S4, "Sciences fondamentales (UE-STP04-SF)", c("EC-STP04-GEOM","EC-STP04-INFO","EC-STP04-PROBA")),
    extract_ue(df_S4, "Orientation et Transition (UE-STP04-ORT)", c("EC-STP04-PPI","EC-STP04-RIE","EC-STP04-TEDS")),
    extract_ue(df_S4, "Humanité (UE-STP04-ENS / ENS-FIRE-FR / ENS-FIRE-NFR)", c("EC-STP04-ANGL","EC-STP04-COMM\nEC-STP04-FLE-COMM","EC-STP04-EPS","EC-STP04-LV2\nEC-STP04-FLE"))
  )
  
  # Nom des colonnes dans le bon ordre
  noms_colonnes <- c(
    "Res_EXP", "ECTS_EXP", "Moy_EXP", "Moy_Chimie", "Moy_ELMG", "Moy_MECA", "Moy_Onde", "Moy_PHYS", "Moy_SI",
    "Res_FONDA", "ECTS_FONDA", "Moy_FONDA", "Moy_GEOM", "Moy_INFO", "Moy_PROBA",
    "Res_ORT", "ECTS_ORT", "Moy_ORT", "Moy_PPI", "Moy_RIE", "Moy_TEDS",
    "Res_HUMA", "ECTS_HUMA", "Moy_HUMA", "Moy_Ang", "Moy_Comm", "Moy_EPS", "Moy_LV2"
  )
  
  result <- as.data.frame(t(ligne), stringsAsFactors = FALSE)
  colnames(result) <- noms_colonnes
  
  result$ID <- df_etud$ID[1]
  result$Nom <- df_etud$Nom[1]
  result$Prénom <- df_etud$Prénom[1]
  
  return(result)
}

# Extraire les lignes, chaque ligne contient les résultats et moyennes pour le semestre 3
df_list_s3 <- split(LesUE_notes, LesUE_notes$ID)
resumes_s3 <- bind_rows(lapply(df_list_s3, extraire_s3)) %>%
  select(ID, Nom, Prénom, everything())  # Mettre ID en tête

# Extraire les lignes, chaque ligne contient les résultats et moyennes pour le semestre 4
df_list_s4 <- split(LesUE_notes, LesUE_notes$ID)
resumes_s4 <- bind_rows(lapply(df_list_s4, extraire_s4)) %>%
  select(ID, Nom, Prénom, everything())  # Mettre ID en tête