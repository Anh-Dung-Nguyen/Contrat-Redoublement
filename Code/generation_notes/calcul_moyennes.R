source("Code/cste/Constantes_MCC.R")
source("Code/generation_notes/generer_notes_automatique.R")
library(dplyr)

compensation_intra_UE <- function(df) {
  df <- df %>%
    group_by(LesUE) %>%
    mutate(
      Moyenne_UE = signif(sum(Moyenne * ECTS, na.rm = TRUE) / sum(ECTS * !is.na(Moyenne), na.rm = TRUE), 4),
      Validation = ifelse(Moyenne_UE >= 10 & Validation == "Non valide", "ValideComp", Validation)
    ) %>%
    ungroup()
  return(df)
}

compensation_inter_UE <- function(df, ue_scientifique) {
  df_scientifique <- df %>% 
    filter(LesUE %in% ue_scientifique)

    moyenne_ponderee_ue <- df_scientifique %>%
      group_by(LesUE) %>%
        summarise(
          Moyenne_UE = mean(Moyenne, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        summarise(
          Moyenne_globale = mean(Moyenne_UE, na.rm = TRUE)
        ) %>%
    pull(Moyenne_globale)

  if (moyenne_ponderee_ue >= 10) {
    df <- df %>%
      mutate(
        Validation = ifelse(UE %in% ue_scientifique & Validation == "Non valide", "ValideComp", Validation)
      )
  }
  return(df)
}