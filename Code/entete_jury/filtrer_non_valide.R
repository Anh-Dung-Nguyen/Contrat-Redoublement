library(tidyr)

lister_etudiants_non_valides <- function(df_s3, df_s4, df_notes) {
  df_s3 <- df_s3 %>% select(ID, Valide_S3 = Valide)
  df_s4 <- df_s4 %>% select(ID, Valide_S4 = Valide)
  
  noms_prenoms <- df_notes %>%
    distinct(ID, Nom, Prénom)
  
  df_merged <- full_join(df_s3, df_s4, by = "ID") %>%
    mutate(
      Valide_S3 = ifelse(is.na(Valide_S3), "Non valide", Valide_S3),
      Valide_S4 = ifelse(is.na(Valide_S4), "Non valide", Valide_S4),
      Valide_Annee = ifelse(Valide_S3 == "Valide" & Valide_S4 == "Valide", "Valide", "Non valide")
    ) %>%
    filter(Valide_Annee == "Non valide") %>%
    left_join(noms_prenoms, by = "ID") %>%
    select(ID, Nom, Prénom, Valide_S3, Valide_S4, Valide_Annee)
  
  return(df_merged)
}

non_valides <- lister_etudiants_non_valides(
  validation_globale_s3,
  validation_globale_s4,
  LesUE_notes
)

head(non_valides)