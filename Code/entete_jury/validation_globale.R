valider_etudiant_par_semestre <- function(df_notes, semestre = "03") {
  pattern <- paste0("UE-STP", semestre)
  
  df_validation <- df_notes %>%
    filter(grepl(pattern, UE)) %>%
    select(ID, UE, Moyenne_UE, ECTS) %>%
    distinct() %>%
    mutate(
      Type = case_when(
        grepl(paste0("UE-STP", semestre, "-SF"), UE) ~ "FONDA",
        grepl(paste0("UE-STP", semestre, "-SE"), UE) ~ "EXP",
        grepl(paste0("UE-STP", semestre, "-ENS"), UE) ~ "HUMA",
        grepl(paste0("UE-STP", semestre, "-ORT"), UE) ~ "ORT",
        grepl(paste0("UE-STP", semestre, "-STAG"), UE) ~ "STAG",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Type)) %>%
    group_by(ID) %>%
    summarise(
      Moy_HUMA  = weighted.mean(Moyenne_UE[Type == "HUMA"], ECTS[Type == "HUMA"], na.rm = TRUE),
      Moy_ORT   = weighted.mean(Moyenne_UE[Type == "ORT"], ECTS[Type == "ORT"], na.rm = TRUE),
      Moy_STAG  = if ("STAG" %in% Type) weighted.mean(Moyenne_UE[Type == "STAG"], ECTS[Type == "STAG"], na.rm = TRUE) else NA_real_,
      Moy_FONDA = weighted.mean(Moyenne_UE[Type == "FONDA"], ECTS[Type == "FONDA"], na.rm = TRUE),
      Moy_EXP   = weighted.mean(Moyenne_UE[Type == "EXP"], ECTS[Type == "EXP"], na.rm = TRUE),
      ECTS_FONDA = sum(ECTS[Type == "FONDA"], na.rm = TRUE),
      ECTS_EXP   = sum(ECTS[Type == "EXP"], na.rm = TRUE),
      Moy_FonExp = ((Moy_FONDA * ECTS_FONDA) + (Moy_EXP * ECTS_EXP)) / (ECTS_FONDA + ECTS_EXP),
      Valide = case_when(
        semestre == "03" ~ ifelse(Moy_HUMA >= 10 & Moy_ORT >= 10 & Moy_STAG >= 10 & Moy_FonExp >= 10, "Valide", "Non valide"),
        semestre == "04" ~ ifelse(Moy_HUMA >= 10 & Moy_ORT >= 10 & Moy_FonExp >= 10, "Valide", "Non valide")
      ),
      .groups = "drop"
    )
  
  return(df_validation)
}

validation_globale_s3 <- valider_etudiant_par_semestre(LesUE_notes, "03")
validation_globale_s4 <- valider_etudiant_par_semestre(LesUE_notes, "04")