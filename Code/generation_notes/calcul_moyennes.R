# Fichier : calcul_moyennes.R

library(dplyr)

# Étape 1 : Calcul de la moyenne de chaque EC
LesUE_notes <- LesUE_notes %>%
  rowwise() %>%
  mutate(
    Moyenne_EC = {
      notes <- c_across(`Évaluation 1`:`Évaluation 3`)
      notes_num <- suppressWarnings(as.numeric(notes))
      mean(notes_num, na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Étape 2 : Calcul de la moyenne de chaque UE (pondérée par ECTS)
# Important : convertir ECTS et Moyenne_EC en numérique
LesUE_notes$ECTS <- as.numeric(LesUE_notes$ECTS)
LesUE_notes$Moyenne_EC <- as.numeric(LesUE_notes$Moyenne_EC)

# Calcul pondéré par UE
moyennes_UE <- LesUE_notes %>%
  group_by(ID, UE) %>%
  summarise(Moyenne_UE = weighted.mean(Moyenne_EC, ECTS, na.rm = TRUE), .groups = "drop")

# Étape 3 : Jointure pour inclure la colonne Moyenne_UE dans LesUE_notes
LesUE_notes <- LesUE_notes %>%
  left_join(moyennes_UE, by = c("ID", "UE"))

# Résultat : LesUE_notes contient désormais Moyenne_EC + Moyenne_UE

LesUE_notes <- LesUE_notes %>%
  mutate(
    Résultat = case_when(
      is.na(Moyenne_UE) ~ "Non défini",
      Moyenne_UE >= 10 ~ "Valide",
      TRUE ~ "Non valide"
    )
  )
