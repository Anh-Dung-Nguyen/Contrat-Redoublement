source("~/Nguyen Anh Dung/Project/Etude Pratique/Contrat-Redoublement/Code/cste/Constantes_MCC.R")
library(dplyr)

noms <- c("Martin", "Bernard", "Thomas", "Petit", "Robert", "Richard", "Durand", "Dubois", "Moreau", "Laurent", "Simon", "Michel", "Garcia", "David", "Bertrand", "Roux", "Vincent", "Fournier", "Leroy", "Morin", "Roger", "Fontaine", "Garnier", "Moulin", "Chevalier")
prenoms <- c("Emma", "Nathan", "Lucas", "Chloé", "Léo", "Manon", "Gabriel", "Jade", "Arthur", "Léa", "Raphaël", "Inès", "Louis", "Sarah", "Hugo", "Camille", "Jules", "Alice", "Adam", "Louise", "Paul", "Lina", "Noah", "Zoé", "Sacha")

# -----------------   génération des notes automatiques ----------------
generer_notes_aleatoires <- function(tableau) {
  # Définir les évaluations pour lesquelles on veut générer une note
  evaluations_valides <- c("DS", "DS1", "DS2", "CC", "TP", "FI", "IO")
  
  # Copier le tableau pour éviter d’écraser les données d’origine
  tableau_resultat <- tableau
  
  # Pour chaque ligne du tableau
  for (i in 1:nrow(tableau_resultat)) {
    for (col in c("Évaluation 1", "Évaluation 2", "Évaluation 3")) {
      eval <- tableau_resultat[i, col]
      # Si c’est une évaluation valide, on génère une note aléatoire
      if (!is.na(eval) && eval %in% evaluations_valides) {
        note <- round(runif(1, 0, 20), 2)  # 2 décimales
        tableau_resultat[i, col] <- as.character(note)
      }
    }
  }
  
  return(tableau_resultat)
}

# Générer 100 étudiants avec données
LesUE_notes <- bind_rows(lapply(1:100, function(i) {
  tableau_etudiant <- generer_notes_aleatoires(LesUE)
  tableau_etudiant$ID <- sprintf("E%03d", i)
  tableau_etudiant$Nom <- sample(noms, 1)
  tableau_etudiant$Prénom <- sample(prenoms, 1)
  return(tableau_etudiant)
}))

# Réorganiser les colonnes : ID, Nom, Prénom d'abord
LesUE_notes <- LesUE_notes %>%
  select(ID, Nom, Prénom, everything())