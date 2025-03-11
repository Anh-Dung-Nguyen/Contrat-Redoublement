source("Code/cste/Constantes_MCC.R")
library(dplyr)

# -----------------   génération des notes automatiques ----------------
generer_notes_automatique <- function(UE, EC, evaluations, poids_evaluation, ects) {
  result_list <- list()

  for (i in seq_along(EC)) {
    eval_dispo <- evaluations[[EC[i]]]
    poids_dispo <- poids_evaluation[[EC[i]]]
    
    non_na_indices <- !is.na(eval_dispo)
    eval_dispo <- eval_dispo[non_na_indices]
    poids_dispo <- poids_dispo[non_na_indices]

    if (length(eval_dispo) > 0){
      notes <- sapply(eval_dispo, function(x) sample(0:20, 1))
      notes <- as.numeric(notes)
      poids_dispo <- as.numeric(poids_dispo)
      moyenne <- sum(notes * poids_dispo) / sum(poids_dispo)
      validation <- ifelse(moyenne >= 10, "Valide", "Non valide")
      df_temp <- data.frame(
        UE = UE,
        EC = EC[i],
        Moyenne = signif(moyenne, 4),
        ECTS = ects[i],
        Validation = validation
      )
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

    result_list[[i]] <- df_temp
  }

  df_final <- do.call(rbind, lapply(result_list, function(x) {
    as.data.frame(t(x), stringsAsFactors = FALSE)
  }))

  return(df_final)
}