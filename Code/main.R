source("Contrat-Redoublement/Code/cste/Constantes_MCC.R")
source("Contrat-Redoublement/Code/generation_notes/compensation.R")
source("Contrat-Redoublement/Code/generation_notes/generer_notes_automatique.R")
source("Contrat-Redoublement/Code/generation_notes/creer_ligne_unique.R")
source("Contrat-Redoublement/Code/generation_notes/write_data_to_sheet.R")
library(dplyr)

main <- function() {
    ligne_unique_totale_s3 <- list()
    ligne_unique_totale_s4 <- list()

    for (i in 1:25){
        resultat_FONDA_S3 <- generer_notes_automatique(lesUE[1], lesEC_FONDAS3, lesNotesEC_FONDAS3, poids_FONDA_S3, lesECTSFONDAS3)
        resultat_FONDA_S4 <- generer_notes_automatique(lesUE[1], lesEC_FONDAS4, lesNotesEC_FONDAS4, poids_FONDA_S4, lesECTSFONDAS4)
        resultat_EXP_S3 <- generer_notes_automatique(lesUE[2], lesEC_EXPS3, lesNotesEC_EXPS3, poids_EXP_S3, lesECTSEXPS3)
        resultat_EXP_S4 <- generer_notes_automatique(lesUE[2], lesEC_EXPS4, lesNotesEC_EXPS4, poids_EXP_S4, lesECTSEXPS4)
        resultat_ORT_S3 <- generer_notes_automatique(lesUE[3], lesEC_ORTS3, lesNotesEC_ORTS3, poids_ORT_S3, lesECTSORTS3)
        resultat_ORT_S4 <- generer_notes_automatique(lesUE[3], lesEC_ORTS4, lesNotesEC_ORTS4, poids_ORT_S4, lesECTSORTS4)
        resultat_HUMA_S3 <- generer_notes_automatique(lesUE[4], lesEC_HUMA_S3, lesNotesEC_HUMA, poids_HUMA_S3, lesECTSHUMAS3)
        resultat_HUMA_S4 <- generer_notes_automatique(lesUE[4], lesEC_HUMA_S4, lesNotesEC_HUMA, poids_HUMA_S4, lesECTSHUMAS4)
        resultat_STAG_S3 <- generer_notes_automatique(lesUE[5], lesEC_Stage, lesNotesEC_Stage, poids_STAG_S3, lesECTSStage)
        
        dfs3 <- bind_rows(resultat_FONDA_S3, resultat_EXP_S3, resultat_ORT_S3, resultat_HUMA_S3)
        dfs3 <- compensation_intra_UE(dfs3)
        dfs3 <- compensation_inter_UE(dfs3, ue_scientifique = c(lesUE[1], lesUE[2]))
        
        dfs4 <- bind_rows(resultat_FONDA_S4, resultat_EXP_S4, resultat_ORT_S4, resultat_HUMA_S4)
        dfs4 <- compensation_intra_UE(dfs4)
        dfs4 <- compensation_inter_UE(dfs4, ue_scientifique = c(lesUE[1], lesUE[2]))
        
        LesUE <- bind_rows(dfs3, dfs4)
        
        ligne_unique_totale_s3[[i]] <- creer_ligne_unique_s3(dfs3)
        ligne_unique_totale_s4[[i]] <- creer_ligne_unique_s4(dfs4)
    }

    ligne_unique_totale_s3 <- bind_rows(ligne_unique_totale_s3)
    ligne_unique_totale_s4 <- bind_rows(ligne_unique_totale_s4)

    write_data_to_sheet("/home/nguyen-anh-dung/jury.xlsx", "S3", ligne_unique_totale_s3)
    write_data_to_sheet("/home/nguyen-anh-dung/jury.xlsx", "S4", ligne_unique_totale_s4)
}

main()