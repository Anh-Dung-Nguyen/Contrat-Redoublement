library(testthat)
list_ue<-c("FONDA S3","EXP S3","ORT S3","HUMA S3","FONDA S4","EXP S4","ORT S4","HUMA S4")
#Test si la moyenne de chaque ec de chaque ue est bien calculée ou pas
test_moyenne_chaque_ec_de_ue <-function(ue){
  test_that("Moyenne_chaque_ec_de_ue",{

    #Récuperer les tableaux pour les test et ses coeffs de chaque ec de chaque ue
    if(grepl("HUMA S3",ue)){
      ue_no_space<-gsub(" S3","",ue)
    }else if(grepl("HUMA S4",ue)){
      ue_no_space<-gsub(" S4","",ue)
    }else{
      ue_no_space<-gsub(" ","",ue)
    }
    ue_with_underscore<-gsub(" ","_",ue)
    table_notes_name <- paste0("lesNotesEC_", ue_no_space)
    table_poids_name <- paste0("poids_",ue_with_underscore)
    lesNotesEC_ue <- get(table_notes_name)
    poids_ue <- get(table_poids_name)


    for (ec in names(lesNotesEC_ue)){
      tests <- lesNotesEC_ue[[ec]]   # Get test names into the vector tests
      coefficients <- poids_ue[[ec]]  # Get coefficients into the vector coefficients

      #Get rid of the NA values in the vector tests
      valid_tests <- !is.na(tests)
      tests <- tests[valid_tests]

      #Get the score of each ec
      scores<-vector()
      for(test in tests){
        scores<-c(scores,LesUE[LesUE$UE == ue & LesUE$EC == ec, test])
      }

      scores<-as.numeric(scores)
      print(scores)

      #The average that AD has calculated
      real_average<-LesUE[LesUE$UE == ue & LesUE$EC == ec, "Moyenne"]
      real_average<-as.numeric(real_average)

      #The average that I calculated
      calculated_average<-sum(scores * coefficients, na.rm = TRUE) / sum(coefficients)

      #Compare the two average
      expect_equal(real_average,calculated_average)


    }

  })

}
for(ue in list_ue){
  test_moyenne_chaque_ec_de_ue(ue)

}
