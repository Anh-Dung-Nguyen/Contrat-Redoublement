library(testthat)
test_that("test FONDA3", {
  #Test la moyenne de algebre 3
  Moy_alg3<-LesUE[LesUE$EC=="ALG","Moyenne"]
  expect_true(Moy_alg3>=0.0&&Moy_alg3<=20.0)

  #Test la moyenne de analyse 3
  Moy_ana3<-LesUE[LesUE$EC=="ANA","Moyenne"]
  expect_true(Moy_ana3>=0.0&&Moy_ana3<=20.0)

  #Test la moyenne de info 3
  indice_info3 <- which(LesUE$EC == "INFO")[1]
  Moy_info3<-LesUE[indice_info3,"Moyenne"]
  expect_true(Moy_info3>=0.0&&Moy_info3<=20.0)

  #Test la moyenne de meca 3
  indice_meca3 <- which(LesUE$EC == "MECA")[1]
  Moy_meca3<-LesUE[indice_meca3,"Moyenne"]
  expect_true(Moy_meca3>=0.0&&Moy_meca3<=20.0)

  ECTS_alg3<-LesUE[LesUE$EC=="ALG","ECTS"]
  ECTS_ana3<-LesUE[LesUE$EC=="ANA","ECTS"]
  ECTS_info3<-LesUE[indice_info3,"ECTS"]
  ECTS_meca3<-LesUE[indice_meca3,"ECTS"]

  #Test la moyenne de FONDA3
  moy_fonda3_expected<-as.integer((Moy_ana3*ECTS_ana3+Moy_alg3*ECTS_alg3+Moy_meca3*ECTS_meca3+Moy_info3*ECTS_info3)/(ECTS_meca3+ECTS_info3+ECTS_ana3+ECTS_alg3))
  moy_fonda3_reel<-as.integer(LesUE[which(LesUE$UE == "FONDA S3")[1],"Moyenne_UE"])
  expect_equal(moy_fonda3_expected,moy_fonda3_reel)

  #Test l'attribute "Validation" de chaque EC
  val_alg3<-as.character(LesUE[LesUE$EC=="ALG","Validation"])
  val_ana3<-as.character(LesUE[LesUE$EC=="ANA","Validation"])
  val_info3<-as.character(LesUE[indice_info3,"Validation"])
  val_meca3<-as.character(LesUE[indice_meca3,"Validation"])

  if(moy_fonda3_reel>=10){ #Si on valide le UE




    if(Moy_alg3>=10){ #test pour algebre3
      expect_equal(val_alg3,"Valide")
    } else{
      expect_equal(val_alg3,"ValideComp")
    }

    if(Moy_ana3>=10){#test pour analyse3
      expect_equal(val_ana3,"Valide")
    } else{
      expect_equal(val_ana3,"ValideComp")
    }

    if(Moy_info3>=10){#test pour info3
      expect_equal(val_info3,"Valide")
    } else{
      expect_equal(val_info3,"ValideComp")
    }

    if(Moy_meca3>=10){#test pour meca3
      expect_equal(val_meca3,"Valide")
    } else{
      expect_equal(val_meca3,"ValideComp")
    }
  } else{

    if(Moy_alg3>=10){ #test pour algebre3
      expect_equal(val_alg3,"Valide")
    } else{
      expect_equal(val_alg3,"Non valide")
    }

    if(Moy_ana3>=10){#test pour analyse3
      expect_equal(val_ana3,"Valide")
    } else{
      expect_equal(val_ana3,"Non valide")
    }

    if(Moy_info3>=10){#test pour info3
      expect_equal(val_info3,"Valide")
    } else{
      expect_equal(val_info3,"Non valide")
    }

    if(Moy_meca3>=10){#test pour meca3
      expect_equal(val_meca3,"Valide")
    } else{
      expect_equal(val_meca3,"Non valide")
    }
  }


})
