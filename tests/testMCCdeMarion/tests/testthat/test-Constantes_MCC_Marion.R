library(testthat)
test_that("Les cases dans EC Algèbre 3", {
  UE_Algebre3<-LesUE[LesUE$EC=="Algèbre 3","UE"]
  ECTS_Algebre3<-LesUE[LesUE$EC=="Algèbre 3","ECTS"]
  Type_Algebre3<-LesUE[LesUE$EC=="Algèbre 3","TypeEC"]
  Eva1_Algebre3<-LesUE[LesUE$EC=="Algèbre 3","Évaluation 1"]
  expect_equal(UE_Algebre3,"Sciences fondamentales (UE-STP03-SF)")
  expect_equal(Type_Algebre3,"Note")
  expect_equal(Eva1_Algebre3,list("DS1"))
  expect_equal(ECTS_Algebre3,2.0)
})

test_that("Les cases dans EC ADS", {
  UE_ADS<-LesUE[LesUE$EC=="ADS","UE"]
  ECTS_ADS<-LesUE[LesUE$EC=="ADS","ECTS"]
  Type_ADS<-LesUE[LesUE$EC=="ADS","TypeEC"]
  Eva2_ADS<-LesUE[LesUE$EC=="ADS","Évaluation 2"]
  expect_equal(UE_ADS,"Orientation et Transition (UE-STP03-ORT)")
  expect_equal(Type_ADS,"Validation")
  expect_equal(Eva2_ADS,list(NA))
  expect_equal(ECTS_ADS,0.0)
})

test_that("Les cases dans EC Anglais 4", {
  UE_Anglais4<-LesUE[LesUE$EC=="Anglais 4","UE"]
  ECTS_Anglais4<-LesUE[LesUE$EC=="Anglais 4","ECTS"]
  Type_Anglais4<-LesUE[LesUE$EC=="Anglais 4","TypeEC"]
  Eva2_Anglais4<-LesUE[LesUE$EC=="Anglais 4","Évaluation 2"]
  expect_equal(UE_Anglais4,"Humanité (UE-STP04-ENS / ENS-FIRE-FR / ENS-FIRE-NFR)")
  expect_equal(Type_Anglais4,"Note")
  expect_equal(Eva2_Anglais4,list("CC"))
  expect_equal(ECTS_Anglais4,1.5)
})

