library(dplyr)

dfs3_fonda <- data.frame(
  UE = c("FONDA S3"),
  EC = c("ALG","ANA","INFO","MECA"),
  Note = c(12, 14, 11, 9),
  Validation = c("Valide", "Valide", "Valide", "Non valide")
)
dfs4_fonda <- data.frame(
  UE = c("FONDA S4"),
  EC = c("Geometrie","INFO","PROBA"),
  Note = c(8, 13, 9),
  Validation = c("Non valide", "Valide", "Non valide")
)

dfs3_exp <- data.frame(
  UE = c("EXP S3"),
  EC = c("ACSA","Chimie","Electro","TP Phys","Thermo"),
  Note = c(11, 10, 9, 8, 15),
  Validation = c("Valide", "Valide", "Non valide", "Non valide", "Valide")
)
dfs4_exp <- data.frame(
  UE = c("EXP S4"),
  EC = c("Chimie","Electro","Meca","Ondes","TP Phys","SI"),
  Note = c(11, 10, 9, 8, 15, 18),
  Validation = c("Valide", "Valide", "Non valide", "Non valide", "Valide", "Valide")
)

dfs3_ort <- data.frame(
  UE = c("ORT S3"),
  EC = c("ADS","PPI","RIE","Stage","TSE"),
  Note = c(NA, NA, NA, 17.3, NA),
  Validation = c("Valide","Valide","Valide", "Valide","Valide")
)
dfs4_ort <- data.frame(
  UE = c("ORT S4"),
  EC = c("PPI","RIE","TSE"),
  Note = c(17, NA, NA),
  Validation = c("Valide","Valide","Valide")
)

df_humatc <- data.frame(
  UE = c("HUMA_TC"),
  EC = c("ANG","C&C","EPS"),
  Note = c(12, 13, 11),
  Validation = c("Valide", "Valide", "Valide")
)

df_huma <- data.frame(
  UE = c("HUMA"),
  EC = c(df_humatc$EC, "LV2"),
  Note = c(df_humatc$Note, 10),
  Validation = c(df_humatc$Validation, "Valide")
)

dfs3 <- bind_rows(df1 = I(list(dfs3_fonda)),
                 df2 = I(list(dfs3_exp)),
                 df3 = I(list(dfs3_ort)),
                 df4 = I(list(df_huma))
                 )
dfs4 <- bind_rows(df1 = I(list(dfs4_fonda)),
                  df2 = I(list(dfs4_exp)),
                  df3 = I(list(dfs4_ort)),
                  df4 = I(list(df_huma))
)

df <- bind_rows(df1 = I(list(dfs3)),
                df2 = I(list(dfs4))
                )

print (dfs3)
print (dfs4)
print (df)