if (!require("openxlsx")) install.packages("openxlsx")

library(openxlsx)

wb <- loadWorkbook("/home/nguyen-anh-dung/jury.xlsx")

sheet_name <- "S3"
existing_data <- read.xlsx(wb, sheet = sheet_name)

for (i in 1:nrow(ligne_unique_total)) {
  writeData(wb, sheet = sheet_name, x = ligne_unique_total[i, ], startRow = i + 3, startCol = 3, colNames = FALSE, rowNames = FALSE)
}

saveWorkbook(wb, "jury.xlsx", overwrite = TRUE)

print(head(ligne_unique_total))