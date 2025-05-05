source("contrat_notes.R")
library(readxl)

# Read the Excel file (replace 'path/to/your/file.xlsx' with your actual file path)
file_path <- "jury.xlsx"
data <- read_excel(file_path, sheet = 3, col_types = "text")
data_cleaned <- data[-c(1:2), ]

#Récupérer les noms et prennoms des étudiants
names_vector <- as.vector(data_cleaned[[2]])   # Second column (Names)
surnames_vector <- as.vector(data_cleaned[[3]]) # Third column (Surnames)
names_surnames_vector <- c()
for (i in 1:length(names_vector)){
  name_surname<-paste0(names_vector[i],"_",surnames_vector[i])
  names_surnames_vector <-c(names_surnames_vector,name_surname)
}
names_surnames_vector_unique <- ave(
  names_surnames_vector,
  names_surnames_vector,
  FUN = function(x) {
    if (length(x) == 1) return(x)
    paste0(x, "_", seq_along(x))
  }
)





