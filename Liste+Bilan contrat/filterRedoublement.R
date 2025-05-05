# Read the EnteteJury file (replace 'path/to/your/file.xlsx' with your actual file path)
file_path <- "EnteteJury.xlsx"
data <- read_excel(file_path, col_types = "text")

#Récupérer les decisions des étudiants 
decision_vector<-as.vector(data[[18]])
codes_vector<-as.vector(data[[1]])
codes_vector<- as.integer(sub("E", "", codes_vector))
indice_redoublements_vector<-c()
for (i in 1:length(names_vector)){
  if(decision_vector[i]=="Red"){
    indice_redoublements_vector<-c(indice_redoublements_vector,codes_vector[i])
  }
}
