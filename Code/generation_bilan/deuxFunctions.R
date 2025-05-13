#s'il y a Error in read_docx() : argument "path" is missing, with no default, executer:install.packages("officer")
listeContratFunction<-function(){
  source("./Code/generation_bilan/listeContratAlgo.R")
}

bilanFunction<-function(){
  source("./Code/generation_bilan/bilanAlgo.R")
}
listeContratFunction()
bilanFunction()

