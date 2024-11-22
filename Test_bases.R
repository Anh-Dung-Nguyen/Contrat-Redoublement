num<-4L
bool<-TRUE

#Manipulation d'un vector
rm(list=ls())
vect<-c(1,2,3,4,5)
print(vect)
vect[2]=0
print(vect)
vect[c(-2)]=4
print(length(vect))


#Manipulation d'un data frame
data<-data.frame(
  Noms=c("Nono","Toto","Loulou"),
  Age=c(12,45,63),
  Taille=c(152,172,165)
)
print(data)

data[2,2]=17
print(data)
data$Age[3]=14
print(data)
print(names(data))
print(data$Noms)
data<-rbind(data,c("Dudu",25,178))
print(data)
data<-cbind(data,Sexe=c("f","h","f","h"))
print(data)
data$Sexe<-NULL
print(data)
data<-data[-2,]
print(data)
data<-data[data$Noms!="Nono",]
print(data)