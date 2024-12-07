library(shiny)
library(ggplot2)
library(shinyDashboard)
rm(list=ls())


#Exemple de curseur 
ui1<- fluidPage(
  titlePanel("Ma première application Shiny"),
  sidebarLayout(sidebarPanel(sliderInput("slider","Choisissez une valeur", min=1, max=100, value=50)),mainPanel(textOutput("valueText")))
)

server1<- function(input,output){
  output$valueText<- renderText({paste("La valeur choisie est :", input$slider)})
}

# shinyApp(ui=ui1, server=server1)

#Exemple de manipulation d'un bouton

ui2<- fluidPage(titlePanel("Exemple de bouton Shiny"), sidebarLayout(sidebarPanel(actionButton("button","Cliquez-moi")),mainPanel(textOutput("text"))))

server2<- function(input,output){output$text<-renderText({"Le bouton n'a pas encore été cliqué"})
  observeEvent(input$button,{output$text<-renderText({"Vous avez cliqué sur le bouton"})})
}

shinyApp(ui=ui2,server=server2)

#Exemple d'ajout d'un graphique

ui3<-fluidPage(plotOutput("plot"))
server3<-function(input,output){
  output$plot<-renderPlot({ggplot(mtcars,aes(x=mpg,y=wt))+geom_point()+ggtitle("Graphique de MPG vs Poids")})
}

shinyApp(ui=ui3,server=server3)

#Exemple d'une boite à choix

ui4<- fluidPage(selectInput("choice","Choisissez une option", choices=c("Option 1", "Option 2", "Option 3")), textOutput("chosenText"))
server4 <- function(input,output){
  output$chosenText<-renderText({paste("Vous avez choisi:",input$choice)})
}

shinyApp(ui=ui4,server=server4)
  
