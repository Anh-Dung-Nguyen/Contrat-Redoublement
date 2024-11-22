library(shiny)
library(shinydashboard)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Menu à gauche"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Notes", tabName = "notes", icon = icon("bar-chart")),
      menuItem("Etudiants", tabName = "etudiants", icon = icon("user-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Contenu de l'onglet Accueil
      tabItem(tabName = "home",
              h2("Bienvenue sur la page d'accueil")
      ),
      # Contenu de l'onglet notes
      tabItem(tabName = "notes",
              h2("Analyse des données"),
      ),
      # Contenu de l'onglet Etudiants
      tabItem(tabName = "etudiants",
              h2("Liste des étudiants"),
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  output$examplePlot <- renderPlot({
    plot(cars)
  })
}

# Lancer l'application
shinyApp(ui, server)
