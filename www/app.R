library(shiny)
library(shinythemes)

createThemedNavbar <- function() {
  ui <- fluidPage(
    selectInput("theme", "Select Theme:",
                choices = c("Default", "Cerulean", "Cosmo", "Flatly", "Journal",
                            "Lumen", "Paper", "Sandstone", "Simplex", "Spacelab",
                            "Superhero", "United", "Yeti"),
                selected = "Default"),
    tags$head(
      tags$link(rel = "stylesheet", id = "theme-css", 
                href = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/bootstrap.min.css")
    ),
    navbarPage(
      "Themed Navbar",
      tabPanel("Tab 1", "Content for Tab 1"),
      tabPanel("Tab 2", "Content for Tab 2")
    )
  )
  
  server <- function(input, output, session) {
    observeEvent(input$theme, {
      theme <- switch(input$theme,
                      "Default" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/bootstrap.min.css",
                      "Cerulean" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/cerulean/bootstrap.min.css",
                      "Cosmo" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/cosmo/bootstrap.min.css",
                      "Flatly" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/flatly/bootstrap.min.css",
                      "Journal" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/journal/bootstrap.min.css",
                      "Lumen" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/lumen/bootstrap.min.css",
                      "Paper" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/paper/bootstrap.min.css",
                      "Sandstone" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/sandstone/bootstrap.min.css",
                      "Simplex" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/simplex/bootstrap.min.css",
                      "Spacelab" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/spacelab/bootstrap.min.css",
                      "Superhero" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/superhero/bootstrap.min.css",
                      "United" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/united/bootstrap.min.css",
                      "Yeti" = "https://maxcdn.bootstrapcdn.com/bootswatch/4.5.2/yeti/bootstrap.min.css")
      
      session$sendCustomMessage(type = "updateTheme", message = theme)
    })
  }
  
  shinyApp(ui, server)
}

# Run the function to create the themed navbarPage Shiny app
createThemedNavbar()
