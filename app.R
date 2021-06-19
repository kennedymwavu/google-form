library(shiny)

ui <- fluidPage(
  includeHTML("header.html")
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
