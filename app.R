library(shiny)
library(DT)

# Number of years R has been around:
yrs <- \() {Sys.Date() |> format("%Y") |> as.integer()} - 1993

ui <- fluidPage(
  includeHTML("header.html"),

  fluidRow(
    # Input column:
    column(
      p(h2("Your Details:")),

      p(br()),

      textInput(
        inputId = "name",
        label = "Name"
      ),

      textInput(
        inputId = "favpkg",
        label = "Favourite R package"
      ),

      checkboxInput(
        inputId = "used_shiny",
        label = "I've built a shiny app in R before"
      ),

      numericInput(
        inputId = "n_yrs",
        label = "Number of years using R",
        value = 0,
        min = 0, max = yrs()
      ),

      selectInput(
        inputId = "os",
        label = "Operating System used most frequently",
        choices = c("Windows", "MacOS", "Linux", "Other")
      ),

      submitButton(
        text = "Submit"
      ),

      # width of this column:
      width = 4,

      offset = 1
    ),

    # output/table column:
    column(
      p(h2("Previous Responses")),
      # Download button:
      downloadButton(
        outputId = "download_table",
        label = "Download responses"
      ),

      p(br()),

      # The table:
      DT::dataTableOutput(
        outputId = "collected"
      ),

      # width of this column:
      width = 5,

      offset = 1
    )
  )
)

server <- function(input, output, session) {
  output$collected <- DT::renderDataTable({
    DT::datatable(iris, filter = "none")
  })
}

shinyApp(ui, server)
