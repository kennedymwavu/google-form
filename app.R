library(shiny)
library(shinyjs)
library(shinyvalidate)
library(DT)
library(digest)
library(aws.s3)

# Number of years R has been around:
yrs <- \() {Sys.Date() |> format("%Y") |> as.integer()} - 1993

# Mandatory fields:
mandatory_fields <- c("name", "favpkg", "os")


# ----------------------------------------------------------------------

# Show mandatory_fields in the UI by adding a red star to them;

# First define the needed CSS:
mandatory_star <- ".mand_star { color: red;}"

# We'll add that CSS to the app by calling shinyjs::inlineCSS(mandatory_star)
# in the UI

# Now define a function to add a star after a mandatory label:
label_mandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mand_star")
  )
}

# To use that we'll wrap each mandatory `label` in `label_mandatory()`

# ------------------------------------------------------------------------
# Storage of user responses:
# ------------------------------------------------------------------------

# ------- <-- Set up aws.s3 --> ------
# *********  My details  *************
s3_bucket_name <- "**"
acc_key_id <- "**"
sec_acc_key <- "**"
region <- "us-east-2"
Sys.setenv("AWS_ACCESS_KEY_ID" = acc_key_id,
           "AWS_SECRET_ACCESS_KEY" = sec_acc_key,
           "AWS_DEFAULT_REGION" = region)
# ------------------------------------

# Define the fields to save:
fields <- c("name", "favpkg", "used_shiny", "n_yrs", "os")


# Function to save additional data:
saveData <- function(data) {
  current <- data |> t() |> as.data.frame()
  current$timestamp <- Sys.time() |> as.character()

  # Create a plain text representation of the data:
  plain_text <- paste0(
    paste(names(current), collapse = ","), "\n",
    paste(unname(current), collapse = ",")
  )

  file_name <- paste0(
    paste(
      Sys.time() |> as.integer(),
      digest(plain_text, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )

  # Upload the file to s3:
  put_object(file = charToRaw(plain_text), object = file_name,
             bucket = s3_bucket_name)
}

# ------------------------------------------------------------------------

# Function to load the saved data:
loadData <- function() {
  # Get a list of all files:
  file_names <- get_bucket(s3_bucket_name) |>
    lapply(FUN = \(x) x[["Key"]]) |> rev()



  file_list <- lapply(file_names, FUN = \(x) {
    x |> get_object(s3_bucket_name) |>
      readBin(what = "character") |> {
        \(x) read.csv(text = x, stringsAsFactors = FALSE)
      }()
  }
  )

  # concatenate into one data.frame:
  file_df <- do.call(rbind, file_list)

  # Row names are unnecessary, remove them:
  rownames(file_df) <- NULL

  file_df
}

# ------------------------------------------------------------------------


ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(mandatory_star),

  # Include the header html created:
  includeHTML("header.html"),


  fluidRow(
    # Input column:
    column(
      div(id = "form",
          p(h2("Your Details:")),

          p(br()),

          textInput(
            inputId = "name",
            label = "Name" |> label_mandatory()
          ),

          textInput(
            inputId = "favpkg",
            label = "Favourite R package" |> label_mandatory()
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
            label = "Operating System used most frequently" |>
              label_mandatory(),
            choices = c("", "Windows", "MacOS", "Linux", "Other")
          ),

          # Submit button:
          actionButton(
            inputId = "submit",
            label = "Submit",
            class = "btn-success"
          )
          ),

      # Thank you section:
      shinyjs::hidden(
        div(id = "thanks_msg",
            h3("Thanks, your response was submitted successfully!"),
            actionLink(
              inputId = "submit_another",
              label = "Submit another reponse"
            )
            )
      ),

      # width of this column:
      width = 3,

      offset = 1
    ),

    # output/table column:
    column(
      p(h2("Previous Responses")),
      # Download button:
      downloadButton(
        outputId = "download_table",
        label = "Download responses",
        class = "btn-success"
      ),

      p(br()),

      # The table:
      DT::dataTableOutput(
        outputId = "previous"
      ),

      # width of this column:
      width = 6,

      offset = 1
    )
  )
)

# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  # validate input fields; os;
  # 1. Create an InputValidator object:
  iv <- InputValidator$new()

  # 2. Add validation rules:
  iv$add_rule("name", sv_required())
  iv$add_rule("favpkg", sv_required())
  iv$add_rule("os", \(value) {
    if (value == "") {
      "Required"
    }
  }
  )

  # 3. Start displaying errors in the UI:
  iv$enable()

  # Whenever a field is filled, aggregate the form data:
  formData <- reactive({
    # 4. Do not proceed if any input is invalid:
    req(iv$is_valid())

    # k <<- sapply(X = fields, FUN = \(x) input[[x]])
    sapply(X = fields, FUN = \(x) input[[x]])
  })

  # When the submit button is clicked:
  observeEvent(
    eventExpr = {
      input$submit
    },
    handlerExpr = {
      # save the data:
      saveData(formData())

      # Reset the form:
      shinyjs::reset(id = "form")

      # Hide the form:
      shinyjs::hide(id = "form")

      shinyjs::show(id = "thanks_msg")
    }
  )

  # Observer for `submit another response`:
  observeEvent(
    eventExpr = input$submit_another,
    handlerExpr = {
      shinyjs::show(id = "form")
      shinyjs::hide(id = "thanks_msg")
    }
  )

  # Show previous submissions; Update with current response when the
  # submit button is clicked:
  output$previous <- DT::renderDataTable({
    input$submit
    loadData()
  },
  colnames = c("Name", "Favourite R Package", "Used shiny before",
               "Years of using R", "Operating System", "timestamp")
  )

  observe({
    # function to check if all mandatory fields have a value:
    mand_condition <- function() {
      mandatory_fields |>
        vapply(
          FUN = \(x) !is.null(input[[x]]) && input[[x]] != "",
          FUN.VALUE = logical(1)
        ) |>
        all()
    }

    # Enable/Disable the submit button:
    shinyjs::toggleState(
      id = "submit",
      condition = mand_condition()
    )}
  )


  # Download button:
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("responses-", Sys.Date(), ".csv")
    },

    content = function(file) {
      write.csv(
        loadData(), file, row.names = FALSE
      )
    }
  )
}

shinyApp(ui, server)
