
library(shiny)
library(shinydashboard)

# Define the fields we want to save from the form
fields <- c("qtext", "randomize", "response_options")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    titlePanel("Survey Development"),
    
    sidebarLayout(
      sidebarPanel(
        tags$textarea(id="qtext", rows=5, cols=40, "Question Text Here!"),
        checkboxInput("randomize", "Randomize Response Options", FALSE),
        sliderInput("response_options", "Number of Response Options",
                    0, 25, 2, ticks = FALSE),
        actionButton("submit", "Submit")
      ),
      
      mainPanel(
        DT::dataTableOutput("responses", width = 700), tags$hr(),
        downloadButton('downloadData', 'Download QSF')
      )
    )
  ),
    
    server = function(input, output, session) {
      
      
      outputDir <- "~/Documents/git_repos/data_storage/data/"
      
      saveData <- function(data) {
        data <- t(data)
        # Create a unique file name
        fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
        # Write the file to the local system
        write.csv(
          x = data,
          file = file.path(outputDir, fileName), 
          row.names = FALSE, quote = TRUE
        )
      }
      
      loadData <- function() {
        # Read all the files into a list
        files <- list.files(outputDir, full.names = TRUE)
        data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
        # Concatenate all data together into one data.frame
        data <- do.call(rbind, data)
        data
      }
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {paste("totalform", '.csv', sep='') },
      content = function(file) {
        readr::write_csv(loadData(), file)
      }
    )
  }
)
