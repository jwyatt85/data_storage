
library(shiny)
library(shinydashboard)

# Define the fields we want to save from the form
fields <- c("qtext", "MC","MA", "MT", "response_options", "block_name")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    titlePanel(title=(div("Survey Platform",
      img(
        src="mc2.jpg", height = 50, width = 50
      )))),
    
    sidebarLayout(
      sidebarPanel(
        textInput("block_name", "Block Name", "BP1"),

        
        tags$textarea(id="qtext", rows=5, cols=45, "Question Text Here!"),
        
        checkboxInput("MC", "Multiple Choice", FALSE),
        checkboxInput("MA", "Multiple Answer", FALSE),
        checkboxInput("MT", "Matrix", FALSE),
        h4("Response options comma delimited "),
        tags$textarea(id="response_options", rows=5, cols=45, "Question Text Here!"),
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
