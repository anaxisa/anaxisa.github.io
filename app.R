library(shiny)
library(pmc2nc)


setwd("C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study")

# Define UI
ui <- fluidPage(
  
  # Create title for app
  titlePanel("Citation Analysis Tool"),
  
  sidebarLayout(
    
    # Panel for user input
    sidebarPanel(
      
      # Input: Entrez key (if applicable)
      textInput("entrezK", "Input NCBI API key:"),
      helpText("Note: The NCBI API key is not required, but will make searching faster.",
               "For more information, please visit",
              tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK25497/", "NCBI.")),
      
      # Input: PMID text list
      textInput("txt", "Input list of PMIDs:"),
      helpText("Note: Please ensure that PMIDs are comma-seperated, followed by a space.",
                "Example: 123, 124"),
      
      # Input: CSV file - FIX server code
      fileInput("file1", "Choose CSV file:",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/plain"
                           )),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Action button to submit
      actionButton("action", "Submit")
    ),
    
    # Main panel for downloading results
    mainPanel(
      h3("Head of list:"),
      tableOutput("summary"),
      
      # Horizontal line
      tags$hr(),
      
      downloadButton("downloadData", "Download results")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Code below only runs when 'submit' button is pressed
  # (!) Update so it takes API key as argument
  observeEvent(input$action, {
    
    # Define text/csv input
    txtFile <- input$txt
    uploadFile <- input$file1
    
    # Get edge list for text input if field is not empty
    # (!) FIX: update so it queries database first
    if(!is.null(txtFile)) {
      
      # update txtFile value if > 1 (as long as it is seperated by commas)
      if(grepl(",", txtFile)) {
        txtFile <- as.numeric(unlist(strsplit(txtFile, ", ")))
      }
      
      pmids <- get_pmc_cited_in(txtFile)
      el <- generateEdgeList(pmids)
    }
    
    # (!) FIX: update so csv can be analyzed
#    if(!is.null(uploadFile)) {
#      read.csv(uploadFile)
#    }
    
    # Output - first 6 PMIDs of Edge List shown (for testing)
    output$summary <- renderTable({
      head(el)
    })
    
    
    # allows user to download edge list
    # (!) FIX: file name function
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("el-", Sys.Date(),".csv")
      },
      content = function(file) {
        write.csv(el, file, row.names = FALSE)
      }      
    )
  })
}


# Run the app
shinyApp(ui = ui, server = server)
