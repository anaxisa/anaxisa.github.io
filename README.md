# Description

A citation analysis tool is proposed that will expand the capabilities of the Cancer Publication Portal (CPP), which will return centrality measures for published papers, return a visualization of the citation networks themselves, and identify network measures characterizing well-studied genes.

## Shiny App
```R
library(shiny)
library(pmc2nc)

setwd("C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study")

# Define UI
ui <- fluidPage(# Create title for app
  titlePanel("Citation Analysis Tool"),
  
  sidebarLayout(
    # Panel for user input
    sidebarPanel(
      # Input: Entrez key (if applicable)
      textInput("entrezK", "Input NCBI API key:"),
      helpText(
        "Note: The NCBI API key is not required, but will make searching faster.",
        "For more information, please visit",
        tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK25497/", "NCBI.")
      ),
      
      # Input: PMID text list
      textInput("txt", "Input list of PMIDs:"),
      helpText(
        "Note: Please ensure that PMIDs are comma-seperated, followed by a space.",
        "Example: 123, 124"
      ),
      
      # Input: CSV file - FIX server code
      fileInput(
        "file1",
        "Choose CSV file:",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/plain")
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Action button to submit
      actionButton("action", "Submit")
    ),
    
    # Main panel for downloading results
    mainPanel(
      tableOutput("resultsum"),
      
      # Horizontal line
      tags$hr(),
      
      h4("Edge list preview:"),
      tableOutput("esum"),
      
      # Horizontal line
      tags$hr(),
      
      h4("Summary of most cited articles:"),
      tableOutput("topCitations"),
      
      # Horizontal line
      tags$hr(),
      
      downloadButton("downloadData", "Download full results")
    )
  ))

# Define server logic
server <- function(input, output) {
  # Code below only runs when 'submit' button is selected
  observeEvent(input$action, {
    
    # if there is no csv file upload, run text analysis
    if (is.null(input$file1)) {
      # reads in text input
      txtFile <- readLines(textConnection(input$txt))
      
      # seperates text input by commas
      txtFile <- trimws(unlist(strsplit(txtFile, ",")))
      
      # removes non-unique and null values
      txtFile <- unique(txtFile)
      txtFile1 <- txtFile[sapply(txtFile, nchar) > 0]
      
      # checks length
      if (length(txtFile1) != 0) {
        # convert to numeric data type to pass to pmc2nc
        analyzeIDs <- as.numeric(txtFile1)
      }
    } else {
      # Define csv input
      uploadFile <- read.csv(input$file1$datapath)
      analyzeIDs <- unique(uploadFile[[1]])
    }
    
    # pass text input or csv input values to pmc2nc
    set_entrez_key(input$entrezK)
    pmids <- get_pmc_cited_in(analyzeIDs)
    EL <- generateEdgeList(pmids)
    
    ########################################################
    # Output - first 6 PMIDs of Edge List shown (for testing)
    output$esum <- renderTable({
      head(EL)
    })
    
    ########################################################
    # get results for summary
    res1 <- length(analyzeIDs)
    res2 <- length(unique(unlist(EL[2])))
    res3 <- res1 - res2
    
    res <-
      data.frame(
        "Summary" = c(
          "Number of articles in user list:",
          "Number cited by articles in PMC:",
          "Number not cited or not in PMC:"),
        "Count" = c(res1, res2, res3)
      )
    
    # Output - number of articles with/without citations from original input
    output$resultsum <- renderTable(res)
    
    ########################################################
    # get results for citation count -
    # convert edge list to data frame in order to construct table
    resl <- as.data.frame(getCitationCounts(EL))
    
    output$topCitations <- renderTable(head(resl[order(resl$n, decreasing = TRUE),]))
    
    ########################################################
    # allows user to download edge list
    # (!) FIX: file name function
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("data-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(EL, file, row.names = FALSE)
      }
    )
  })
}
```

# Run the app
shinyApp(ui = ui, server = server)

## Results
[download TNF results](https://github.com/anaxisa/anaxisa.github.io/blob/master/TNFRES.csv) </br>
[download TP53 results](https://github.com/anaxisa/anaxisa.github.io/blob/master/TP53RES.csv) </br>
[download BRCA1 results](https://github.com/anaxisa/anaxisa.github.io/blob/master/brca1RES.csv) </br>
[download BRCA2 results](https://github.com/anaxisa/anaxisa.github.io/blob/master/brca2RES.csv)

