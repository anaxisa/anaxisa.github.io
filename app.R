library(shiny)
library(pmc2nc)

setwd("C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study")

# Define UI
ui <- fluidPage(# Create title for app
  titlePanel("Citation Analysis Tool"),
  
  sidebarLayout(
    # Panel for user input
    sidebarPanel(
      h4(strong("Step 1: Enter PMIDs using method (A) or (B)"), style = "color:slategrey"),
      
      # Input: PMID text list
      textAreaInput("txt", "(A) Paste/input list of PMIDs:", height = "150px"),
      helpText(
        "Note: PMIDs can be input as a comma-seperated list OR one PMID can be listed per line."
      ),
      
      h4(strong("-OR-"), align = "center", style = "color:red"),
      
      # Input: CSV file - FIX server code
      fileInput(
        "file1",
        "(B) Upload CSV file:",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/plain")
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Input: checkbox for additional return of results
      h4(strong("Step 2: Additional information"), style = "color:slategrey"),
      checkboxGroupInput(
        "SorT",
        "Return information from Source or Target?:",
        c("Source" = "source",
          "Target" = "target")
      ),
      checkboxGroupInput(
        "choices",
        "Include following in edge list:",
        c(
          "Authors" = "authors",
          "Title" = "title",
          "Publication Date" = "pubdate",
          "Journal Name" = "jname"
        )
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Entrez key (if applicable)
      h4(strong("Step 3: (Optional)"), style = "color:slategrey"),
      textInput("entrezK", "Input NCBI API key:"),
      helpText(
        "Note: The NCBI API key is not required, but will make searching faster.",
        "For more information, please visit",
        tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK25497/", "NCBI.")
      ),
      
      tags$hr(),
      
      # Input: Action button to submit
      h4(strong("Step 4: Submit!"), style = "color:slategrey"),
      actionButton("action", "Submit")
    ),
    
    # Main panel for downloading results
    mainPanel(
      column(
        4,
        tableOutput("resultsum"),
        tags$hr(),
        h4("Summary of most cited articles:"),
        tableOutput("topCitations")
      ),
      column(
        4,
        h4("Edge list preview:"),
        tableOutput("esum"),
        downloadButton("downloadData", "Download full results")
      )
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
    
    
    set_entrez_key(input$entrezK)
    
    print(paste(input$choices, collapse = ", "))
    
    # pass text input or csv input values to pmc2nc
    pmids <- get_pmc_cited_in(analyzeIDs, filter = "title")
    EL <- generateEdgeList(pmids$res[[1]])
    
    ########################################################
    # Output - first 6 PMIDs of Edge List shown
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
          "Number not cited or not in PMC:"
        ),
        "Count" = c(res1, res2, res3)
      )
    
    # Output - number of articles with/without citations from original input
    output$resultsum <- renderTable(res)
    
    ########################################################
    # get results for citation count -
    # convert edge list to data frame in order to construct table
    resl <- as.data.frame(getCitationCounts(EL))
    colnames(resl) <- c("PMIDs", "Count")
    
    output$topCitations <-
      renderTable(head(resl[order(resl$Count, decreasing = TRUE), ]))
    
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


# Run the app
shinyApp(ui = ui, server = server)
