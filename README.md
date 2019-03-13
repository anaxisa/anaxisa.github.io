# Description

A citation collection tool is proposed that will expand the capabilities of the Cancer Publication Portal (CPP), which will return an edge list of PMIDs uploaded by the user. The edge list can then be uploaded to a graphing software, such as Gephi, in order to visualize citation networks.

## The preferred way of running the citation tool locally is through docker:

1. Download docker from https://www.docker.com/get-started

2. Pull the docker image by running the following from your terminal or DOS prompt:

    `docker pull gdancik/shinypmc2nc`

3. Run *shiny_pmc2nc* by using the command:

    `docker run -it --rm -p 3838:3838 gdancik/shinypmc2nc`

4. View *shinypmc2nc* by opening a web browser and entering *localhost:3838* into the address bar.

## Shiny App
```R
library(shiny)
library(pmc2nc)

ARTICLE_INFO <- TRUE

articleInfoButtons <- NULL
articleResults <- NULL

if (ARTICLE_INFO) {
  articleInfoButtons <- radioButtons(
    "choices",
    "Include authors, title, publication date, and journal name of input articles in edge list?:",
    choices = c(
      "Yes" = "yes",
      "No" = "no"
    ),
    # defaults additional information button to no
    selected = "no"
  )
  
  articleResults <- column(
    4,
    h4("Article information preview:"),
    tableOutput("articleInfo")
  )
  
}

# Define UI
ui <- fluidPage(# Create title for app
  titlePanel("Citaton Collection Tool"),
  
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
      
      # Get additional information buttons
      h4(strong("Step 2: (Optional) Include additional information?"), style = "color:slategrey"),
      articleInfoButtons,
      
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
      ), 
      articleResults
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
    
    
    set_entrez_key(as.character(input$entrezK))
    
    
    # pass text input or csv input values to pmc2nc
    pmids <- get_pmc_cited_in(analyzeIDs)
    EL <- generateEdgeList(pmids)
    
    ########################################################
    artInfo <- NULL
    
    # get additional information if requested
    if(input$choices == "yes") {
      artInfo <- get_article_info(analyzeIDs)
    }
    output$articleInfo <- renderTable({
      head(artInfo)
    })
    
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
          "Number not cited or cited by articles not in PMC:"
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
```

## Results
[download TNF edge list results](https://github.com/anaxisa/anaxisa.github.io/blob/master/Edge%20Lists/TNFel2.csv) </br>
[download TP53 edge list results](https://github.com/anaxisa/anaxisa.github.io/blob/master/Edge%20Lists/tp53el.csv) </br>
[download BRCA1 edge list results](https://github.com/anaxisa/anaxisa.github.io/blob/master/Edge%20Lists/brca1el.csv) </br>
[download BRCA2 edge list results](https://github.com/anaxisa/anaxisa.github.io/blob/master/Edge%20Lists/BRCA2_full_edge_list.xlsx)

