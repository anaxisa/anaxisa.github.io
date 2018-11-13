# Description

A citation analysis tool is proposed that will expand the capabilities of the Cancer Publication Portal (CPP), which will return centrality measures for published papers, return a visualization of the citation networks themselves, and identify network measures characterizing well-studied genes.

## **Get citations**
    # Updated script for extracting citation information - articles that cite a given article </br>
    library(httr) 
    library(xlsx)

    # get base URL for web scraping PMID citation information in XML format for one article
    baseURL <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin"

    # get brca2 PMIDs to add onto baseURL
    pmids <- unlist(read.delim("~/School/Fall 2018/Independent Study/brca2.txt", header = FALSE, sep = "\n"))
    # working with only 95 articles for now
    pmids <- pmids[1:95]

    # add personal email to baseURL
    email <- "ramosanay@my.easternct.edu"
    url_email <- paste0("&email=", email)

    # create empty dataframe to populate through loop
    d = NULL

    for(pmid in pmids) {
      # add pmid and email information onto base URL
      url_ids <- paste0("&id=", pmid, collapse = "")
      url <- paste0(baseURL, url_ids, url_email)
  
      # retrieve information from constructed url above
      res <- GET(url)
  
      # retrieve contents of URL in text format
      text <- content(res, as = "text")
  
      # get all information within the 'pubmed_pubmed_citedin' tag
      getPMIDS1 <- substring(text, regexpr("<LinkName>pubmed_pubmed_citedin</LinkName>", text), regexpr("</LinkSetDb>\n", text))
  
      # get only PMID citation information (all numerical)
      getPMIDS2 <- regmatches(getPMIDS1, gregexpr('[0-9]+', getPMIDS1))[[1]]
  
  
      if(length(getPMIDS2) > 150) {
        d <- rbind(d, data.frame(pmid=as.numeric(pmid), citing=as.numeric(getPMIDS2)))
      }
  
      Sys.sleep(1)
    }

    nodes <- data.frame(id=unique(c(d$pmid, d$citing)))
    edges <- data.frame(from = d[,2], to = d[,1])

    # write dataframe to excel spreadsheet file
    write_excel_csv(edges, "C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study/edgeList1000.csv")

## Edge List of 1000 BRCA2 articles cited more than 150 times
[download edge list](anaxisa.github.io/edgeList1000(150).csv)
      
## **Filtering in pmc2nc**
    # script for addition of option to limit requests based on availability within PMC
    library(dplyr)

    # Downloads .csv.gz file containing the titles/PMIDs of articles fully available in PMC
    download.file("ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/PMC-ids.csv.gz", destfile = "~/OneDrive/Documents/School/Fall 2018/Independent Study/limitDB.csv.gz")
    limitDB_csv <- read_csv("~/OneDrive/Documents/School/Fall 2018/Independent Study/limitDB.csv.gz")

    # Extracts PMIDs of articles available in PMC
    index.pmids <- gsub("PMID:", "", limitDB_csv$PMID)

    # testing filtering on 3 articles:
    test <- c("10655514", "9990096", "10655515")

    limit_to_pmc <-
      function(pmids) {
        # Find matches of given PMIDs
        matches <- filter(limitDB_csv, index.pmids %in% pmids)
        results <- matches$PMID
        return(results)
      }

    limit_to_pmc(test)
