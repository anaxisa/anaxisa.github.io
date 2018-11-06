# Updated script for extracting citation information - articles that cite a given article
library(httr) 
library(xlsx)
library(visNetwork)

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

vis.nodes <- nodes
vis.edges <- edges

vis.nodes$shape <- "dot"
vis.nodes$shadow <- TRUE
vis.nodes$title <- vis.nodes$id 
vis.nodes$borderWidth <- 2
vis.nodes$color.border <- "black"
vis.nodes$color.hightlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"
visNetwork(vis.nodes, vis.edges)

# write dataframe to excel spreadsheet file
write_excel_csv(edges, "C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study/edgeList1000.csv")

# create simple network - (!) use a different network function for more complex graphs
simpleNetwork(d)



