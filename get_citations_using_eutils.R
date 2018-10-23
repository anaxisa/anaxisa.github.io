# Updated script for extracting citation information
library(httr)

baseURL <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pmc_refs"


email <- "ramosanay@my.easternct.edu"
pmids <- c(9365162, 21876761)

url_email <- paste0("&email=", email)


# start with 95 pmids

# for each pmid in the above list,


#     get up to 10 pmids at a time
#     extract citations and write to file



url_ids <- paste0("&id=", pmids, collapse = "")
url <- paste0(baseURL, url_ids, url_email)

res <- GET(url)
Sys.sleep(1)

text <- content(res, as = "text")
View(text)
# pmcFiles <- substring(text, regexpr("Id", text), regexpr("/Id", text))
# l <- strsplit(text, "[</Id>\n\t\t\t</Link>\n ]")
# matches <- regmatches(text, gregexpr("[[:digit:]]+", text))

getPMID1 <- substring(text, regexpr("<DbFrom>pubmed</DbFrom>\n", text), regexpr("</Id>\n", text))
getPMID2 <- regmatches(getPMID1, gregexpr('[0-9]+', getPMID1))[[1]]

getCitations1 <- substring(text, regexpr("<LinkName>pubmed_pmc_refs</LinkName>\n", text), regexpr("</LinkSetDb>\n", text))
getCitations2 <- regmatches(getCitations1, gregexpr('[0-9]+', getCitations1))[[1]]
