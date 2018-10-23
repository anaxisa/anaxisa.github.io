# Script for extracting information information from files - update to go through each file
#library(stringr)
library(readr)
library(dplyr)
library(xml2)

# specify directory for tar files
tarDir <- "C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study/tar/"
# specify directory for extracted files
dir <- "C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study/PMC/"
# specify directory for results (citation information)
resultsDir <- "C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study/txt/"

# Download .txt file of Open Access Subset -- in other file!

# Create table containing downloaded files from Open Access Subset
testfile <- read_delim("~/School/Fall 2018/Independent Study/testfile.txt",
                       "\t", escape_double = FALSE, col_names = FALSE,
                       locale = locale(), trim_ws = TRUE, skip = 1)

# Extracts PMIDs of Pubmed Open Access Subset
index.pmids <- gsub("PMID:", "", testfile$X4)

# (!) Given PMIDs - update to more general format
pmids <- read.delim("~/School/Fall 2018/Independent Study/brca2.txt", header = FALSE)

# Find matches of given PMIDs and those in table
matches <- filter(testfile, index.pmids %in% pmids$V1)
results <- matches$X1

# Download XML data of results -- in other file!

# Extract XML citation information of matched files
pmcFiles <- substring(results, regexpr("PMC", results), regexpr("..gz", results))

# function to check for XML file, otherwise return
extractXML <- function(x) {
  # Get content names of tar files, but does not extract contents
  f <- untar(tarFile, list = TRUE, exdir = dir)
  
  # looping through contents of f to get XML file
  for(object in f) {
    if(grepl("xml$", object, fixed = FALSE) == TRUE) {
      untar(tarFile, list = FALSE, exdir = dir)
      
      l <- read_xml(paste0(dir, object))
      
      # Get PMID of orginal article (for file name in preliminary "database") - returns as character vector
      nameA <- xml_find_all(l, "//article-id")
      for(name in nameA) {
        if(xml_attrs(name) == "pmid") {
          pmidArt <- paste0(xml_text(xml_contents(name)), ".txt")
          break
        }
      }
      
      # (!) Get all PMIDs of cited article - do above so it only gives PMIDs
      find2 <- xml_contents(xml_find_all(l, "//pub-id"))
      
      # Seperate citation PMIDs by a comma
      findUpd <- unlist(paste(find2, collapse = ", "))
      
      # Outputs citation PMIDs to txt file, named as the PMID of the original article 
      write(findUpd, file = paste0(resultsDir, pmidArt))
    }
  }
}

for(pmc in pmcFiles) {
  # Get path for tar files
  tarFile <- paste0(tarDir, pmc)
  
  extractXML(tarFile)
}



