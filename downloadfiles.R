# Script for downloading data for one file

# Downloads .txt file containing the names of the .tar.gx files of articles in the Pubmed Open Access Subset
download.file("ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_file_list.txt", destfile = "C:/Users/airam/OneDrive/Documents/School/Fall 2018/Independent Study/testfile.txt")

# Iterate through result list to extract XML information, and the citation information of each article
for(result in results) {
  getFileURL <- paste0("ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/", result)
  fileNameDL <- paste0(tarDir, substring(result, regexpr("PMC", result), regexpr("..tar", result)), ".tar")
  articleDL <- download.file(getFileURL, destfile = fileNameDL)
}
