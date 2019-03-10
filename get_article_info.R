#' Get article information
#'
#' `get_article_info` takes a vector of article PMIDs and returns summary 
#' information about the articles
#' 
#' @details
#' Returns summary information for a list of articlees
#' This function is a wrapper for the `entrez_summary` and 
#' 'extract_from_esummary' functions from `rentrez`. 
#' Function is currently limited to handling no more than 5000 PMIDs. 
#' The "web history" feature is used to post IDs to the 
#' server, 200 at a time, and retreive information 10 IDs at a time.
#' 
#' @param pmids a vector of PMIDs look-up 
#' @param elements a vector indicating the article info to retrieve, defaulting to c("uid", "title", "lastauthor", "fulljournalname")
#' @param show_progress outputs progress if TRUE
#' @return a data.frame containing article information for each PMID
#'
#' @examples
#' res <- get_article_info(c(21876726,21876761))

#' @export
get_article_info <-function(pmids, elements = NULL, show_progress = TRUE) {
  if (is.null(elements)) {
      elements <- c("uid", "title", "lastauthor", "fulljournalname")
  }
  pmids <- unique(pmids)
  if (length(pmids) > 5000) {
    stop("can only process up to 5000 unique pmids for now")
  }
  wait <- 0.34
  if (Sys.getenv("ENTREZ_KEY") != "") {
    print("using ENTREZ_KEY")
    wait <- 0.11
  }
  res <- NULL
  
  start <- 1
  n <- length(pmids)
  upload <- NULL
  batchSize <- 200
  retmax <- 10
  
  # post batchSize then get retmax at a time
  batchNum = 1
  
  withProgress(message = 'Getting Additional Information.', detail = 'This may take a while...', value = 0, {
  while (start <= n) {
    end <- min(start+batchSize-1, n)
    upload <- entrez_post(db = "pubmed", id = pmids[start:end])
    batchSize <- end - start + 1
    start <- end + 1
    if (show_progress) {
        cat("Getting article info for batch #", batchNum, "\n")
        pb <- progress_bar$new(total = ceiling(batchSize/retmax))
        incProgress(1 / (ceiling(n/batchSize)))
        batchNum <- batchNum + 1
    }
    for (st in seq(0, batchSize-1, by = retmax)) {
      e <- entrez_summary(db = "pubmed", web_history = upload, 
                          retstart = st, retmax = retmax, always_return_list = TRUE)
      if (show_progress) pb$tick()
      r <- extract_from_esummary(e, elements, FALSE)
      res <- rbind(res, do.call(rbind.data.frame, r))
      Sys.sleep(wait)
    }
    
  }
  })
  return(res)
}

