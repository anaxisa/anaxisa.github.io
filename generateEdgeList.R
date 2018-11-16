#' Constructs edge lists from elink citation results.
#'
#' `generateEdgeList` generates the edge list for results from `get_pmc_cited_in` 

#' @details
#' `elink` must be one of the following: a list containing one elink object, 
#' with list name corresponding to target PMID; a list of elink objects,
#' with names corresponding to target PMIDs, or a list of any combination
#' of the above. These corresond to a single PMID, a single batch,
#' and multiple batches, respectively.
#' 
#' @param elink elink results, as obtained from `get_pmc_cited_in` (see details)
#' @return An edge list (data.frame) with one column for target PMIDS and one column for source PMIDS
#' @seealso \code{\link{get_pmc_cited_in}} for obtaining elink citation results
#' 
#' @examples
#' 
#' # generate an edge list for a single article
#' res1 <- get_pmc_cited_in(21876761)
#' e2 <- generateEdgeList(res1)
#'
#' # generate an edge list for multiple articles
#' res2 <- get_pmc_cited_in(c(21876761, 311,29463753), batchSize = 2)
#' e2 <- generateEdgeList(res2)

#' @export
generateEdgeList <- function(elink) {
  if (length(elink) == 1 || "elink_list" %in% class(elink)) {
      return(generateEdgeList1(elink))
  } 
  s <- lapply(elink, generateEdgeList1)
  data.frame(do.call("rbind", s), stringsAsFactors = FALSE)
}

generateEdgeList1 <- function(elink) {
  
  ff <- function(i,x) {
    if (is.null(x[[i]]$links$pubmed_pubmed_citedin)) {
      return(NULL)
    }
    cbind(Source = x[[i]]$links$pubmed_pubmed_citedin,Target = names(x)[i])
  }
  
  s <- lapply(1:length(elink), ff, x = elink)
  data.frame(do.call("rbind", s), stringsAsFactors = FALSE)
}

