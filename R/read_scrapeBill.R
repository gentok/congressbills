#' Read and write \code{scrapeBill} Object
#' 
#' @description Write (especially a list of) \code{scrapeBill} object. 
#' 
#' @references \url{https://stackoverflow.com/questions/37703689/cannot-save-load-xml-document-generated-from-rvest-in-r} 
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param file A connection or the name of the file (use \code{.rds} extension).
#' @param ... Additional argument passed to \code{\link[base]{saveRDS}}
#' 
#' @return For \code{read_scrapeBill}, an \code{R} object. For \code{write_scrapeBill}, \code{NULL} invisibly.
#' 
#' @importFrom xml2 read_html
#' 
#' @export
read_scrapeBill = function(file) {

  #load from file
  x = readRDS(file)
  
  readF <- function(k) {
    k$info <- read_html(k$info)
    k$text <- read_html(k$text)
    return(k)
  }
  
  # Read
  if (class(x)[1]=="scrapeBill") {
    x <- readF(x)
    class(x) <- "scrapeBill"
  } else if (class(x)[1]=="list" & class(x[[1]])=="scrapeBill") {
    x <- lapply(x, readF)
    for (i in 1:length(x)) class(x[[i]]) <- "scrapeBill"
  } else {
    stop("invalid object type.")
  }
  
  return(x)
}

#' @rdname read_scrapeBill
#' @export
write_scrapeBill = function(x, file, ...) {
  
  #convert to character
  if (class(x)[1]=="scrapeBill") {
    x <- lapply(x, as.character)
    class(x) <- "scrapeBill"
  } else if (class(x)[1]=="list" & class(x[[1]])=="scrapeBill") {
    x <- lapply(x, function(k) lapply(k, as.character))
    for (i in 1:length(x)) class(x[[i]]) <- "scrapeBill"
  } else {
    stop("invalid object type.")
  }

  #save
  saveRDS(x, file = file, ...)
}
