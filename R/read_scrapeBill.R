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
  
  #read
  if (is.list(x)) {
    x <- Map(read_html, x)
  } else {
    x <- read_html(x)
  }
  
  x
}

#' @rdname read_scrapeBill
#' @export
write_scrapeBill = function(x, file, ...) {
  #convert to character
  #is list?
  if (is.list(x)) {
    x = Map(as.character, x)
  } else {
    x = Map(as.character, x)
  }
  
  #save
  saveRDS(x, file = file, ...)
}
