#' Downloading Bill HTML files from Congress Website
#' 
#' @description Downloading Bill Information from \href{https://www.congress.gov}{US Congress Website}.
#' 
#' @param url A \code{character} object of Bill URL (e.g., \code{https://www.congress.gov/bill/109th-congress/house-bill/247})
#' @param progress If \code{TRUE}(default), show progress bar for the processing of multiple bills.
#' 
#' @return A (or a list of) \code{scrapeBill} object(s) that contains two \code{xml_document} objects.
#' \itemize{
#'   \item \code{info}: contains meta information of the bill
#'   \item \code{text}: contains full texts of the bill
#' }
#' 
#' @examples
#' tgturl <- "https://www.congress.gov/bill/109th-congress/house-bill/247"
#' bill247 <- scrapeBill(tgturl)
#' 
#' @importFrom xml2 read_html
#' @importFrom pbapply pblapply
#' @importFrom stringr str_c
#' 
#' @export

scrapeBill <- function(url, progress=TRUE) {
  
  scrapeBill1 <- function(url) {
    
    # Meta Information of the Bill
    info <- try(read_html(str_c(url,"/all-info")))
    if (class(info)[1]=="try-error") info <- try(read_html(str_c(url,"/all-info")))
    if (class(info)[1]=="try-error") info <- try(read_html(str_c(url,"/all-info")))
    
    # Texts of the Bill
    text <- try(read_html(str_c(url,"/text")))
    if (class(text)[1]=="try-error") text <- try(read_html(str_c(url,"/text")))
    if (class(text)[1]=="try-error") text <- try(read_html(str_c(url,"/text")))
    
    # Export Results
    res <- list(info = info, text = text, url = url)
    class(res) <- "scrapeBill"
    
    return(res)
  }
  
  # Possibly Applied for Vector
  if (length(url)==1) {
    resx <- scrapeBill1(url)
  } else if (length(url)>1) {
    if (progress==TRUE) {
      resx <- pblapply(url, scrapeBill1)
    } else {
      resx <- lapply(url, scrapeBill1)
    }
  } else {
    stop("url has length 0!")
  }
  
  return(resx)
  
}

