#' Downloading Bill HTML files from Congress Website
#' 
#' @description Downloading Bill Information from \href{https://www.congress.gov}{US Congress Website}.
#' 
#' @param url A \code{character} object of Bill Summary Page URL (e.g., \code{https://www.congress.gov/bill/109th-congress/house-bill/247})
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
#' @seealso \code{\link{getBill}}, \code{\link{read_scrapeBill}}, and \code{\link{write_scrapeBill}}
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
    names(resx) <- BillIDfromURL(url)
  } else {
    stop("url has length 0!")
  }
  
  return(resx)
  
}

#' Get Bill URL or HTML by Bill Number
#' 
#' @description Get Bill Suummary Page URL or \code{scrapeBill} object by Bill Number and Congress Number. 
#' 
#' @param number \code{numeric} scalar of Bill number
#' @param congress \code{numeric} scalar of Congress number
#' @param chamber Choose from \code{"House"} (default) or \code{"Senate"} 
#' @param type Choose from \code{"Bill"} (default), \code{"Simple Resolution"}, 
#' \code{"Joint Resolution"}, \code{"Concurrent Resolution"}, and 
#' \code{"Amendment"}
#' @param out The output type. \code{"URL"} (default) to export bill summary page URL 
#' and \code{"scrapeBill"} to export \code{"scrapeBill"} object.
#' @param infoDF (Only used in \code{getBillDF} function) \code{data.frame} object with 
#' columns \code{number}, \code{congress}, \code{chamber}, and \code{type}. Rows should 
#' represent individual bills.
#' @param progress (Only used in \code{getBillDF} function) If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return \code{character} scalar of URL or \code{"scrapeBill"} object.
#' 
#' @seealso \code{\link{scrapeBill}}, \code{\link{read_scrapeBill}}, and \code{\link{write_scrapeBill}}
#' 
#' @examples
#' a <- getBill(1, 103)
#' b <- scrapeBill(a)
#' collectBillID(b)
#' 
#' a <- getBill(1, 103, "Senate", "Joint Resolution")
#' b <- scrapeBill(a)
#' collectBillID(b)
#' 
#' a <- getBill(1, 101, out="scrapeBill")
#' collectBillID(a)
#' 
#' a <- data.frame(number = c(1,2,3,4),
#'                 congress = c(103,104,108,109),
#'                 chamber = "House",
#'                 type = "Bill")
#' b <- getBillDF(a, out="scrapeBill")
#' collectBillID(b)
#'                 
#' @importFrom pbapply pbapply
#' 
#' @export

getBill <- function(number,
                    congress,
                    chamber = "House",
                    type = "Bill",
                    out = "URL") {
  
  # Check Number argument
  if (length(number)!=1 | !is.numeric(number)) {
    stop("number argument invalid!")
  }

  # Congress number postfix
  pf <- "th-congress"
  if (length(grep("1$", congress))==1) pf <- "st-congress"
  if (length(grep("2$", congress))==1) pf <- "nd-congress"
  if (length(grep("3$", congress))==1) pf <- "rd-congress"
  if (length(grep("(11|12|13)$", congress))==1) pf <- "th-congress"

  # Congress part
  if (length(congress)==1 & is.numeric(congress)) {
    c <- paste0(congress, pf)
  } else {
    stop("congress argument invalid!")
  }
  
  # Chamber Name
  if (length(chamber)==1) {
    if (chamber=="House") {
      ch <- "house"
    } else if (chamber=="Senate") {
      ch <- "senate"
    } else {
      stop("invalid chamber argument!")
    }
  } else {
    stop("length of chamber is not 1!")
  }
  
  # Bill Type 
  if (length(type)==1) {
    if (type=="Bill") {
      t <- "bill"
    } else if (type=="Simple Resolution") {
      t <- "resolution"
    } else if (type=="Joint Resolution") {
      t <- "joint-resolution"
    } else if (type=="Concurrent Resolution") {
      t <- "concurrent-resolution"
    } else if (type=="Amendment") {
      t <- "amendment"
    } else {
      stop("invalid type argument!")
    }
  } else {
    stop("length of type is not 1!")
  }
  
  # Chamber and Type
  cht <- paste(ch, t, sep="-")
  
  # URL
  res <- paste("https://www.congress.gov/bill", c, cht, number, sep="/")
  # If out == scrapeBill
  if (out=="scrapeBill") res <- scrapeBill(res)
  
  return(res)
  
}

#' @rdname getBill
#' @export

getscrapeBill <- function(number,
                          congress,
                          chamber = "House",
                          type = "Bill") {
  
  getBill(number=number,
          congress=congress,
          chamber=chamber,
          type=type,
          out="scrapeBill")
  
}

#' @rdname getBill
#' @export

getBillDF <- function(infoDF,
                      out="URL",
                      progress=FALSE) {
  
  if(!is.data.frame(infoDF)) stop("infoDF is not data.frame!")
  if(!"number" %in% colnames(infoDF)) stop("number column not found!")
  if(!"congress" %in% colnames(infoDF)) stop("congress column not found!")
  if(!"chamber" %in% colnames(infoDF)) stop("chamber column not found!")
  if(!"type" %in% colnames(infoDF)) stop("type column not found!")
  
  gb <- function(idl, out) {
    getBill(number=as.numeric(idl["number"]),
            congress=as.numeric(idl["congress"]),
            chamber=as.character(idl["chamber"]),
            type=as.character(idl["type"]),
            out=out)
  }
  
  if (progress==TRUE) {
    res <- pbapply(infoDF, 1, gb, out=out)
  } else {
    res <- apply(infoDF, 1, gb, out=out)
  }
  
  return(res)
  
}

#' @rdname getBill
#' @export

getscrapeBillDF <- function(infoDF,
                            progress=TRUE) {
  
  getBillDF(infoDF=infoDF,
            out="scrapeBill",
            progress=progress)
  
}

#' Get Bill ID from URL
#' 
#' @description Get Bill ID from URL
#' 
#' @param url \code{character} scalar of vector of Bill Summary Page URL(s)
#' 
#' @return \code{character} scalar of Bill ID.
#' 
#' @importFrom stringr str_split
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#' 
#' @export

BillIDfromURL <- function(url) {
  
  BillIDfromURL1 <- function(url) {
    a <- gsub("https\\://www.congress.gov/bill/","",url)
    a <- str_split(a, "/")[[1]]
    
    cong <- as.numeric(str_extract(a[1], "^[[:digit:]]+"))
    number <- as.numeric(a[3])
    chab <- toupper(paste(str_extract(str_split(a[2], "-")[[1]], "^[[:alpha:]]"),
                          collapse=""))
    chab <- gsub("B","", chab)
    
    id <- str_c(chab, sprintf("%03.0f", cong), sprintf("%05.0f", number))
    return(id)
  }
  
  if (length(url)==1) {
    idx <- BillIDfromURL1(url)
  } else if (length(url)>1) {
    idx <- sapply(url, BillIDfromURL1)
  } else {
    stop("url has length 0!")
  }
  
  return(idx)
  
}
