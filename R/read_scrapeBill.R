#' Read and write \code{scrapeBill} Object
#' 
#' @description Write (especially a list of) \code{scrapeBill} object. 
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param file A connection or the name of the file (use \code{.rds} extension).
#' @param aux Whether to export auxiliary files into separate folder when \code{write_scrapeBill}.
#' If \code{"None"} (default), do not export any auxiliary file. If \code{"raw"}, export
#' raw serialized XML vectors. If \code{"html"}, export HTML files.   
#' Set to \code{"raw"} or \code{"html"} if you want to avoid the \code{.rds} file 
#' size to become too big 
#' (Although the auxiliary files folder size tend to become very big). 
#' @param ... Additional argument passed to \code{\link[base]{saveRDS}}
#' 
#' @return For \code{read_scrapeBill}, an \code{R} object. 
#' For \code{write_scrapeBill}, \code{NULL} invisibly. If \code{write_scrapeBill}
#' writes out auxiliary files under \code{FILENAME_files} folder, DON'T delete the 
#' folder. \code{read_scrapeBill} will need the files in the folder to read file.
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples
#' tgturl <- "https://www.congress.gov/bill/116th-congress/senate-bill/252"
#' bill252 <- scrapeBill(tgturl)
#' write_scrapeBill(bill252, "bill252.rds")
#' test <- read_scrapeBill("bill252.rds")
#'  
#' @importFrom xml2 xml_serialize
#' @importFrom xml2 xml_unserialize
#' @importFrom tools file_path_sans_ext
#' @importFrom pbapply pblapply
#' @importFrom rvest html_nodes
#' @importFrim stats rbinom
#' 
#' @export
read_scrapeBill <- function(file) {
  
  # Read File
  x <- readRDS(file)

  filename <- basename(file_path_sans_ext(file))
  foldername <- paste0(filename,"_files")
  dirloc <- paste(dirname(file),foldername,sep="/")
  
  # Put xml object back
  xmlback <- function(obj) {
    if (class(obj)[1]=="character") {
      if (dir.exists(dirloc)==FALSE) stop("auxiliary files not found!")
      tf <- paste(dirloc,obj,sep="/")
      con <- file(tf, "rb")
      on.exit(close(con), add = TRUE)
    } else if (class(obj)[1]=="raw"){
      con <- obj
    } else {
      stop("invalid object type!")
    }
    res <- xml_unserialize(con)
    return(res)
  }
  
  writexml <- function(k) {
    k$info <- xmlback(k$info)
    k$text <- xmlback(k$text)
    class(k) <- "scrapeBill"
    if (rbinom(1,1,0.05)==1) Sys.sleep(0.1)
    return(k)
  }
  
  if (class(x)[1]=="scrapeBill") {
    x <- writexml(x)
  } else if (class(x)[1]=="list" & class(x[[1]])=="scrapeBill") {
    x <- pblapply(x, function(k) writexml(k))
  } else {
    stop("invalid object type!")
  }
  
  return(x)
  
}

#' @rdname read_scrapeBill
#' @export
write_scrapeBill <- function(x, file, aux="None",...) {
  
  if (aux!="None") {
    filename <- basename(file_path_sans_ext(file))
    foldername <- paste0(filename,"_files")
    dirloc <- paste(dirname(file),foldername,sep="/")
    print(dir.exists(dirloc))
    if (dir.exists(dirloc)==TRUE) {
      unlink(paste0(dirloc,"/*"))
    } else {
      dir.create(dirloc)
    }
  }

  locback <- function(obj, aux) {
    
    # Save as nodeset
    if (class(obj)[1]=="xml_document") {
      obj <- html_nodes(obj, xpath="/html")
    }

    if (aux=="None") {
      res <- xml_serialize(obj, NULL)
    } else if (aux=="raw") {
      tf <- tempfile(tmpdir=dirloc)
      con <- file(tf, "wb")
      on.exit(close(con), add = TRUE)
      xml_serialize(obj, con)
      res <- basename(tf)
    } else if (aux=="html") {
      
    } else {
      stop("invalid aux argument!")
    }
    return(res)
  }
  
  writeloc <- function(k) {
    k$info <- locback(k$info, aux=aux)
    k$text <- locback(k$text, aux=aux)
    class(k) <- "scrapeBill"
    return(k)
  }
  
  if (class(x)[1]=="scrapeBill") {
    x <- writeloc(x)
  } else if (class(x)[1]=="list" & class(x[[1]])=="scrapeBill") {
    x <- pblapply(x, function(k) writeloc(k))
  } else {
    stop("invalid object type.")
  }
  
  #save
  saveRDS(x, file = file, ...)
}

