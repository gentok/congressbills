#' Read and write \code{scrapeBill} Object
#' 
#' @description Write (especially a list of) \code{scrapeBill} object. 
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param file A connection or the name of the file (use \code{.rds} extension).
#' @param ... Additional argument passed to \code{\link[base]{saveRDS}}
#' 
#' @return For \code{read_scrapeBill}, an \code{R} object. 
#' For \code{write_scrapeBill}, \code{NULL} invisibly. \code{write_scrapeBill} also writes out 
#' auxiliary files under \code{FILENAME_files} folder. DON'T delete the folder. 
#' \code{read_scrapeBill} will need the files in the folder to read file.
#' 
#' @importFrom xml2 xml_serialize
#' @importFrom xml2 xml_unserialize
#' @importFrom tools file_path_sans_ext
#' @importFrom pbapply pblapply
#' @importFrom rvest html_nodes
#' 
#' @export
read_scrapeBill <- function(file) {
  
  filename <- basename(file_path_sans_ext(file))
  foldername <- paste0(filename,"_files")
  dirloc <- paste(dirname(file),foldername,sep="/")
  if (dir.exists(dirloc)==FALSE) stop("auxiliary files not found!")
  
  # Read File
  x <- readRDS(file)
  
  # Put xml object back
  xmlback <- function(obj) {
    tf <- paste(dirloc,obj,sep="/")
    con <- file(tf, "rb")
    on.exit(close(con), add = TRUE)
    res <- xml_unserialize(con)
    return(res)
  }
  
  writexml <- function(k) {
    k$info <- xmlback(k$info)
    k$text <- xmlback(k$text)
    class(k) <- "scrapeBill"
    return(k)
  }
  
  if (class(x)[1]=="scrapeBill") {
    x <- writexml(x)
  } else if (class(x)[1]=="list" & class(x[[1]])=="scrapeBill") {
    x <- pblapply(x, function(k) writexml(k))
  } else {
    stop("invalid object type.")
  }
  
  return(x)
  
}

#' @rdname read_scrapeBill
#' @export
write_scrapeBill <- function(x, file, ...) {
  
  filename <- basename(file_path_sans_ext(file))
  foldername <- paste0(filename,"_files")
  dirloc <- paste(dirname(file),foldername,sep="/")
  if (dir.exists(dirloc)==TRUE) {
    unlink(paste0(dirloc,"/*"))
  } else {
    dir.create(dirloc)
  }

  locback <- function(obj) {
    obj <- html_nodes(obj, xpath="/html")
    tf <- tempfile(tmpdir=dirloc)
    con <- file(tf, "wb")
    xml_serialize(obj, con)
    on.exit(close(con), add = TRUE)
    return(basename(tf))
  }
  
  writeloc <- function(k) {
    k$info <- locback(k$info)
    k$text <- locback(k$text)
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

