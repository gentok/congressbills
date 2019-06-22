#' Collecting Bill ID from a \code{scrapeBill} Object.
#' 
#' @description Collect information that identifies bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return A \code{data.frame} with bill ID information.
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#' @importFrom stringr str_c
#' @importFrom pbapply pbsapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/109th-congress/house-bill/247"
#' bill247 <- scrapeBill(tgturl)
#' collectBillID(bill247)
#' 
#' @export

collectBillID <- function(x, progress=FALSE) {
  
  collectBillID1 <- function(x) {
    
    if (!"scrapeBill" %in% class(x)) stop("x (element) class must be 'scrapeBill'!")
    
    #All-info HTML
    tgt <- x$info
    
    # Look into Location of Bill IDs
    a <- tgt %>% html_nodes("title") %>% html_text()
    
    # Extract Info
    billinfo <- str_extract(a, "[[:alpha:]..]+[[:digit:]]+")
    conginfo <- str_extract(a, "[[:digit:]]+[[:alpha:]]+ Congress")
    billname <- sub("^.*\\: ", "", a)
    billname <- sub(" \\|.*$", "", billname)
    
    # Clean Up Info
    number <- as.numeric(str_extract(billinfo, "[[:digit:]]+"))
    chx <- str_extract(billinfo, "[[:alpha:]..]+")
    chx <- gsub("^H.R.$","H.", chx)
    if (chx %in% c("H.","H.Res.","H.J.Res.","H.Con.Res.","H.Amdt.")) {
      chamber <- "House"
      type <- gsub("^H", "", chx)
      type <- gsub("^\\.$", "Bill", type)
      type <- gsub("^\\.Res\\.$", "Simple Resolution", type)
      type <- gsub("^\\.J\\.Res\\.$", "Joint Resolution", type)
      type <- gsub("^\\.Con\\.Res\\.$", "Concurrent Resolution", type)
      type <- gsub("^\\.Amdt\\.$", "Amendment", type)
      chab <- gsub("[[:lower:]..]", "", chx)
    } else if (chx %in% c("S.","S.Res.","S.J.Res.","S.Con.Res.","S.Amdt.")) {
      chamber <- "Senate"
      type <- gsub("^S", "", chx)
      type <- gsub("^\\.$", "Bill", type)
      type <- gsub("^\\.Res\\.$", "Simple Resolution", type)
      type <- gsub("^\\.J\\.Res\\.$", "Joint Resolution", type)
      type <- gsub("^\\.Con\\.Res\\.$", "Concurrent Resolution", type)
      type <- gsub("^\\.Amdt\\.$", "Amendment", type)
      chab <- gsub("[[:lower:]..]", "", chx)
    } else {
      stop("Unknown Bill Chamber/Type Information")
    } 
    cong <- as.numeric(str_extract(conginfo, "[[:digit:]]+"))
    id <- str_c(chab, sprintf("%03.0f", cong), sprintf("%05.0f", number))
    
    # Export Results
    res <- data.frame(id, cong, chamber, type, number, billname,
                      stringsAsFactors = FALSE)
    colnames(res) <- c("ID", "Congress", "Chamber", "Type", "Number","Name")
    
    return(res)
  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectBillID1(x)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Bill ID...\n")
      resx <- pbsapply(x, collectBillID1)
    } else {
      resx <- sapply(x, collectBillID1)
    }
    resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
    for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)

}


#' Collecting Bill Title(s) from a \code{scrapeBill} Object.
#' 
#' @description Collect information regarding title(s) of the bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param officialonly If \code{TRUE} (default), the function returns the the text of the official title as introduced. 
#' If \code{FALSE}, the function returns the \code{data.frame} object of all 
#' published official and short titles and their published timings.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return A \code{character} vector or \code{data.frame} list with bill title information.
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom stringr str_extract
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pblapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/109th-congress/house-bill/247"
#' bill247 <- scrapeBill(tgturl)
#' collectTitle(bill247)
#' 
#' @export

collectTitle <- function(x, 
                         officialonly=TRUE,
                         progress=FALSE) {
  
  # For Length 1 x
  collectTitle1<- function(x, officialonly) {
    
    if (!"scrapeBill" %in% class(x)) stop("x (element) class must be 'scrapeBill'!")
    
    #All-info HTML
    tgt <- x$info
    
    # Look into Location of Bill IDs
    a <- tgt %>% html_nodes("div") 
    aids <- a %>% html_attr("id")
    a <- a[which(aids=="titles-content")] %>% html_nodes("div")
    acls <- a %>% html_attr("class")
    
    # Function to be used later
    splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
    
    # Official Title
    otloc <- which(acls=="officialTitles")
    ot <- a[otloc:length(a)]
    # Official Title Timing
    otmeta <- ot %>% html_nodes(xpath=".//h4|.//h5") %>% html_text
    otmeta1 <- otmeta[grep("Official Title(s|) (as|on)", otmeta)]
    otmeta1 <- gsub("Official Title(s|) ","", otmeta1)
    # Official Titles Chamber
    otdivide <- grep("Official Title(s|) - ", otmeta)
    otmeta2 <-  splitAt(otmeta, otdivide)
    otmeta2 <- sapply(otmeta2, function(k) length(k[grep("Official Title(s|) (as|on)", k)]))
    if (otdivide[1]==1) {
      otnames <- otmeta[grep("Official Title(s|) - ", otmeta)]
    } else {
      otnames <- c("Unspecified", otmeta[grep("Official Title(s|) - ", otmeta)])
    }
    otmeta2 <- rbind(otmeta2, otnames)
    otmeta2 <- unlist(apply(otmeta2, 2, function(k) rep(k[2],k[1])))
    otmeta2 <- str_extract(otmeta2, "House|Senate|Unspecified")
    # Official Titles Text
    ottext <- ot %>% html_nodes("p") %>% html_text()
    # Compile into Data
    otdata <- data.frame(Type = "Official",
                         Chamber = rev(otmeta2),
                         Timing = rev(otmeta1),
                         Text = ottext, 
                         stringsAsFactors = FALSE)
    
    # Short Title
    stloc <- which(acls=="shortTitles")
    if (length(stloc)==1) {
      st <- a[seq(stloc,otloc-1,1)]
      # Short Title Timing
      stmeta <- st %>% html_nodes(xpath=".//h4|.//h5") %>% html_text()
      stmeta1 <- stmeta[grep("Short Title(s|) (as|on)|Other Short Titles", stmeta)]
      stmeta1 <- gsub("Short Title(s|) ","", stmeta1)
      # Short Titles Chamber
      stdivide <- grep("Short Title(s|) - ", stmeta)
      stmeta2 <-  splitAt(stmeta, stdivide)
      stmeta2 <- sapply(stmeta2, function(k) length(k[grep("Short Title(s|) (as|on)|Other Short Titles", k)]))
      if (stdivide[1]==1) {
        stnames <- stmeta[grep("Short Title(s|) - ", stmeta)]
      } else {
        stnames <- c("Unspecified", stmeta[grep("Short Title(s|) - ", stmeta)])
      }
      stmeta2 <- rbind(stmeta2, stnames)
      stmeta2 <- unlist(apply(stmeta2, 2, function(k) rep(k[2],k[1])))
      stmeta2 <- str_extract(stmeta2, "House|Senate|Unspecified")
      # Short Titles Text
      sttext <- st %>% html_nodes(xpath=".//p|.//ul") %>% html_text
      sttext <- str_squish(sttext)
      # Compile into Data
      stdata <- try(data.frame(Type = "Short",
                           Chamber = rev(stmeta2),
                           Timing = rev(stmeta1),
                           Text = sttext, 
                           stringsAsFactors = FALSE))
      if (class(stdata)[1]=="try-error") {
        if (officialonly==FALSE) warning("Short Title Data Extraction Failed!")
        stdata <- data.frame(Type = character(),
                             Chamber = character(),
                             Stage = character(),
                             Text = character(), 
                             stringsAsFactors = FALSE)
      }
    } else {
      stdata <- data.frame(Type = character(),
                           Chamber = character(),
                           Stage = character(),
                           Text = character(), 
                           stringsAsFactors = FALSE)
    }
    
    # All Title Data
    titledata <- rbind(otdata,stdata)
    
    # Reordering Data to (approximately) Follow the Legislation Process
    if (otdata$Chamber[1]=="House") {
      rowlevels <- c("House","Senate","Unspecified")
    } else {
      rowlevels <- c("Senate","House","Unspecified")
    }
    roworder <- order(factor(titledata$Chamber,levels=rowlevels))
    titledata <- titledata[roworder,]
    
    if (officialonly==TRUE) {
      res <- otdata$Text[1]
    } else {
      res <- titledata
    }
    
    return(res)
  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectTitle1(x, officialonly = officialonly)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Title...\n")
      if (officialonly==TRUE) {
        resx <- pbsapply(x, collectTitle1, officialonly=officialonly)
      } else {
        resx <- pblapply(x, collectTitle1, officialonly=officialonly)
      }
    } else {
      if (officialonly==TRUE) {
        resx <- sapply(x, collectTitle1, officialonly=officialonly)
      } else {
        resx <- lapply(x, collectTitle1, officialonly=officialonly)
      }
    }
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)
  
}

#' Collecting Summary Information from a \code{scrapeBill} Object.
#' 
#' @description Collect information regarding summary of the bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param latestonly If \code{TRUE} (default), the function returns only the text of 
#' the latest summary. 
#' If \code{FALSE}, the function returns the \code{data.frame} object of all 
#' published summaries and their published dates and timings.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return A \code{character} or \code{data.frame} object with summary texts.
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom stringr str_squish
#' @importFrom stringr str_extract
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pblapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/109th-congress/house-bill/247"
#' bill247 <- scrapeBill(tgturl)
#' collectSummary(bill247)
#' 
#' @export

collectSummary <- function(x, 
                           latestonly=TRUE,
                           progress=FALSE) {
  
  collectSummary1 <- function(x, latestonly) {
    
    if (!"scrapeBill" %in% class(x)) stop("x class must be 'scrapeBill'!")
    
    # All-info HTML
    tgt <- x$info
    
    # Look for Location of Summary
    a <- tgt %>% html_nodes("div") 
    sumloc <- which(html_attr(a, "id")=="allSummaries-content")[1]
    a <- a[sumloc]
    sumN <- a %>% html_nodes("h2") %>% html_text()
    sumN <- as.numeric(str_extract(sumN, "[[:digit:]]+"))
    
    # Generate Summary Data
    if (sumN==0) {
      
      summaries <- data.frame(Date = character(),
                              Timing = character(), 
                              Text = character(),
                              stringsAsFactors = FALSE)
      if (latestonly==TRUE) summaries <- NA
      
    } else {
      
      # Extract Summary Texts
      b1 <- a %>% html_nodes("div")
      droptxt <- b1 %>% html_nodes("b") %>% html_text()
      b1 <- b1[grep("^[[:digit:]]+$", html_attr(b1, "id"))] %>% html_text
      b1 <- sub("\\(This measure has not been amended since it was .* The summary has been expanded because action occurred on the measure\\.\\)",
                 "", b1)
      b1 <- gsub("\\(This measure has not been amended since it was .* summary of .* version is repeated here\\.\\)",
                 "", b1)
      if (length(droptxt)>0) b1 <- gsub(droptxt[1], "", b1)
      b1 <- str_squish(b1)
      # Extract Summary Date & Timing
      b2 <- a %>% html_nodes("h3") %>% html_text
      b2 <- gsub("^Shown Here\\:", "", b2)
      b2a <- gsub(" \\(+[[:digit:]/]+\\)", "", b2)
      b2b <- str_extract(b2, "\\(+[[:digit:]/]+\\)")
      b2b <- gsub("\\(|\\)", "", b2b)
      # Compile Data in Data.frame
      summaries <- data.frame(Date = b2b,
                              Timing = b2a, 
                              Text = b1,
                              stringsAsFactors = FALSE)
      # If latestonly==TRUE, provide only the latest summary
      if (latestonly==TRUE) summaries <- summaries$Text[1]
      
    }
    
    return(summaries)
    
  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectSummary1(x, latestonly = latestonly)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Summary...\n")
      if (latestonly==TRUE) {
        resx <- pbsapply(x, collectSummary1, latestonly=latestonly)
      } else {
        resx <- pblapply(x, collectSummary1, latestonly=latestonly)
      }
    } else {
      if (latestonly==TRUE) {
        resx <- sapply(x, collectSummary1, latestonly=latestonly)
      } else {
        resx <- lapply(x, collectSummary1, latestonly=latestonly)
      }
    }
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)

}


#' Collecting Sponsorship Information from a \code{scrapeBill} Object.
#' 
#' @description Collect information regarding sponsorship of the bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param single Whether to export single row results (see following description). 
#' The default is \code{TRUE}.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return If \code{single==TRUE}, returns \code{data.frame} object with 
#' following variables (single row):
#' \itemize{
#'   \item \code{Sp_Chamber}: Chamber of Primary Sponsor
#'   \item \code{Sp_Name}: Full Name of Primary Sponsor
#'   \item \code{Sp_LastName}: Last Name of Primary Sponsor
#'   \item \code{Sp_Party}: Party of Primary Sponsor
#'   \item \code{Sp_State}: State of Primary Sponsor
#'   \item \code{Sp_District}: District of Primary Sponsor (Only for House Representatives)
#'   \item \code{Cosp_N}: Total Number of Cosponsors
#'   \item \code{Cosp_Original}: Number of Original Cosponsors
#'   \item \code{Cosp_D}: Number of Democratic Cosponsors
#'   \item \code{Cosp_R}: Number of Republican Cosponsors
#' }
#' 
#' If \code{single==FALSE}, \code{data.frame} object with following 
#' variables (rows represent sponsors):
#' \itemize{
#'   \item \code{Date}: Date of Sponsorship. If Primary Sponsor, this is the bill introduction date.
#'   \item \code{Status}: Status of Sponsorship. One of "Primary Sponsor," "Original Cosponsor," and "Cosponsor."
#'   \item \code{Chamber}: Chamber of Sponsor
#'   \item \code{Name}: Full Name of Sponsor
#'   \item \code{LastName}: Last Name of Sponsor
#'   \item \code{Party}: Party of Sponsor
#'   \item \code{State}: State of Sponsor
#'   \item \code{District}: District of Sponsor (Only for House Representatives)
#' }
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom rvest html_table
#' @importFrom stringr str_split
#' @importFrom stringr str_extract
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pblapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/109th-congress/house-bill/19"
#' bill19 <- scrapeBill(tgturl)
#' collectSponsorship(bill19)
#' 
#' @export

collectSponsorship <- function(x,
                               single = TRUE,
                               progress=FALSE) {
  
  collectSponsorship1 <- function(x, single) {
    
    if (!"scrapeBill" %in% class(x)) stop("x class must be 'scrapeBill'!")
    
    # All-info HTML
    tgt <- x$info
    
    # Look for Location of Cosponsorship Information
    a <- tgt %>% html_nodes("div") 
    # Sponsor
    loc1 <- which(html_attr(a, "class")=="overview")[1]
    a1 <- a[loc1]
    # Cosponsors
    loc2 <- which(html_attr(a, "id")=="cosponsors-content")[1]
    a2 <- a[loc2]
    
    # Sponsor
    sp <- a1 %>% html_nodes("table") %>% html_table()
    sp <- sp[[1]][1,2]
    stab <- data.frame(Cosponsor = gsub(" \\(.*$", "", sp),
                       Date = gsub("^.*Introduced |\\)$", "", sp),
                       stringsAsFactors = FALSE)
    
    
    # Cosponsors
    c <- a2 %>% html_nodes("h2") %>% html_text()
    ncosponsors <- as.numeric(str_extract(c, "[[:digit:]]+"))
    
    # If N of Cosponsors > 0
    if (ncosponsors>0) {
      stab2 <- a2 %>% html_nodes("table") %>% html_table()
      stab2 <- stab2[[1]]
      colnames(stab2)[colnames(stab2)=="Date Cosponsored"] <- "Date"  
      stab <- rbind(stab, stab2, stringAsFactors=FALSE)
    }
    
    ## Modify Data
    # Status of cosponsorship
    stab$Status <- "Cosponsor"
    stab$Status[1] <- "Primary Sponsor"
    stab$Status[grep("\\*$", stab$Cosponsor)] <- "Original Cosponsor"
    # Representative or Senate
    stab$Chamber <- sapply(str_split(stab$Cosponsor," "), function(k) k[1])
    stab$Chamber <- gsub("Rep.", "House", stab$Chamber)
    stab$Chamber <- gsub("Sen.", "Senate", stab$Chamber)
    # Party, State, District
    psdinfo <- str_extract(stab$Cosponsor, "\\[+[[:upper:][:digit:]-]+(|At Large)+\\]")
    psdinfo <- str_split(gsub("\\[|\\]", "", psdinfo), "-")
    stab$Party <- sapply(psdinfo, function(k) k[1])
    stab$State <- sapply(psdinfo, function(k) k[2])
    stab$District <- as.character(sapply(psdinfo, function(k) k[3]))
    # Clean Names
    stab$Name <- gsub("\\*$", "", stab$Cosponsor)
    stab$Name <- gsub("^Rep. ", "", stab$Name)
    stab$Name <- gsub("^Sen. ", "", stab$Name)
    stab$Name <- gsub(" \\[+[[:upper:][:digit:]-]+(|At Large)+\\]", "", stab$Name)
    # Last Name
    stab$LastName <- sub(",.*$", "", stab$Name)

    ## Export Data
    stab <- stab[,c("Date","Status","Chamber","Name","LastName","Party","State","District")]
    
    if (single==FALSE) {

      res <- stab
      
    } else {

      # Make simplified Data
      sponsor <- stab[1,-c(1,2)]
      cosponsors <- stab[-1,]
      colnames(sponsor) <- c("Sp_Chamber","Sp_Name","Sp_LastName",
                             "Sp_Party","Sp_State","Sp_District")
      sponsor$Cosp_N <- nrow(cosponsors)
      sponsor$Cosp_Original <- length(which(cosponsors$Status=="Original Cosponsor"))
      sponsor$Cosp_D <- length(which(cosponsors$Party=="D"))
      sponsor$Cosp_R <- length(which(cosponsors$Party=="R"))
      res <- sponsor
      
    }
    return(res)
    
  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectSponsorship1(x, single = single)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Sponsorship...\n")
      if (single==TRUE) {
        resx <- pbsapply(x, collectSponsorship1, single=single)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- pblapply(x, collectSponsorship1, single=single)
      }
    } else {
      if (single==TRUE) {
        resx <- sapply(x, collectSponsorship1, single=single)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- lapply(x, collectSponsorship1, single=single)
      }
    }
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)

}

#' Collecting Action Information from a \code{scrapeBill} Object.
#' 
#' @description Collect information regarding action(s) made on the bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param latestonly If \code{TRUE} (default), the function returns summary of 
#' latest actions (and introduced date) in single row. If \code{FALSE}, the function returns \code{data.frame} 
#' object of all actions and their dates and timings.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return If \code{latestonly==TRUE}, returns \code{data.frame} object with following 
#' contents (single row):
#' \itemize{
#'   \item \code{IntroducedDate}: Date of Introduction in Actions Overview
#'   \item \code{ActLatestDate_Overview}: Date of Latest Action in Actions Overview
#'   \item \code{ActLatestText_Overview}: Overview Content of Latest Action in Actions Overview
#'   \item \code{ActLatestDate_All}: Date of Latest Action in All Actions
#'   \item \code{ActLatestText_All}: Detailed Content of Latest Action in All Actions
#'   \item \code{VoteTimes_House}: Number of Voting Opportunities in House
#'   \item \code{VoteLatestDate_House}: Date of Latest Vote Action in House
#'   \item \code{VoteLatestType_House}: Type of Latest Vote in House
#'   \item \code{VoteLatestText_House}: Content of Latest Vote in House
#'   \item \code{VoteTimes_Senate}: Number of Voting Opportunities in Senate
#'   \item \code{VoteLatestDate_Senate}: Date of Latest Action in Senate
#'   \item \code{VoteLatestType_Senate}: Type of Latest Vote in Senate
#'   \item \code{VoteLatestText_Senate}: Content of Latest Vote in Senate
#'   \item \code{RCTimes_House}: Number of Roll Call Voting Opportunities in House
#'   \item \code{RCLatestDate_House}: Date of Latest Roll Call in House
#'   \item \code{RCLatestText_House}: Content of Latest Roll Call Vote in House
#'   \item \code{RCLatestNumber_House}: ID Number of Latest Roll Call Vote in House
#'   \item \code{RCLatestYea_House}: Number of Yea in Latest Roll Call Vote in House
#'   \item \code{RCLatestNay_House}: Number of Nay in Latest Roll Call Vote in House
#'   \item \code{RCTimes_Senate}: Number of Roll Call Voting Opportunities in Senate
#'   \item \code{RCLatestDate_Senate}: Date of Latest Roll Call in Senate
#'   \item \code{RCLatestText_Senate}: Content of Latest Roll Call Vote in Senate
#'   \item \code{RCLatestNumber_Senate}: ID Number of Latest Roll Call Vote in Senate
#'   \item \code{RCLatestYea_Senate}: Number of Yea in Latest Roll Call Vote in Senate
#'   \item \code{RCLatestNay_Senate}: Number of Nay in Latest Roll Call Vote in Senate
#' }
#' 
#' If \code{latestonly==FALSE}, A \code{data.frame} object containing following variables:
#' \itemize{
#'   \item \code{Chamber}: Chamber where the Action is Placed.
#'   \item \code{Date}: Date of Action
#'   \item \code{Time}: Time (in Day) of Action
#'   \item \code{Detail}: Detailed Content of Action
#'   \item \code{Overview}: Summarized Overview Content of Action (If Available)
#'   \item \code{Amendment}: 1 if Actions is related to amendment of the bill
#'   \item \code{VoteType}: Type of Vote (if any decision is made)
#'   \item \code{rollnumber}: Roll Call Number if Action is Roll Call Vote
#'   \item \code{rollYea}: Roll Call Yea Votes if Action is Roll Call Vote
#'   \item \code{rollNay}: Roll Call Nay Votes if Action is Roll Call Vote
#' }
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom rvest html_table
#' @importFrom stringr str_split
#' @importFrom stringr str_extract
#' @importFrom stringr str_squish
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pblapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/115th-congress/house-bill/2"
#' bill2 <- scrapeBill(tgturl)
#' collectAction(bill2)
#'  
#' @export

collectAction <- function(x,
                          latestonly = TRUE,
                          progress=FALSE) {
  
  collectAction1 <- function(x, latestonly) {
    
    if (!"scrapeBill" %in% class(x)) stop("x class must be 'scrapeBill'!")
    
    # All-info HTML
    tgt <- x$info

    # Look for Location of Action Information
    a <- tgt %>% html_nodes("div") 
    # Action Overview
    loc1 <- which(html_attr(a, "id")=="actionsOverview-content")[1]
    a1 <- a[loc1]
    # All Actions
    loc2 <- which(html_attr(a, "id")=="allActions-content")[1]
    a2 <- a[loc2]
    
    # Action Overview
    ao <- a1 %>% html_nodes("table") %>% html_table()
    ao <- ao[[1]]
    colnames(ao)[colnames(ao)==""] <- "Action"
    passedline <- grep("Passed/agreed to in (House|Senate): ", ao$Action)
    passedvetoline <- grep("Passed (House|Senate) over veto: ", ao$Action)
    failedline <- grep("Failed of passage/not agreed to in (House|Senate): ", ao$Action)
    failedvetoline <- grep("Failed of passage in (House|Senate) over veto: ", ao$Action)
    ao$Action <- gsub("Passed/agreed to in (House|Senate): ", "", ao$Action)
    ao$Action <- gsub("Passed (House|Senate) over veto: ", "", ao$Action)
    ao$Action <- gsub("Failed of passage/not agreed to in (House|Senate): ", "", ao$Action)
    ao$Action <- gsub("Failed of passage in (House|Senate) over veto: ", "", ao$Action)
    passedtxt <- ao$Action[passedline]
    passedvetotxt <- ao$Action[passedvetoline]
    failedtxt <- ao$Action[failedline]
    failedvetotxt <- ao$Action[failedvetoline]
    
    # All Actions
    aa <- a2 %>% html_nodes("table") %>% html_table()
    aa <- aa[[1]]
    colnames(aa)[colnames(aa)==""] <- "Action"
    # If Chamber Variable Does Not Exist
    if (!"Chamber" %in% colnames(aa)) {
      aa_by <- a2 %>% html_nodes("table") %>% html_nodes("span") %>% html_text()
      for(i in 1:nrow(aa)) {
        aa$Action[i] <- str_squish(sub(aa_by[i], "", aa$Action[i]))
      }
      aa$Chamber <- str_extract(aa_by, "House|Senate")
    }
    aa$Chamber[aa$Chamber==""] <- NA
    # Clean Actions
    aa$Action <- sub(" \\((consideration|text).*\\)","", aa$Action)
    # Overview 
    aa$Overview <- NA
    aa$Overview[aa$Action %in% passedtxt] <- paste("Passed in", aa$Chamber[aa$Action %in% passedtxt])
    aa$Overview[aa$Action %in% failedtxt] <- paste("Failed in", aa$Chamber[aa$Action %in% failedtxt])
    aa$Overview[aa$Action %in% passedvetotxt] <- paste("Passed in", aa$Chamber[aa$Action %in% passedvetotxt], "over veto")
    aa$Overview[aa$Action %in% failedvetotxt] <- paste("Failed in", aa$Chamber[aa$Action %in% failedvetotxt], "over veto")
    aa$Overview[grep("Signed by President", aa$Action)] <- "Signed by President"
    aa$Overview[grep("Vetoed by President", aa$Action)] <- "Vetoed by President"
    aa$Overview[grep("Presented to President", aa$Action)] <- "To President"
    aa$Overview[grep("Became Public Law", aa$Action)] <- "Became Law"
    aa$Overview[grep("^Introduced", aa$Action)] <- "Introduced"
    # Rows that are Amendment
    aa$Amendment <- str_extract(aa$Action, "[[:alpha:].]+Amdt+[[:digit:].]+")
    aa$Action <- gsub("[[:alpha:].]+Amdt+[[:digit:].]+ ", "", aa$Action)
    # Rollcall Vote
    s_rolltxt <- str_extract(aa$Action, "Record Vote Number: +[[:digit:]]+(.|)")
    h_rolltxt <- str_extract(aa$Action, "\\(Roll no. +[[:digit:]]+\\)(.|)")
    aa$Action <- gsub("Record Vote Number: +[[:digit:]]+(.|)","", aa$Action)
    aa$Action <- gsub("\\(Roll no. +[[:digit:]]+\\)(.|)","", aa$Action)
    s_rollnumber <- str_extract(s_rolltxt, "[[:digit:]]+")
    h_rollnumber <- str_extract(h_rolltxt, "[[:digit:]]+")
    aa$rollnumber <- NA
    aa$rollnumber[aa$Chamber %in% "House"] <- h_rollnumber[aa$Chamber %in% "House"]
    aa$rollnumber[aa$Chamber %in% "Senate"] <- s_rollnumber[aa$Chamber %in% "Senate"]
    aa$rollnumber  <- as.numeric(aa$rollnumber)
    # Yea Nay Vote
    rollres <- str_extract(aa$Action, "[[:digit:]]+ - +[[:digit:]]+")
    rollres <- str_split(rollres, " - ")
    aa$rollYea <- as.numeric(sapply(rollres, function(k) k[1]))
    aa$rollYea[is.na(aa$rollnumber)] <- NA
    aa$rollNay <- as.numeric(sapply(rollres, function(k) k[2]))
    aa$rollNay[is.na(aa$rollnumber)] <- NA
    # Delete Unnecessary spaces
    aa$Action <- gsub("\\(TXT \\| PDF\\)","", aa$Action)
    aa$Action <- str_squish(aa$Action)
    # VoteType
    aa$VoteType <- NA
    aa$VoteType[!is.na(aa$rollnumber)] <- "Roll Call"
    aa$VoteType[grep("(V|v)oice (V|v)ote", aa$Action)] <- "Voice Vote"
    aa$VoteType[grep("(w|W)ithout (O|o)bjection", aa$Action)] <- "Unanimous Consent"
    # Date Time Split 
    datesplit <- str_split(aa$Date, "-")
    aa$Date <- sapply(datesplit, function(k) k[1])
    aa$Time <- sapply(datesplit, function(k) k[2])
    # Rename Action Variable
    aa$Detail <- aa$Action
    
    # Reorder Columns
    aa <- aa[,c("Date","Time","Detail",
                "Overview","Amendment","VoteType",
                "rollnumber","rollYea","rollNay")]
    
    # Simplifying Result If Required
    if (latestonly==FALSE) {
      res <- aa
    } else {
      
      # Extract Only Latest Actions
      latestov <- which(!is.na(aa$Overview))[1]
      latestVoteH <- which(!is.na(aa$VoteType) & aa$Chamber %in% "House")
      latestVoteS <- which(!is.na(aa$VoteType) & aa$Chamber %in% "Senate")
      latestRCH <- which(!is.na(aa$rollnumber) & aa$Chamber %in% "House")
      latestRCS <- which(!is.na(aa$rollnumber) & aa$Chamber %in% "Senate")
      
      res <- data.frame(IntroducedDate = NA,
                        ActLatestDate_Overview = aa$Date[latestov],
                        ActLatestText_Overview = aa$Overview[latestov],
                        ActLatestDate_All = aa$Date[1],
                        ActLatestText_All = aa$Detail[1],
                        VoteTimes_House = length(latestVoteH),
                        VoteLatestDate_House = aa$Date[latestVoteH[1]],
                        VoteLatestType_House = aa$VoteType[latestVoteH[1]],
                        VoteLatestText_House = aa$Detail[latestVoteH[1]],
                        VoteTimes_Senate = length(latestVoteS),
                        VoteLatestDate_Senate = aa$Date[latestVoteS[1]],
                        VoteLatestType_Senate = aa$VoteType[latestVoteS[1]],
                        VoteLatestText_Senate = aa$Detail[latestVoteS[1]],
                        VoteTimes_House = length(latestVoteH),
                        RCLatestDate_House = aa$Date[latestRCH[1]],
                        RCLatestText_House = aa$Detail[latestRCH[1]],
                        RCLatestNumber_House = aa$rollnumber[latestRCH[1]],
                        RCLatestYea_House = aa$rollYea[latestRCH[1]],
                        RCLatestNay_House = aa$rollNay[latestRCH[1]],
                        RCTimes_Senate = length(latestRCS),
                        RCLatestDate_Senate = aa$Date[latestRCS[1]],
                        RCLatestText_Senate = aa$Detail[latestRCS[1]],
                        RCLatestNumber_Senate = aa$rollnumber[latestRCS[1]],
                        RCLatestYea_Senate = aa$rollYea[latestRCS[1]],
                        RCLatestNay_Senate = aa$rollNay[latestRCS[1]],
                        stringsAsFactors = FALSE)
      
      if (length(which(aa$Overview=="Introduced"))==1) {
        res$IntroducedDate = aa$Date[which(aa$Overview=="Introduced")]
      }
    }
    
    return(res)

  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectAction1(x, latestonly = latestonly)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Action...\n")
      if (latestonly==TRUE) {
        resx <- pbsapply(x, collectAction1, latestonly=latestonly)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- pblapply(x, collectAction1, latestonly=latestonly)
      }
    } else {
      if (latestonly==TRUE) {
        resx <- sapply(x, collectAction1, latestonly=latestonly)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- lapply(x, collectAction1, latestonly=latestonly)
      }
    }
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)
  
}

#' Collecting Committee Information from a \code{scrapeBill} Object.
#' 
#' @description Collect information regarding committees considering the bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param single Whether to export single row results (see following description). 
#' The default is \code{TRUE}.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return If \code{single==TRUE}, returns \code{data.frame} object with 
#' following information (single row):
#' \itemize{
#'   \item \code{Committee_N}: Number of Committees the Bill Referred to
#'   \item \code{Committee_Names}: Name(s) of Referred Committees (If multiple committees, divided by \code{;}) 
#' }
#' 
#' If \code{single==FALSE}, A \code{data.frame} object containing following variables 
#' (rows represent date of activity):
#' \itemize{
#'   \item \code{Date}: Date of Committee Activity
#'   \item \code{Main}: Name of Committee 
#'   \item \code{Sub}: Name of Subcommittee, if any
#'   \item \code{Activity}: Content of Activity 
#'   \item \code{Reports}: Name of Reports, if any 
#' }
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom stringr str_squish
#' @importFrom stringr str_extract
#' @importFrom stringr str_split
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pblapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/116th-congress/senate-bill/252"
#' bill252 <- scrapeBill(tgturl)
#' collectCommittee(bill252)
#' 
#' @export

collectCommittee <- function(x, 
                             single=TRUE,
                             progress=FALSE) {
  
  collectCommittee1 <- function(x, single) {
    
    if (!"scrapeBill" %in% class(x)) stop("x class must be 'scrapeBill'!")
    
    # All-info HTML
    tgt <- x$info
    
    # Look for Location of Committee Inoformation
    a <- tgt %>% html_nodes("div") 
    comloc <- which(html_attr(a, "id")=="committees-content")[1]
    a <- a[comloc] 
    # Number of Committees
    comN <- a %>% html_nodes("h2") %>% html_text()
    comN <- as.numeric(str_extract(comN, "[[:digit:]]+"))

    # Generate Summary Data
    if (comN==0) {
      
      committees <- data.frame(Date = character(),
                               Committee = character(),
                               Subcommittee = character(),
                               Activity = character(),
                               Reports = character(),
                               stringsAsFactors = FALSE)
      if (single==TRUE) {
        
        committees <- data.frame(Committee_N = 0,
                                 Committee_Names = NA,
                                 stringsAsFactors = FALSE)
        
      }
      
    } else {
      
      b <- a %>% html_nodes("table") %>% html_table(fill=TRUE)
      b <- b[[1]]
      keeploc <- which(apply(b, 1, function(k) length(which(!is.na(k))))>0)
      b <- b[keeploc,]
      b$Reports[a$Reports==""] <- NA
      
      comnames <- b$`Committee / Subcommittee`
      comnames <- str_split(comnames, "Subcommittee on ")
      b$Main <- str_squish(sapply(comnames, function(k) k[1]))
      b$Sub <- str_squish(sapply(comnames, function(k) k[2]))

      committees <- b[,c("Date",
                         "Main","Sub",
                         "Activity","Reports")]
      
      # If single==TRUE:
      if (single==TRUE) {
        
        comns <- unique(b$Main)
        comns <- comns[!is.na(comns)]
        committees <- data.frame(Committee_N = length(comns),
                                 Committee_Names = paste(comns, collapse="; "),
                                 stringsAsFactors = FALSE)
      }
      
    }
    
    return(committees)
    
  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectCommittee1(x, single = single)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Committee...\n")
      if (single==TRUE) {
        resx <- pbsapply(x, collectCommittee1, single=single)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- pblapply(x, collectCommittee1, single=single)
      }
    } else {
      if (single==TRUE) {
        resx <- sapply(x, collectCommittee1, single=single)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- lapply(x, collectCommittee1, single=single)
      }
    }
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)
  
}

#' Collecting Subject Information from a \code{scrapeBill} Object.
#' 
#' @description Collect information regarding subject field of the bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param single If \code{TRUE} (default), the function returns single row output (see below).
#' If \code{FALSE}, long format \code{data.frame} object with separate subject fields as rows.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return If \code{single==TRUE}, returns \code{data.frame} object with 
#' following information (single row):
#' \itemize{
#'   \item \code{Subject_Primary}: Primary Subject Field of the Bill
#'   \item \code{SUbject_N}: Number of Subject Fields Assigned to the Bill
#'   \item \code{Subject_All}: All Subject Fields divided by ";"
#' }
#' 
#' If \code{single==FALSE}, A \code{data.frame} object containing following variables 
#' (rows represent each subject filed):
#' \itemize{
#'   \item \code{ID}: Bill ID 
#'   \item \code{Subject}: Name of Subject Field
#'   \item \code{Rank}: The order of subject fields as presented in website. 1 indicates primary subject.
#' }
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pblapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/116th-congress/senate-bill/252"
#' bill252 <- scrapeBill(tgturl)
#' collectSubject(bill252)
#' 
#' @export

collectSubject <- function(x, 
                           single=TRUE,
                           progress=FALSE) {
  
  collectSubject1 <- function(x, single) {
    
    if (!"scrapeBill" %in% class(x)) stop("x class must be 'scrapeBill'!")
    
    # All-info HTML
    tgt <- x$info
    
    # Look for Location of Subjects
    a <- tgt %>% html_nodes("div") 
    subloc <- which(html_attr(a, "id")=="subjects-content")[1]
    a <- a[subloc] 
    # Subjects
    b <- a %>% html_nodes("li") %>% html_text()
    
    # Generate Subject Data
    res <- data.frame(Subject = b,
                      Rank = seq(1,length(b),1),
                      stringsAsFactors = FALSE)
    
    # If single==TRUE:
    if (single==TRUE) {
      
      subns <- unique(res$Subject)
      subns <- subns[!is.na(subns)]
      
      res <- data.frame(Subject_Primary = res$Subject[1],
                        Subject_N = nrow(res),
                        Subject_All = paste(subns, collapse="; "),
                        stringsAsFactors = FALSE)
      
    }

    return(res)
    
  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectSubject1(x, single = single)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Subject...\n")
      if (single==TRUE) {
        resx <- pbsapply(x, collectSubject1, single=single)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- pblapply(x, collectSubject1, single=single)
      }
    } else {
      if (single==TRUE) {
        resx <- sapply(x, collectSubject1, single=single)
        resx <- as.data.frame(t(resx), stringAsFactors=FALSE)
        for(k in 1:ncol(resx)) resx[,k] <- unlist(resx[,k])
      } else {
        resx <- lapply(x, collectSubject1, single=single)
      }
    }
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)
  
}

#' Collecting Text Information from a \code{scrapeBill} Object.
#' 
#' @description Collect information regarding the latest full text of the bill.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param allinone If \code{TRUE} (default), the function returns all texts of 
#' bill into one character scalar. If \code{FALSE}, the function separates text by 
#' potential sections and exported as list.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return A \code{character} or \code{list} object of Bill Texts.
#' 
#' @import magrittr
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_attrs
#' @importFrom rvest html_text
#' @importFrom stringr str_squish
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom pbapply pbsapply
#' @importFrom pbapply pblapply
#' 
#' @seealso \code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/116th-congress/senate-bill/252"
#' bill252 <- scrapeBill(tgturl)
#' collectText(bill252)
#' 
#' @export

collectText <- function(x, 
                        allinone=TRUE,
                        progress=FALSE) {
  
  collectText1 <- function(x, allinone) {
    
    if (!"scrapeBill" %in% class(x)) stop("x class must be 'scrapeBill'!")
    
    # All-info HTML
    tgt <- x$text
    
    # Look for Location of Texts
    a <- tgt %>% html_nodes("div") 
    loc1 <- which(html_attr(a, "id")=="textSelector") + 1
    loc2 <- loc1+1
    att1 <- html_attrs(a[loc1])[[1]]
    att2 <- html_attrs(a[loc2])[[1]]
    att1a <- length(grep("^generated", att1))>0
    att2a <- length(grep("^generated", att2))>0
    att1b <- length(grep("^billText", att1))>0
    att2b <- length(grep("^billText", att2))>0
    
    
    if (att1a) {
      a <- a[loc1]
    } else if (att2a) {
      a <- a[loc2]
    } else if (att1b|att2b) {
      
      a2 <- tgt %>% html_nodes("pre")
      locx <- grep("^billText", html_attr(a2, "id"))
      if (length(locx)==0) {
        stop("Unable to Find Bill Text!")
      }
      a <- a2[locx]

    } else {
      stop("Unable to Find Bill Text!")
    }
    
    # Make it A Text
    b <- a %>% html_text()
    b <- str_squish(b)
    
    # Split into Sections (if possible)
    if (allinone == TRUE) {

      res <- b

    } else {
      
      splitsec <- function(b) {
        txtsplit <- str_extract_all(b, "SEC(|TION)(\\.|) +[[:digit:][:upper:]]+(\\.|)")[[1]]
        c <- str_split(b, "SEC(|TION)(\\.|) +[[:digit:][:upper:]]+(\\.|)")[[1]]
        c <- as.list(str_squish(c))
        if (length(txtsplit)+1==length(c)) {
          txtsplit <- c("HEAD", txtsplit)
        }
        names(c) <- txtsplit
        return(c)
      }
      
      res <- try(splitsec(b))
      if (class(res)[1]=="try-error") {
        warning("Section Split Failed. Exported as All-In-One.")
        res <- b
      }

    }
    return(res)
    
  }
  
  # Possibly Applied for Vector
  if (class(x)=="scrapeBill") {
    resx <- collectText1(x, allinone = allinone)
  } else if (length(x)>=1 & "list" %in% class(x)) {
    if (progress==TRUE) {
      cat("\nCollecting Text...\n")
      if (allinone==TRUE) {
        resx <- pbsapply(x, collectText1, allinone=allinone)
      } else {
        resx <- pblapply(x, collectText1, allinone=allinone)
      }
    } else {
      if (allinone==TRUE) {
        resx <- sapply(x, collectText1, allinone=allinone)
      } else {
        resx <- lapply(x, collectText1, allinone=allinone)
      }
    }
  } else {
    stop("x has length 0 or x (element) class must be 'scrapeBill'!")
  }
  
  return(resx)
  
}

#' Collecting Bill Information from a \code{scrapeBill} Object.
#' 
#' @description Collect Bill Information and Put Into Data Frame.
#' 
#' @param x A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.
#' @param include \code{character} vector of What element(s) of bill information to 
#' include in output. Options are \code{"BillID"}, \code{"URL"}, \code{"Title"},
#' \code{"Summary"}, \code{"Sponsorship"}, \code{"Action"}, \code{"Committee"},
#' \code{"Subject"}, and \code{"Text"}. The default is to include everything 
#' except for \code{"Text"}. Need at least one element. 
#' Information set by this option uses single row output 
#' (i.e., Set \code{officialonly}, \code{latestonly}, \code{single}, and 
#' \code{allinone} to \code{TRUE} in \code{collectOPTION} functions).
#' @param expand The default is \code{"None"} (not to expand data). Else,
#' create long data by using expanded information (i.e., Set \code{officialonly}, \code{latestonly}, \code{single}, and 
#' \code{allinone} to \code{FALSE} in \code{collectOPTION} functions). 
#' Choose option from ONE of \code{"Title"},
#' \code{"Summary"}, \code{"Sponsorship"}, \code{"Action"}, \code{"Committee"},
#' \code{"Subject"}, and \code{"Text"}. CANNOT set more than one option.
#' @param progress If \code{TRUE}, show progress bar for the processing of multiple bills.
#' 
#' @return A \code{data.frame} object of bill information. See 
#' functions in \emph{See Also} section for variable details.
#' 
#' @seealso \code{\link{collectBillID}}, \code{\link{collectTitle}}, 
#' \code{\link{collectSummary}}, \code{\link{collectSponsorship}},
#' \code{\link{collectAction}}, \code{\link{collectCommittee}}, 
#' \code{\link{collectSubject}}, \code{\link{collectText}}, 
#' \code{\link{scrapeBill}}, and \code{\link{getscrapeBill}}
#' 
#' @examples 
#' tgturl <- "https://www.congress.gov/bill/116th-congress/senate-bill/252"
#' bill252 <- scrapeBill(tgturl)
#' collectBillData(bill252)
#' 
#' @export

collectBillData <- function(x, 
                        include = c("BillID",
                                    "URL",
                                    "Title",
                                    "Summary",
                                    "Sponsorship",
                                    "Action",
                                    "Committee",
                                    "Subject"),
                        expand = "None",
                        progress=FALSE) {
  
  # Initiate Data
  if (class(x)[1]=="scrapeBill") {
    urls <- x$url
  } else if (class(x)[1]=="list" & class(x[[1]])[1]=="scrapeBill") {
    urls <- sapply(x, function(k) k$url)
  } else {
    stop("invalid class of x")
  }
  d <- data.frame(initiate = urls,
                  stringsAsFactors = FALSE)
  d <- d[,-1]
  did <- collectBillID(x, progress = progress)
  
  if (length(include)==0) stop("Length of include is 0!")
  
  # Add Bill ID
  if("BillID" %in% include) {
    d <- cbind(d, did)
  }
  
  # Add URL
  if ("URL" %in% include) {
    d$URL <- urls
  }
  
  # Add Title
  if ("Title" %in% include) {
    d$Title <- collectTitle(x, progress = progress)
  }
  
  # Add Summary
  if ("Summary" %in% include) {
    d$Summary <- collectSummary(x, progress = progress)
  }
  
  # Add Sponsorship
  if ("Sponsorship" %in% include) {
    dx <- collectSponsorship(x, progress = progress)
    d <- cbind(d,dx)
  }
  
  # Add Action
  if ("Action" %in% include) {
    dx <- collectAction(x, progress = progress)
    d <- cbind(d, dx)
  }
  
  # Add Committee
  if ("Committee" %in% include) {
    dx <- collectCommittee(x, progress = progress)
    d <- cbind(d, dx)
  }
  
  # Add Subject
  if ("Subject" %in% include) {
    dx <- collectSubject(x, progress = progress)
    d <- cbind(d, dx)
  }
  
  # Add Text
  if ("Text" %in% include) {
    d$Text <- collectText(x, progress = progress)
  }

  if (length(expand)!=1) stop("Length of expand is not 1!")
  
  if (expand != "None") {
    
    if (expand %in% c("Title",
                      "Summary",
                      "Sponsorship",
                      "Action",
                      "Committee",
                      "Subject",
                      "Text")) {
      
      # Get Expanded Part of Data
      if (expand=="Title") {
        etmp <- collectTitle(x, officialonly = FALSE, progress = progress)
      } else if (expand=="Summary") {
        etmp <- collectSummary(x, latestonly = FALSE, progress = progress)
      } else if (expand=="Sponsorship") {
        etmp <- collectSponsorship(x, single = FALSE, progress = progress)
      } else if (expand=="Action") {
        etmp <- collectAction(x, latestonly = FALSE, progress = progress)
      } else if (expand=="Committee") {
        etmp <- collectCommittee(x, single = FALSE, progress = progress)
      } else if (expand=="Subject") {
        etmp <- collectSubject(x, single = FALSE, progress = progress)
      } else if (expand=="Text") {
        etmp <- collectText(x, allinone=FALSE, progress = progress)
        cDF <- function(k) {
          c1 <- names(k)
          c2 <- unlist(k)
          c <- data.frame(Order = seq(1,length(k),1),
                          Section = c1,
                          Content = c2,
                          stringsAsFactors = FALSE)
          return(c)
        }
        etmp <- lapply(etmp, cDF)
      }
      
      
      # Expand the Current Data.Frame
      if (length(x)==1) {
        
        if (nrow(etmp)==0) {
          wtxt <- paste0("No ", expand, " Found. Cannot Expand Data.")
          stop(wtxt)
        }
        dtmp <- t(replicate(nrow(etmp), d, simplify = TRUE))
        colnames(etmp) <- paste(expand,colnames(etmp),sep="_")
        dtmp <- cbind(dtmp, etmp)
        
      } else {
        
        if (nrow(etmp[[1]])==0) {
          wtxt <- paste0("No ", expand, " Found for ", did$ID[1], " (Bill ", 1,"). Omit This Bill From Expanded Data.")
          warning(wtxt)
          dtmp <- d[-seq(1,nrow(d),1),] 
        } else {
          dtmp <- t(replicate(nrow(etmp[[1]]), d[1,], simplify = TRUE))
        }
        colnames(etmp[[1]]) <- paste(expand,colnames(etmp[[1]]),sep="_")
        dtmp <- cbind(dtmp, etmp[[1]])
        
        for (i in 2:length(etmp)) {
          
          if (nrow(etmp[[i]])==0) {
            wtxt <- paste0("No ", expand, " Found for ", did$ID[i], " (Bill ", i,"). Omit This Bill From Expanded Data.")
            warning(wtxt)
          } else {
            dtmpx <- t(replicate(nrow(etmp[[i]]), d[i,], simplify = TRUE))
            colnames(etmp[[i]]) <- paste(expand,colnames(etmp[[i]]),sep="_")
            dtmpx <- cbind(dtmpx, etmp[[i]])
            dtmp <- rbind(dtmp, dtmpx)
          }
          
        }
        
      }
      
      # Assign Back to Initial Data Frame
      d <- dtmp
      
    } else {

      stop("Invalid expand argument!")
      
    }

  }
  
  # Return Data.Frame
  for(k in 1:ncol(d)) if(is.list(d[,k])) d[,k] <- unlist(d[,k])
  return(d)  

}
