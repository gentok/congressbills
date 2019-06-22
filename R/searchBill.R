#' Search Bills
#' 
#' @description Searching Bill Information in \href{https://www.congress.gov}{US Congress Website}.
#' 
#' @param key A \code{character} element of keywords.
#' @param congress A \code{character} vector of target congress(es) (e.g, \code{c(116,115)}).
#' @param chamber A \code{character} of target chamber. One of \code{"House"}, \code{"Senate"}, and \code{"Both"}. The default is both chambers.
#' @param billtype A \code{character} vector of target bill types. Choose any combination of 
#' \code{"bills"}, \code{"amendments"}, \code{"resolutions"}, \code{"joint-resolutions"}, and 
#' \code{"concurrent-resolutions"}. If \code{NULL}, include all types. The defauls is \code{"bills"}.
#' @param billstatus A \code{character} vector of target bill statuses. Choose any combination of 
#' \code{"introduced"}, \code{"committee"},\code{"floor"},\code{"failed-one"},\code{"passed-one"},
#' \code{"passed-both"},\code{"resolving"},\code{"president"},\code{"veto"}, and \code{"law"}.
#' If \code{NULL} (default), include bills of all statuses.
#' @param subject A \code{character} vector of policy subject areas. Choose any combination of subjects provided in 
#' \href{https://www.congress.gov/advanced-search/subject-policy-area}{Subject Area List}. If \code{NULL} (default), 
#' include all policy areas.
#' @param house_committee A \code{character} vector of House committees. Choose any combination of 
#' committees from \href{https://www.congress.gov/advanced-search/choose-committees?chamber=House}{House Committee List}.
#' @param senate_committee A \code{character} vector of Senate committees. Choose any combination of 
#' committees from \href{https://www.congress.gov/advanced-search/choose-committees?chamber=Senate}{Senate Committee List}.
#' @param party A \code{character} vector of parties of a sponsor. Choose any combination of 
#' \code{"Democratic"},\code{"Republican"},\code{"Independent"} and \code{"Independent Democrat"}.
#' If \code{NULL}, include any party.  
#' @param pageSort The way the bills in search are sorted. Choose one of 
#' \code{"relevancy"}, \code{"title"}, \code{"documentNumber"} (default), 
#' \code{"lawNumber"}, \code{"latestAction"}, and \code{"dateOfIntroduction"}. 
#' @param pageSort_direction Direction of \code{pageSort}. Choose \code{"descending"} (default) or \code{"ascending"}. 
#' Not applied if \code{pageSort} is one of \code{"relevancy"} and \code{"title"}.
#' @param maxN The max number of bills to import. The default is \code{1000}.
#' @param urlonly if \code{TRUE}, export character vector or bill summary page URLs. 
#' If \code{FALSE}, export \code{data.frame} of bill URLs and other extractable information from search page.
#' @param searchURLprint If \code{TRUE}, return search page URL.
#' 
#' @return If \code{urlonly==FALSE} and \code{searchURLprint==FALSE}, 
#' A \code{data.frame} that contains following variables.
#' \itemize{
#'   \item \code{ID}: Bill ID (See \code{\link{BillIDfromURL}})
#'   \item \code{URL}: Bill summary page URLs
#'   \item \code{Title}: Bill title
#'   \item \code{Sp_Chamber}: Chamber (i.e., House or Senate) of sponsor
#'   \item \code{Sp_Name}: Sponsor full name
#'   \item \code{Sp_LastName}: Sponsor last name
#'   \item \code{Sp_Party}: Party of sponsor
#'   \item \code{Sp_State}: State of sponsor
#'   \item \code{Sp_District}: District of sponsor
#'   \item \code{Cosp_N}: Number of cosponsors
#'   \item \code{IntroducedDate}: Date of introduction
#'   \item \code{ActLatestDate}: Date of the latest action
#'   \item \code{ActLatestText}: Content of the latest action
#'   \item \code{Committees}: Name of committees
#' }
#' 
#' @examples
#' # Search all bills (importing only newest 10)
#' billdt <- searchBill(maxN=10)
#' # Seach bills with keyword "air" (importing newest 100)
#' billdt <- searchBill(key="air", maxN=100)
#' 
#' 
#' @seealso \code{\link{getBill}}, \code{\link{read_scrapeBill}}, and \code{\link{write_scrapeBill}}
#' 
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom pbapply pblapply
#' @importFrom stringr str_split
#' @importFrom stringr str_squish
#' @importFrom stringr str_extract
#' 
#' @export

searchBill <- function(key = NULL,
                       congress = NULL,
                       chamber = "Both",
                       billtype = "bills",
                       billstatus = NULL,
                       subject = NULL,
                       house_committee = NULL,
                       senate_committee = NULL,
                       party = NULL,
                       pageSort = "documentNumber",
                       pageSort_direction = "descending",
                       maxN = 1000,
                       urlonly = FALSE,
                       searchURLprint = FALSE
                       ) 
{
  # base URL text
  base_txt <- "https://www.congress.gov/search?searchResultViewType=compact&pageSize=250"
  
  # page sort criteria
  if (is.null(pageSort)) {
    stop("pageSort must be assigned.")
  } else {
    if (pageSort %in% c("relevancy","title")) {
      (pageSort_txt <- paste0("&pageSort=",pageSort))
    } else {
      pageSort <- paste0(pageSort,"%3A",gsub("ending","",pageSort_direction))
      (pageSort_txt <- paste0("&pageSort=",pageSort))
    }
  }
  
  # keywords 
  if (is.null(key)) {
    (key_txt <- "")
  } else {
    key <- gsub(" ", "+", key) 
    (key_txt <- paste0('%2C"search"%3A"', key,'"'))
  }

  # congress
  if (is.null(congress)) {
    (congress_txt <- "")
  } else {
    congress <- paste0('"',congress,'"')
    (congress_txt <- paste0('%2C"congress"%3A[',paste(congress,collapse="%2C"),']'))
  }
  
  # chamber
  if (length(chamber)>1) stop("Length of chamber must be 1.")
  if (is.null(chamber)) {
    (chamber_txt <- "")
  } else if (chamber=="Both") {
    (chamber_txt <- "")
  } else if (chamber%in%c("House","Senate")) {
    (chamber_txt <- paste0('%2C"chamber"%3A"',chamber,'"'))  
  } else {
    stop("Invalid Values of chamber.")
  }
  
  # Bill Type
  if (is.null(billtype)) {
    billtype_txt <- ""
  } else {
    billtype <- paste0('"',billtype,'"')
    (billtype_txt <- paste0('%2C"type"%3A[',paste(billtype,collapse="%2C"),']'))
  }
  
  # Bill Status
  if (is.null(billstatus)) {
    billstatus_txt <- ""
  } else {
    billstatus <- paste0('"',billstatus,'"')
    (billstatus_txt <- paste0('%2C"bill-status"%3A[',paste(billstatus,collapse="%2C"),']'))
  }
  
  # Subject
  if (is.null(subject)) {
    subject_txt <- ""
  } else {
    subject <- paste0('"',gsub(" ","+", subject),'"')
    (subject_txt <- paste0('%2C"subject"%3A[',paste(subject,collapse="%2C"),']'))
  }
  
  # House Committee
  if (is.null(house_committee)) {
    house_committee_txt <- ""
  } else {
    house_committee <- paste0('"',gsub(" ","+", house_committee),'"')
    (house_committee_txt <- paste0('%2C"house-committee"%3A[',paste(house_committee,collapse="%2C"),']'))
  }
  
  # Senate Committee
  if (is.null(senate_committee)) {
    senate_committee_txt <- ""
  } else {
    senate_committee <- paste0('"',gsub(" ","+", senate_committee),'"')
    (senate_committee_txt <- paste0('%2C"senate-committee"%3A[',paste(senate_committee,collapse="%2C"),']'))
  }
  
  # Party
  if (is.null(party)) {
    party_txt <- ""
  } else {
    party <- paste0('"',gsub(" ","+", party),'"')
    (party_txt <- paste0('%2C"party"%3A[',paste(party,collapse="%2C"),']'))
  }
  
  # Search page URL
  searchURL <- paste0(base_txt, pageSort_txt, '&q={"source"%3A"legislation"', 
                      key_txt, congress_txt, chamber_txt, billtype_txt, billstatus_txt, 
                      subject_txt, house_committee_txt, senate_committee_txt, 
                      party_txt, "}")
  
  if(searchURLprint==TRUE) {
    return(cat(searchURL))
  }
  # First search page
  searchpage <- read_html(searchURL)
  
  # Extract Number of Bills
  nbills <- searchpage %>% html_nodes("span")
  nbills <- as.numeric(str_squish(gsub("^.*of |,","",nbills[which((nbills %>% html_attr("class")) == "results-number")] %>% html_text()))[1])
  # Determine N of bills to import
  nimports <- nbills
  if (nbills > maxN) nimports <- maxN
  cat(paste0(nbills, " legislations found. Import first ",nimports, "."))
  pages <- seq(1,ceiling(nimports / 250),1)
  
  # First Page Imports.
  infolist <- searchpage %>% html_nodes("li")
  infolist <- infolist[which((infolist %>% html_attr("class")) == "compact")]
  
  # Function to Get Bill information
  getOneBill <- function(infoset,urlonly) {
    
    # Bill Info Data for One Nodeset
    tmp <- infoset %>% html_nodes("span")
    tmpclass <- tmp %>% html_attr("class")
    
    # Get URL
    tmpURL <- gsub("\\?.*$","",tmp[which(tmpclass=="result-heading")] %>% html_nodes("a") %>% html_attr("href"))
    if(urlonly==TRUE) return(tmpURL)
    
    # Data of Extractable Information
    tmpdt <- data.frame(ID=character(1),
                        URL=character(1), 
                        Title=character(1),
                        Sp_Chamber=character(1),
                        Sp_Name=character(1),
                        Sp_LastName=character(1),
                        Sp_Party=character(1),
                        Sp_State=character(1),
                        Sp_District=character(1),
                        Cosp_N=NA,
                        IntroducedDate=character(1),
                        ActLatestDate=character(1),
                        ActLatestText=character(1),
                        Committees=NA)
    
    # Insert URL
    tmpdt$URL <- tmpURL
    
    # Get ID
    tmpdt$ID <- BillIDfromURL(tmpdt$URL)
    
    # Get Title
    tmpdt$Title <- tmp[grep("result-title", tmpclass)[1]] %>% html_text
    
    # Other Info Items
    tmpitems <- tmp[which(tmpclass=="result-item")] %>% html_text
    
    # Get Sponsorship & Introduction Date Information
    if (length(grep("Sponsor:",tmpitems))==1) {
      
      # Sponsorship Data
      tmpsp <- str_squish(gsub("Sponsor:","",tmpitems[grep("Sponsor:",tmpitems)]))
      
      # Representative or Senate
      tmpdt$Sp_Chamber <- sapply(str_split(tmpsp," "), function(k) k[1])
      tmpdt$Sp_Chamber <- gsub("Rep.", "House", tmpdt$Sp_Chamber)
      tmpdt$Sp_Chamber <- gsub("Sen.", "Senate", tmpdt$Sp_Chamber)
      # Party, State, District
      psdinfo <- str_extract(tmpsp, "\\[+[[:upper:][:digit:]-]+(|At Large)+\\]")
      psdinfo <- str_split(gsub("\\[|\\]", "", psdinfo), "-")
      tmpdt$Sp_Party <- sapply(psdinfo, function(k) k[1])
      tmpdt$Sp_State <- sapply(psdinfo, function(k) k[2])
      tmpdt$Sp_District <- as.character(sapply(psdinfo, function(k) k[3]))
      # Clean Names
      tmpdt$Sp_Name <- gsub("\\*$", "", tmpsp)
      tmpdt$Sp_Name <- gsub("^Rep. ", "", tmpdt$Sp_Name)
      tmpdt$Sp_Name <- gsub("^Sen. ", "", tmpdt$Sp_Name)
      tmpdt$Sp_Name <- gsub(" \\[+[[:upper:][:digit:]-]+(|At Large)+\\].*$", "", tmpdt$Sp_Name)
      # Last Name
      tmpdt$Sp_LastName <- sub(",.*$", "", tmpdt$Sp_Name)
      
      # Introduction Date
      tmpdt$IntroducedDate <- gsub(" Cosponsors.*$", "", tmpsp)
      tmpdt$IntroducedDate <- gsub("^.*Introduced |\\)$", "", tmpdt$IntroducedDate)
      
      # Number of Cosponsors
      tmpdt$Cosp_N <- as.numeric(gsub("^.* Cosponsors: \\(|\\)", "", tmpsp))
      
    }
    
    # Get Committees
    if (length(grep("Committees:",tmpitems))==1) {
      
      tmpdt$Committees <- str_squish(gsub("Committees:","",tmpitems[grep("Committees:",tmpitems)]))
      
    }  
    
    # Get Latest Actions
    if (length(grep("Latest Action:",tmpitems))==1) {
      
      tmpact <- str_squish(gsub("Latest Action:|\\(All Actions\\)","",tmpitems[grep("Latest Action:",tmpitems)]))
      tmpdt$ActLatestDate <- str_extract(tmpact, "[:digit:]{2}/[:digit:]{2}/[:digit:]{4}")
      tmpdt$ActLatestText <- gsub("[:digit:]{2}/[:digit:]{2}/[:digit:]{4} ","", tmpact)
      
    }  
    
    return(tmpdt)
    
  }
  
  if (length(pages)>1) {
    
    addinfolist <- function(page){
      searchpage <- read_html(paste0(searchURL,"&page=",page))
      infolist <- searchpage %>% html_nodes("li")
      infolist <- infolist[which((infolist %>% html_attr("class")) == "compact")]
    }
    
    cat("\n\n Scraping search pages...\n")
    infolists <- pblapply(pages[-1], addinfolist)
    
  }
  
  cat("\n\n Getting URL from search page 1...\n")
  billlist <- pblapply(infolist, getOneBill, urlonly=urlonly)
  
  if (length(pages)>1) {
    for (i in pages[-1]) {
      cat(paste0("\n\n Getting URL from search page ", i, "...\n"))
      tmp <- pblapply(infolists[[i-1]], getOneBill, urlonly=urlonly)
      billlist <- c(billlist, tmp)
    }
  }
  
  # Return Results
  if (urlonly==TRUE) {
    res <- unlist(billlist[1:nimports,])
  } else {
    # Convert into Data Frame
    res <- do.call("rbind", billlist)[1:nimports,]
  }
  return(res)
}

