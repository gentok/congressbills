% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrapeBill.R
\name{getBill}
\alias{getBill}
\alias{getscrapeBill}
\alias{getBillDF}
\alias{getscrapeBillDF}
\title{Get Bill URL or HTML by Bill Number}
\usage{
getBill(number, congress, chamber = "House", type = "Bill",
  out = "URL")

getscrapeBill(number, congress, chamber = "House", type = "Bill")

getBillDF(infoDF, out = "URL", progress = FALSE)

getscrapeBillDF(infoDF, progress = TRUE)
}
\arguments{
\item{number}{\code{numeric} scalar of Bill number}

\item{congress}{\code{numeric} scalar of Congress number}

\item{chamber}{Choose from \code{"House"} (default) or \code{"Senate"}}

\item{type}{Choose from \code{"Bill"} (default), \code{"Simple Resolution"}, 
\code{"Joint Resolution"}, \code{"Concurrent Resolution"}, and 
\code{"Amendment"}}

\item{out}{The output type. \code{"URL"} (default) to export bill summary page URL 
and \code{"scrapeBill"} to export \code{"scrapeBill"} object.}

\item{infoDF}{(Only used in \code{getBillDF} function) \code{data.frame} object with 
columns \code{number}, \code{congress}, \code{chamber}, and \code{type}. Rows should 
represent individual bills.}

\item{progress}{(Only used in \code{getBillDF} function) If \code{TRUE}, show progress bar for the processing of multiple bills.}
}
\value{
\code{character} scalar of URL or \code{"scrapeBill"} object.
}
\description{
Get Bill Suummary Page URL or \code{scrapeBill} object by Bill Number and Congress Number.
}
\examples{
a <- getBill(1, 103)
b <- scrapeBill(a)
collectBillID(b)

a <- getBill(1, 103, "Senate", "Joint Resolution")
b <- scrapeBill(a)
collectBillID(b)

a <- getBill(1, 101, out="scrapeBill")
collectBillID(a)

a <- data.frame(number = c(1,2,3,4),
                congress = c(103,104,108,109),
                chamber = "House",
                type = "Bill")
b <- getBillDF(a, out="scrapeBill")
collectBillID(b)
                
}
\seealso{
\code{\link{scrapeBill}}, \code{\link{read_scrapeBill}}, and \code{\link{write_scrapeBill}}
}
