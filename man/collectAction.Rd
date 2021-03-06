% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collectBillData.R
\name{collectAction}
\alias{collectAction}
\title{Collecting Action Information from a \code{scrapeBill} Object.}
\usage{
collectAction(x, latestonly = TRUE, progress = FALSE)
}
\arguments{
\item{x}{A (or A list of) \code{scrapeBill} object created by \code{\link{scrapeBill}} function.}

\item{latestonly}{If \code{TRUE} (default), the function returns summary of 
latest actions (and introduced date) in single row. If \code{FALSE}, the function returns \code{data.frame} 
object of all actions and their dates and timings.}

\item{progress}{If \code{TRUE}, show progress bar for the processing of multiple bills.}
}
\value{
If \code{latestonly==TRUE}, returns \code{data.frame} object with following 
contents (single row):
\itemize{
  \item \code{IntroducedDate}: Date of Introduction in Actions Overview
  \item \code{ActLatestDate_Overview}: Date of Latest Action in Actions Overview
  \item \code{ActLatestText_Overview}: Overview Content of Latest Action in Actions Overview
  \item \code{ActLatestDate_All}: Date of Latest Action in All Actions
  \item \code{ActLatestText_All}: Detailed Content of Latest Action in All Actions
  \item \code{VoteTimes_House}: Number of Voting Opportunities in House
  \item \code{VoteLatestDate_House}: Date of Latest Vote Action in House
  \item \code{VoteLatestType_House}: Type of Latest Vote in House
  \item \code{VoteLatestText_House}: Content of Latest Vote in House
  \item \code{VoteTimes_Senate}: Number of Voting Opportunities in Senate
  \item \code{VoteLatestDate_Senate}: Date of Latest Action in Senate
  \item \code{VoteLatestType_Senate}: Type of Latest Vote in Senate
  \item \code{VoteLatestText_Senate}: Content of Latest Vote in Senate
  \item \code{RCTimes_House}: Number of Roll Call Voting Opportunities in House
  \item \code{RCLatestDate_House}: Date of Latest Roll Call in House
  \item \code{RCLatestText_House}: Content of Latest Roll Call Vote in House
  \item \code{RCLatestNumber_House}: ID Number of Latest Roll Call Vote in House
  \item \code{RCLatestYea_House}: Number of Yea in Latest Roll Call Vote in House
  \item \code{RCLatestNay_House}: Number of Nay in Latest Roll Call Vote in House
  \item \code{RCTimes_Senate}: Number of Roll Call Voting Opportunities in Senate
  \item \code{RCLatestDate_Senate}: Date of Latest Roll Call in Senate
  \item \code{RCLatestText_Senate}: Content of Latest Roll Call Vote in Senate
  \item \code{RCLatestNumber_Senate}: ID Number of Latest Roll Call Vote in Senate
  \item \code{RCLatestYea_Senate}: Number of Yea in Latest Roll Call Vote in Senate
  \item \code{RCLatestNay_Senate}: Number of Nay in Latest Roll Call Vote in Senate
}

If \code{latestonly==FALSE}, A \code{data.frame} object containing following variables:
\itemize{
  \item \code{Chamber}: Chamber where the Action is Placed.
  \item \code{Date}: Date of Action
  \item \code{Time}: Time (in Day) of Action
  \item \code{Detail}: Detailed Content of Action
  \item \code{Overview}: Summarized Overview Content of Action (If Available)
  \item \code{Amendment}: 1 if Actions is related to amendment of the bill
  \item \code{VoteType}: Type of Vote (if any decision is made)
  \item \code{rollnumber}: Roll Call Number if Action is Roll Call Vote
  \item \code{rollYea}: Roll Call Yea Votes if Action is Roll Call Vote
  \item \code{rollNay}: Roll Call Nay Votes if Action is Roll Call Vote
}
}
\description{
Collect information regarding action(s) made on the bill.
}
\examples{
tgturl <- "https://www.congress.gov/bill/115th-congress/house-bill/2"
bill2 <- scrapeBill(tgturl)
collectAction(bill2)
 
}
\seealso{
\code{\link{scrapeBill}} and \code{\link{getscrapeBill}}
}
