
#setwd("D:/temp")

# Include At Large (No Summary)
url1 <- getBill(1218, 116)
# Many co-sponsors
url2 <- getBill(19, 109)
# No co-sponsors
url3 <- getBill(53, 109)
# Senate bill
url4 <- getBill(47, 116, chamber = "Senate")
# many summaries
url5 <- getBill(6450, 114)
# Vetoed by Obama
url6 <- getBill(64, 111, type = "Joint Resolution")
# Vetoed by Obama (Overridden)
url7 <- getBill(2040, 114, chamber = "Senate")
# Excessive number of actions
url8 <- getBill(2, 115)
# Two committees
url9 <- getBill(252, 116, chamber = "Senate")
# Three committees
url10 <- getBill(268, 115, type = "Simple Resolution")
# Combine all
url <- c(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10)

# Scrape Data
k <- scrapeBill(url)

# Save Scraped Data (without auxiliary files)
write_scrapeBill(k, "test.rds")
# Write out raw xml files separately
write_scrapeBill(k, "test.rds", aux = "raw")
# Write out html files separately
write_scrapeBill(k, "test.rds", aux = "html")

# Read Data
k <- read_scrapeBill("test.rds")

# Single Row Functions
a <- collectBillID(k)
a <- collectTitle(k)
a <- collectSummary(k)
a <- collectSponsorship(k)
a <- collectAction(k)
a <- collectCommittee(k)
a <- collectSubject(k)
a <- collectText(k)

# Expanded Outputs of Individual Functions
a <- collectTitle(k, officialonly = FALSE)
a <- collectSummary(k, latestonly = FALSE)
a <- collectSponsorship(k, single = FALSE)
a <- collectAction(k, latestonly = FALSE)
a <- collectCommittee(k, single = FALSE)
a <- collectSubject(k, single = FALSE)
a <- collectText(k, allinone = FALSE)

# Universal FUnction (single row per bill)
a <- collectBillData(k)
a <- collectBillData(k, c("BillID",
                          "URL",
                          "Title",
                          "Summary",
                          "Sponsorship",
                          "Action",
                          "Committee",
                          "Subject",
                          "Text"), progress=TRUE)

# Expand the data.frame
a <- collectBillData(k, include="BillID", expand="Title",progress=TRUE)
a <- collectBillData(k, include="BillID", expand="Summary",progress=TRUE)
a <- collectBillData(k, include="BillID", expand="Sponsorship",progress=TRUE)
a <- collectBillData(k, include="BillID", expand="Action",progress=TRUE)
a <- collectBillData(k, include="BillID", expand="Committee",progress=TRUE)
a <- collectBillData(k, include="BillID", expand="Subject",progress=TRUE)
a <- collectBillData(k, include="BillID", expand="Text",progress=TRUE)
