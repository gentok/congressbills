
# Include At Large (No Summary)
url1 <- "https://www.congress.gov/bill/116th-congress/house-bill/1218"
# Many co-sponsors
url2 <- "https://www.congress.gov/bill/109th-congress/house-bill/19"
# No co-sponsors
url3 <- "https://www.congress.gov/bill/109th-congress/house-bill/53"
# Senate bill
url4 <- "https://www.congress.gov/bill/116th-congress/senate-bill/47"
# many summaries
url5 <- "https://www.congress.gov/bill/114th-congress/house-bill/6450"
# Vetoed by Obama
url6 <- "https://www.congress.gov/bill/111th-congress/house-joint-resolution/64"
# Vetoed by Obama (Overridden)
url7 <- "https://www.congress.gov/bill/114th-congress/senate-bill/2040"
# Excessive number of actions
url8 <- "https://www.congress.gov/bill/115th-congress/house-bill/2"
# Two committees
url9 <- "https://www.congress.gov/bill/116th-congress/senate-bill/252"
# Three committees
url10 <- "https://www.congress.gov/bill/115th-congress/house-resolution/268"

url <- c(url1,url2,url3,url4,url5,url6,url7,url8,url9,url10)
k <- scrapeBill(url)

a <- collectBillID(k)
a <- collectTitle(k)
a <- collectSummary(k)
a <- collectSponsorship(k)
a <- collectAction(k)
a <- collectCommittee(k)
a <- collectSubject(k)
a <- collectText(k)

a <- collectTitle(k, officialonly = FALSE)
a <- collectSummary(k, latestonly = FALSE)
a <- collectSponsorship(k, single = FALSE)
a <- collectAction(k, latestonly = FALSE)
a <- collectCommittee(k, single = FALSE)
a <- collectSubject(k, single = FALSE)
a <- collectText(k, allinone = FALSE)

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

a <- collectBillData(k, expand="Title",progress=TRUE)
a <- collectBillData(k, expand="Summary",progress=TRUE)
a <- collectBillData(k, expand="Sponsorship",progress=TRUE)
a <- collectBillData(k, expand="Action",progress=TRUE)
a <- collectBillData(k, expand="Committee",progress=TRUE)
a <- collectBillData(k, expand="Subject",progress=TRUE)
a <- collectBillData(k, expand="Text",progress=TRUE)
