# <code>congressbills</code> Package
Developped by Gento Kato (Last Updated: 06/21/2019) 

## Description

Extract and manipulate bill information from [US Congress website](https://www.congress.gov). 
The package website is published [HERE](https://gentok.github.io/congressbills/).

## Installation

<code>devtools::install_github("gentok/congressbills")</code>

## Main Functions

* <code>searchBill</code> Search bills by keywords and other conditions.
* <code>getBill</code> Get bill summary page URL (or <code>scrapeBill</code> object) from bill and congress number.
* <code>scrapeBill</code> Scrape bill information from Congress website using bill summary page URL.
* <code>collectBillData</code> Collect information of Congress bill from <code>scrapeBill</code> object.
* <code>read_scrapeBill</code> Import a (or a list of) <code>scrapeBill</code> object saved as <code>.rds</code> file.
* <code>write_scrapeBill</code> Write a (or a list of) <code>scrapeBill</code> object into <code>.rds</code> file.

## Supplemental Functions

* <code>BillIDfromURL</code> Collect Bill ID from bill summary page URL(s).
* <code>collectBillID</code> Collect information that identifies bill from <code>scrapeBill</code> object.
* <code>collectTitle</code> Collect information regarding title(s) of the bill from <code>scrapeBill</code> object.
* <code>collectSummary</code> Collect information regarding summary of the bill from <code>scrapeBill</code> object.
* <code>collectSponsorship</code> Collect information regarding sponsorship of the bill from <code>scrapeBill</code> object.
* <code>collectAction</code> Collect information regarding actions of the bill from <code>scrapeBill</code> object.
* <code>collectCommittee</code> Collect information regarding committees considering the bill from <code>scrapeBill</code> object.
* <code>collectSubject</code> Collect information regarding subject field of the bill from <code>scrapeBill</code> object.
* <code>collectText</code> Collect information regarding the latest full text of the bill from <code>scrapeBill</code> object.

## Updates Log

* 12/06/2019 Version 0.0.1.002 bug fix in collectBillData function
* 06/21/2019 Version 0.0.1.001 adding searchBill and small change in collectAction output
* 03/19/2019 Version 0.0.0.003 bug fix and add BillIDfromURL function
* 03/19/2019 Version 0.0.0.002 bug fix and adding getBill function
* 03/18/2019 Version 0.0.0.001 released
