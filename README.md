# <code>congressbills</code> Package
Developped by Gento Kato (Last Updated: 03/18/2019) 

## Description

Extract and manipulate bill information from [US Congress website](https://www.congress.gov).

## Installation

<code>devtools::install_github("gentok/congressbills")</code>

## Main Functions

* <code>scrapeBill</code> Scrape bill information from Congress website using bill page URL.
* <code>collectBillData</code> Collect information of Congress bill from <code>scrapeBill</code> object.
* <code>read_scrapeBill</code> Import a (or a list of) <code>scrapeBill</code> object saved as <code>.rds</code> file.
* <code>write_scrapeBill</code> Write a (or a list of) <code>scrapeBill</code> object into <code>.rds</code> file.

## Supplemental Functions

* <code>collectBillID</code> Collect information that identifies bill from <code>scrapeBill</code> object.
* <code>collectTitle</code> Collect information regarding title(s) of the bill from <code>scrapeBill</code> object.
* <code>collectSummary</code> Collect information regarding summary of the bill from <code>scrapeBill</code> object.
* <code>collectSponsorship</code> Collect information regarding sponsorship of the bill from <code>scrapeBill</code> object.
* <code>collectCommittee</code> Collect information regarding committees considering the bill from <code>scrapeBill</code> object.
* <code>collectSubject</code> Collect information regarding subject field of the bill from <code>scrapeBill</code> object.
* <code>collectText</code> Collect information regarding the latest full text of the bill from <code>scrapeBill</code> object.

## Updates Log

* 03/18/2019 Version 0.0.0.001 released