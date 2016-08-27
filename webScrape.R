
# scrape the web site r-users.com
library(data.table)
library(XML)

# create the list of urls for each page
# format: 
# https://www.r-users.com/jobs/page/2/
pageNum <- c(2:10)

createUrl <- function (x) {
  
  url <- paste("https://www.r-users.com/jobs/page/", x, "/", sep = "")
  #print url
  data.frame(url)
}
urlList <- rbindlist(lapply(pageNum, createUrl), fill = TRUE)


## get the url content

# title
#//*[@id="mainContent"]/div[2]/ol/li[4]/dl/dd[2]/strong/a

# location
#//*[@id="mainContent"]/div/ol/li/dl/dd[3]/span

#date
#//*[@id="mainContent"]/div[2]/ol/li[4]/dl/dd[4]

library("httr")

##
# for the html document, get the relevant x path values.
getXPathValues <- function(doc, xpathLoc) {
  locations <- getNodeSet(doc, xpathLoc)

  # get the value
  result <- sapply(locations, function(x) { xmlValue(x)})  
}

## Get the URL content
getUrlContent <- function(url) {
  #doc <- htmlParse(url, isURL = TRUE)
  doc <- htmlParse(rawToChar(GET(url)$content))
  #print(doc)
  #get the locations using the docment's xpath
 
  # job location 
  jobLocations <- getXPathValues(doc, '//*[@id="mainContent"]/div/ol/li/dl/dd[3]/strong')
  #print(jobLocations)
 
  #job country
  # if the location is "AnyWhere", the country is empty.
  # so list is of different size.
  jobCountryXPath = '//*[@id="mainContent"]/div[2]/ol/li[2]/dl/dd[3]/span'
  jobCountry <- getXPathValues(doc, jobCountryXPath)
  #print(jobCountry)
  
  # title
  titleDocXPath = '//*[@id="mainContent"]/div/ol/li/dl/dd[2]/strong/a'
  jobTitle <- getXPathValues(doc, titleDocXPath)
  #print(jobTitle)
  
  # get the date
  dateDocXPath = '//*[@id="mainContent"]/div/ol/li/dl/dd[4]/strong'
  jobDate <- getXPathValues(doc, dateDocXPath)
  #print(jobDate)
  
  # job year
  yearDocXPath = '//*[@id="mainContent"]/div/ol/li/dl/dd[4]/span'
  jobYear <- getXPathValues(doc, yearDocXPath)
  #print(jobYear)
  
  jobYear <- paste(jobYear, jobDate)
  data.frame(title = jobTitle, location= jobLocations, date = jobYear)
  #data.frame(content)
}

#//*[@id="mainContent"]/div[1]/ol/li[6]/dl/dd[3]/span

#tt <- getUrlContent("http://www.r-users.com/jobs/page/10/" )
jobListing <- rbindlist(apply(urlList, 1, getUrlContent))

df <- data.frame(table(jobListing$location))
#df <- data.frame(jobListing)

# print in descending order
head(df[order(-df$Freq),])
#//*[@id="mainContent"]/div[2]/ol/li[4]/dl/dd[3]/strong

## get the listing of AnyWhere
t2 <- jobListing[location == "Anywhere",]
t3 <- t2[order(t2$date),]
