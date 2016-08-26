
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
getUrlContent <- function(url) {
  #doc <- htmlParse(url, isURL = TRUE)
  doc <- htmlParse(rawToChar(GET(url)$content))
  #print(doc)
  #get the locations using the docment's xpath
  locations <- getNodeSet(doc, '//*[@id="mainContent"]/div/ol/li/dl/dd[3]/span')
  #print(locations)
  # get the value
  data.frame(sapply(locations, function(x) { xmlValue(x)}))
  
  #data.frame(content)
}

#//*[@id="mainContent"]/div[1]/ol/li[6]/dl/dd[3]/span

getUrlContent("http://www.r-users.com/jobs/page/10/" )
jobListing <- rbindlist(apply(urlList, 1, getUrlContent))

df <- data.frame(table(jobListing))

# print in descending order
head(df[order(-df$Freq),])
#//*[@id="mainContent"]/div[2]/ol/li[4]/dl/dd[3]/strong
