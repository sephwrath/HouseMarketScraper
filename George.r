setwd("C:\\Users\\Stephen\\Documents\\helping george")

install.packages("rjson")
install.packages("RCurl")
install.packages("httr")

library("rjson")
library("RCurl")

nChk<- function(var) {
  ifelse(is.null(var), NA, var)
}

houseList <- "https://services.realestate.com.au/services/listings/search?query={%22channel%22:%22new_homes%22
  ,%22filters%22:{%22excludeProjectListings%22:true,%22surroundingSuburbs%22:true,%22priceRange%22:{%22minimum%22
  :%22any%22,%22maximum%22:%22any%22},%22bedroomsRange%22:{%22minimum%22:%22any%22,%22maximum%22:%22any%22}
  ,%22minimumBathroom%22:%22any%22,%22minimumCars%22:%22any%22},%22pageSize%22:200,%22page%22:%22"
houseList2 <- "%22,%22sortType%22:%22relevance%22,%22localities%22:[{%22locality%22:%22wa%22}]}"

viewsUrl <- "https://www.realestate.com.au/pdpvisits.ds?id="

remainingPages = 1
currentPage = 1

houseDf<-data.frame()

while(remainingPages > 0) {
  houseJsonUrl <- paste(houseList, currentPage, houseList2, sep="")
  
  json_data <- fromJSON(readLines(houseJsonUrl))
  
  resultsList <- json_data$tieredResults[[1]]$results
  # loop through each of the results in results list
  for (item in resultsList) {
    
    # get the number or views
    listingId <- item$listingId
    webpage <- getURL(paste(viewsUrl, listingId, sep=""))
    print(paste("View count request for: ",  listingId, sep=""))
    
    number <- sub(".*page-views__page-views-box-count\">(.*?)<.*", "\\1", webpage, ignore.case = T)
    print(paste("Request result: ", number, sep=""))
    
    # need to do this for each of the list items
    df<-data.frame(id = item$listingId
                   , price = nChk(item$price$display)
                   , title = nChk(item$title)
                   , streetAddress = nChk(item$address$streetAddress)
                   , locality = nChk(item$address$locality)
                   , postCode = nChk(item$address$postcode)
                   , agencyName = nChk(item$agency$name)
                   , agencyId = nChk(item$agency$agencyId)
                   , agencyListingId = nChk(item$agencyListingId)
                   , bathrooms=nChk(item$generalFeatures$bathrooms$value)
                   , bedrooms= nChk(item$generalFeatures$bedrooms$value)
                   , parkingSpaces = nChk(item$generalFeatures$parkingSpaces$value)
                   , views=as.numeric(gsub(",", "", number))
                   , modifiedDate = nChk(item$modifiedDate$value)
                   , lat=nChk(item$address$location$latitude) # put whatever you like here
                   , lng=nChk(item$address$location$longitude))

    #tempList<-do.call(rbind, df)
    houseDf<-rbind(houseDf, df)
    
    ## old cold that might get used if I cant get it to work the other way
    # Process escape characters
    #webpage <- readLines(tc <- textConnection(webpage)); close(tc)
    # Parse the html tree, ignoring errors on the page
    #pagetree <- htmlTreeParse(webpage, error=function(...){})
  }
  
  # this isn't in a loop
  if (currentPage == 1) {
    recordCount <- json_data$totalResultsCount
    remainingPages = recordCount/200
  }
  currentPage = currentPage + 1
  remainingPages = remainingPages - 1
  print(paste("current page ", currentPage, sep=""))
  
} 

## output temp files

write.csv(houseDf, "houseList.csv")
