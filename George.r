houseList <- paste("https://services.realestate.com.au/services/listings/search?query={%22channel%22:%22new_homes%22",
  ",%22filters%22:{%22excludeProjectListings%22:true,%22surroundingSuburbs%22:true,%22priceRange%22:{%22minimum%22",
  ":%22any%22,%22maximum%22:%22any%22},%22bedroomsRange%22:{%22minimum%22:%22any%22,%22maximum%22:%22any%22}",
  ",%22minimumBathroom%22:%22any%22,%22minimumCars%22:%22any%22},%22pageSize%22:200,%22page%22:%22", sep="")
houseList2 <- "%22,%22sortType%22:%22relevance%22,%22localities%22:[{%22locality%22:%22wa%22}]}"

viewsUrl <- "https://www.realestate.com.au/pdpvisits.ds?id="

remainingPages = 1
currentPage = 1

houseDf<-data.frame()

while(remainingPages > 0) {
  houseJsonUrl <- paste(houseList, currentPage, houseList2, sep="")
  
  # I've put getURL here hopefully this works
  resp <- GET(houseJsonUrl,
      user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2"))
  json_data <- fromJSON(content(resp, as="text"))
  
  resultsList <- json_data$tieredResults[[1]]$results
  # loop through each of the results in results list
  for (item in resultsList) {
    
    listingId <- item$listingId
    
    # test if it exists in the list and only readd if it isn't there
    if (nrow(houseDf) == 0 || count(filter(houseDf, id == listingId)) != 1) {
      # get the number or views
      webpage <- getURL(paste(viewsUrl, listingId, sep=""))
      print(paste("View count request for: ",  listingId, sep=""))
      
      number <- sub(".*page-views__page-views-box-count\">(.*?)<.*", "\\1", webpage, ignore.case = T)
      print(paste("Request result: ", number, sep=""))
      
      # need to do this for each of the list items
      df<-data.frame(id = listingId
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
  }
  
  # this isn't in a loop
  if (currentPage == 1) {
    recordCount <- json_data$totalResultsCount
    remainingPages = recordCount/200
  }
  currentPage = currentPage + 1
  
  # modified to use the number of records in the dataFrame so it won't stop till the records in the df = the records
  # in the count - this is because duplicates seem to be returned from the page call for some reason
  remainingPages = remainingPages - 1
  print(paste("current page ", currentPage, sep=""))
  
} 

## output temp files
# there seem to be duplicates look into why this is
write.csv(unique(houseDf), "houseList.csv")
