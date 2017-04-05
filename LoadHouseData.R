setwd("C:/Users/st244584/RProjects/LdnHouseMkt")

install.packages("rjson")

library("rjson")

nChk<- function(var) {
      ifelse(is.null(var), NA, var)
}
mapFile <- "http://www.rightmove.co.uk/ajax/property-for-sale/map-search.html?_includeSSTC=off&auction=false&lastPersistLocId=REGION%5E87490&locationIdentifier=REGION%5E87490&maxPrice="
mapFile2 <- "&numberOfPropertiesPerPage=24&previousSearchLocation=London&radius=20.0&searchLocation=London&searchType=SALE&sortType=2&topMatchPersistRegIds=87490&useLocationIdentifier=false&viewType=LIST"

propHtml<-"http://www.rightmove.co.uk/api/_search?locationIdentifier=REGION%5E87490&maxPrice="
propHtml2<-"&numberOfPropertiesPerPage=50&radius=20.0&sortType=2&index="
propHtml3<-"&includeSSTC=false&viewType=LIST&channel=BUY&areaSizeUnit=sqft&currencyCode=GBP&isFetching=false"

price = 200000

fullList<-data.frame()
houseList<-data.frame()

while(price < 400000) {
      priceStr <- paste(format(price + 500, scientific = F), "&minPrice=", format(price+1, scientific = F), sep="")
      json_file <- paste(mapFile, priceStr, mapFile2, sep="")
      json_data <- fromJSON(readLines(json_file))
      print(paste("price str: ", priceStr, sep=""))
      ##Sys.sleep(1)
      
      print(paste("json location values: ", length(json_data$mappedProperties), sep=""))
      
      ## http://www.rightmove.co.uk/api/_search?locationIdentifier=REGION%5E87490&numberOfPropertiesPerPage=24&radius=10.0&sortType=2&index=72&includeSSTC=false&viewType=LIST&channel=BUY&areaSizeUnit=sqft&currencyCode=GBP&isFetching=false
      
      df<-data.frame(id=NA, lat=NA, lng=NA)
      df<-lapply(json_data$mappedProperties, function(x, i) {
            data.frame(id=as.character(x$id)
                       , lat=x$latLng$lat
                       , lng=x$latLng$lng)
          })
      tempList<-do.call(rbind, df)
      fullList<-rbind(fullList, tempList)
      print(paste("location list: ", dim(fullList)[1], sep=""))
      
      ## get the detailed data
      index = 0
      
      while(!is.null(index) ) {
            json_file <- paste(propHtml, priceStr, propHtml2, format(index, scientific = F), propHtml3, sep="")
            json_data <- fromJSON(readLines(json_file))
            print(paste("index: ", index, sep=""))
            ##Sys.sleep(1)
            
            df<-data.frame(id=NA, bedrooms=NA, numberOfImages=NA, numberOfFloorplans=NA, summary=NA
                           , displayAddress=NA, propertySubType=NA, listingUpdateReason=NA
                           , listingUpdateDate=NA, premiumListing=NA, featuredProperty=NA, amount=NA, currencyCode=NA
                           , branchId=NA, contactTelephone=NA, branchDisplayName=NA, branchName=NA)
            
            print(paste("json prop values: ", length(json_data$properties), sep=""))
            df<-lapply(json_data$properties, function(x) {
                  ##x<-unlist(x)
                  data.frame(id=as.character(x$id)
                             , bedrooms=x$bedrooms
                             , numberOfImages=x$numberOfImages
                             , numberOfFloorplans=x$numberOfFloorplans
                             , summary=x$summary
                             , displayAddress=x$displayAddress
                             , propertySubType=x$propertySubType
                             , listingUpdateReason=nChk(x$listingUpdate$listingUpdateReason)
                             , listingUpdateDate=nChk(x$listingUpdate$listingUpdateDate)
                             , premiumListing=x$premiumListing
                             , featuredProperty=x$featuredProperty
                             , amount=x$price$amount
                             , currencyCode=x$price$currencyCode
                             , branchId=x$customer$branchId
                             , contactTelephone=x$customer$contactTelephone
                             , branchDisplayName=x$customer$branchDisplayName
                             , branchName=x$customer$branchName)
            })
            
            tempList<-do.call(rbind, df)
            houseList<-rbind(houseList, tempList)
            print(paste("detail:", dim(houseList)[1], sep=""))
            
            if (is.null(json_data$pagination[["next"]])) index<- NULL
            else index <- as.numeric(json_data$pagination[["next"]])
            print(paste("index post: ", index, sep=""))
      }
      
      ##Sys.sleep(4)
      
      price<- price + 500
}

## output temp files

write.csv2(fullList, "fullList12Jan.csv")
write.csv2(houseList, "houseList12Jan.csv")
