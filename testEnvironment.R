#https://api.openaq.org/v1/measurements?parameter= (ui.dropdown input, ex= pm25)
urlHeader <- "https://api.openaq.org/v1/measurements"
urlParameter<-urlHeader + "?parameter="
#append url with & date_to of slider input (use latest)

dataUrl <- "https://api.openaq.org/v1/measurements?Parameter=pm25&Date_to=2012-12-12"
airData <- GET(dataUrl)
airData <- content(airData,"text")
airData<- fromJSON(airData)
airData<- flatten(airData$results)
