library(readr)
malaria<-read_csv("labo database malaria.csv")

#subsetting out NAs in the Blood column
NAs2<-malaria[is.na(malaria$Blood),]
IDs<-unique(malaria$IDBirdBandingData)
IDs2<-unique(NAs2$IDBirdBandingData)
t<-IDs %in% IDs2
t2<-data.frame("IDs"=IDs,"test"=t)
w<-t2[t2$test==FALSE,]
w2<-w$IDs
new.malaria<-malaria[malaria$IDBirdBandingData %in% w2,]

#subsetting out birds for which blood was taken
blood<-new.malaria[new.malaria$Blood=="Y",]
