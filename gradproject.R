library(readr)
library(stringr)
library(dplyr)

#reading in dataframes
labo.prime<-read_csv("labo database malaria.csv")
blood.prime<-read_csv("Blood.csv")
PCR.prime<-read_csv("PCR.csv")

#subsetting out NAs in the Blood column of labo.prime
NAs.labo<-labo.prime[is.na(labo.prime$Blood),]
IDs<-unique(labo.prime$IDBirdBandingData)
IDs2<-unique(NAs.labo$IDBirdBandingData)
t<-IDs %in% IDs2
t2<-data.frame("IDs"=IDs,"test"=t)
w<-t2[t2$test==FALSE,]
w2<-w$IDs
new.labo<-labo.prime[labo.prime$IDBirdBandingData %in% w2,]

#subsetting out birds for which blood was taken in new.labo
blood.labo<-new.labo[new.labo$Blood=="Y",]

#subsetting out NAs in the FieldCode column in blood.prime
NAs.blood<-blood.prime[is.na(blood.prime$FieldCode),]
bandnumbers<-blood.prime$BandNumber
bandnumbers2<-NAs.blood$BandNumber
t.blood<-bandnumbers %in% bandnumbers2
t2.blood<-data.frame("BandNumbers"=bandnumbers,"test"=t.blood)
w.blood<-t2.blood[t2.blood$test==FALSE,]
w2.blood<-w.blood$BandNumbers
new.blood<-blood.prime[blood.prime$BandNumber %in% w2.blood,]

#comparing band numbers between new.blood and blood.labo
bledbands<-new.blood$BandNumber
matched.labo<-blood.labo[blood.labo$BandNumber %in% bledbands,]

#cleaning PCR.prime
PCR.prime$TakeOrLeave<-NA
test<-function(x)ifelse(test=str_detect(x,"SMD"),yes="take",no="leave")
for(i in 1:nrow(PCR.prime)){
  PCR.prime[i,]$TakeOrLeave<-test(PCR.prime[i,]$FieldPrefix)
}
new.PCR<-PCR.prime[PCR.prime$TakeOrLeave=="take",]
new.PCR$TakeOrLeave<-NULL

#creating FieldCode column in new.PCR
new.PCR$FieldCode<-NA
for(i in 1:nrow(new.PCR)){
  if(nchar(new.PCR[i,]$FieldID)>=2){
  new.PCR[i,]$FieldCode<-str_c(new.PCR[i,]$FieldPrefix,new.PCR[i,]$FieldID,sep="")
  }else{
    new.PCR[i,]$FieldCode<-str_c(new.PCR[i,]$FieldPrefix,new.PCR[i,]$FieldID,sep="0")}
}
new.PCR<-new.PCR[1:540,]

#comparing field codes between new.PCR and new.blood
analyzedblood<-new.PCR$FieldCode
matched.blood<-new.blood[new.blood$FieldCode %in% analyzedblood,]

#merging new.PCR and matched.blood
testmerge<-merge(matched.blood,new.PCR)
