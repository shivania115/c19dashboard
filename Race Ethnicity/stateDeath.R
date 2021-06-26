install.packages("tidyr")
library(tidyr)

stateDeath1<-read.csv("https://data.cdc.gov/api/views/pj7m-y5uh/rows.csv?accessType=DOWNLOAD")
names(stateDeath1)[1] <- "date"
names(stateDeath1)[7] <- "state"
stateDeath1<-stateDeath1[,-c(2:5,16)]
stateDeath1<-subset(stateDeath1,Group=="By Total")
stateDeath1<-stateDeath1[,-c(2)]
transpose<-stateDeath1
names(transpose)[4]<-"White"
names(transpose)[5]<-"African American"
names(transpose)[6]<-"American Natives"
names(transpose)[7]<-"Asian"
names(transpose)[8]<-"NHPI"
names(transpose)[9]<-"Non Hispanic"
names(transpose)[10]<-"Hispanic"
transpose1<-transpose%>%pivot_longer(cols=c(4:10))
names(transpose1)[5]<-"totalDeaths"
names(transpose1)[4]<-"race"
transpose1$FIPS<-fips(transpose1$state)
transpose1$FIPS[is.na(transpose1$FIPS)]<-"_nation"
transpose1$totalDeaths[is.na(transpose1$totalDeaths)] <- -9999
transpose2<-subset(transpose1,Indicator=="Count of COVID-19 deaths")
transpose2<-transpose2[,-c(3)]
names(transpose2)[4]<-"covidDeaths"
transpose2$coviddeathDistribution<-transpose1$totalDeaths[transpose1$Indicator=="Distribution of COVID-19 deaths (%)"]
transpose2$popDistribution<-transpose1$totalDeaths[transpose1$Indicator=="Unweighted distribution of population (%)"]
transpose2$weighteddeathDistribution<-transpose1$totalDeaths[transpose1$Indicator=="Weighted distribution of population (%)"]
transpose2<-transpose2[,c(1:2,5,3:4,6:8)]

transpose2<-transpose2[-c(239:245),]
write.csv(transpose2,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/stateDeaths.csv")
