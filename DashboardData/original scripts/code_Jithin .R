###########     SETTING WORKING DIRECTORY   ####################################

#get your working directory
getwd()


box1 = "/Users/air/Box/COVID19_data_shared" 
# I recommend setting a local directory even when there's 
# going to be no file called from your local directory if you're linked to the box1.

local = "/Users/air/Downloads"
onedrive="/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard"
R.Version()


############   DOWNLOADING REQUIRED PACKAGES   #################################

if (!require("base")){
  install.packages("base")}
if (!require("tidyverse")){
  install.packages("tidyverse")}
if (!require("tidyr")){
  install.packages("tidyr")}
if (!require("plyr")){
  install.packages("plyr")}
if (!require("readr")){
  install.packages("readr")}
if (!require("dplyr")){
  install.packages("dplyr")}
if (!require("data.table")){
  install.packages("data.table")}
if (!require("ggplot2")){
  install.packages("ggplot2")}
if (!require("tibble")){
  install.packages("tibble")}
if (!require("lubridate")){
  install.packages("lubridate")}
if (!require("cdlTools")){
  install.packages("cdlTools")}
if (!require("openintro")){
  install.packages("openintro")}
if (!require("gtools")){
  install.packages("gtools")}
if (!require("rvest")){
  install.packages("rvest")}
if (!require("dplyr")){
  install.packages("dplyr")}
if (!require("DescTools")){
  install.packages("DescTools")}
if (!require("readxl")){
  install.packages("readxl")}
if (!require("stringr")){
  install.packages("stringr")}
if (!require("naniar")){
  install.packages("naniar")}
if (!require("zoo")){
  install.packages("zoo")}




library(base)
library(tidyverse)
library(tidyr)
library(plyr)
library(readr)
library(data.table)
library(ggplot2)
library(tibble)
library(lubridate)
library(cdlTools)
library(openintro)
library(gtools)
library(rvest)
library(dplyr)
library(DescTools)
library(readxl)
library(stringr)  
library(naniar)
library(zoo)
################  ALASKA ################

alaska <- read.csv("https://opendata.arcgis.com/datasets/797cbc3e398241a2b11e76fc06dd2b8b_0.csv") %>%
  select(Hospitalized_Cases__Cumulative_,Date_Reported) %>% dplyr::rename(hospTot = Hospitalized_Cases__Cumulative_,
                                                                          date = Date_Reported) %>%
  mutate(date = as.Date(date,origin = "1899-12-30"), statename = "Alaska")

#lastbarchart<-read.csv("./DataUpload/Yubin/lastbarcharts_merged.csv")
#lastbarchart$quintileVar[merged$quintileVa=="cdk"] <- "ckd"
#write.csv(lastbarchart,"./DataUpload/Yubin/lastbarcharts_merged.csv")


####################  MICHIGAN  ##########################

webpage <- read_html("https://www.michigan.gov/coronavirus/0,9753,7-406-98159-523641--,00.html")
tbls <- html_nodes(webpage, "table")
tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[6] %>%
  html_table(fill = TRUE)

datetext <- html_nodes(webpage, "caption")
datetext_ls <- webpage %>%
  html_nodes("caption") %>%
  .[6]
update <- html_text(datetext_ls)
string <- "Patient Census as of "
updatetext <- sub(string,"",update)


michiganhosp <- as_tibble(tbls_ls[[1]]) %>% dplyr::rename(date = "Hospital", hospDaily = "COVID-19 Patients", icuCOVID = "COVID-19 Patients in ICU",percentBedOcc = "Bed Occupancy %") %>%
  filter(date == "Grand Total") %>% mutate(date = updatetext) %>% mutate(date = as.Date(date,format = "%m/%d/%Y"))


rm(webpage,datetext,datetext_ls,tbls,tbls_ls,string,update)


webpage <- read_html("https://www.michigan.gov/coronavirus/0,9753,7-406-98159-523641--,00.html")
tbls <- html_nodes(webpage, "table")
tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

names(tbls_ls[[1]]) <- c("none","Hospital Beds", "Adult \r\n\r\n\t\t\tHospital Beds",
                         "Hospital \r\n\r\n\t\t\tInpatient Beds",
                         "Hospital \r\n\r\n\t\t\tInpatient Bed Occupancy" ,
                         "Adult \r\n\r\n\t\t\tHospital \r\n\r\n\t\t\tInpatient Beds",
                         "Adult Hospital  \r\n\r\n\t\t\tInpatient Bed Occupancy",
                         "ICU  Beds" ,
                         "ICU Bed Occupancy",
                         "Adult ICU Beds",
                         "Adult ICU Bed Occupancy",
                         "Total Ventilators" ,
                         "Mechanical  \r\n\r\n\t\t\tVentilators  in use")

michiganbeds <- as_tibble(tbls_ls[[1]]) %>% dplyr::rename(date = "none", allBeds = "Hospital Beds", inpatientBedOccupancy = "Hospital \r\n\r\n\t\t\tInpatient Bed Occupancy", icuBeds = "ICU  Beds", icuBedsOccupancy="ICU Bed Occupancy",ventilators= "Total Ventilators",ventilatorsInUse="Mechanical  \r\n\r\n\t\t\tVentilators  in use") %>%
  filter(date == "Total") %>% mutate(date = updatetext) %>% mutate(date = as.Date(date,format = "%m/%d/%Y")) %>%
  select(date ,allBeds, inpatientBedOccupancy, icuBeds, icuBedsOccupancy,ventilators,ventilatorsInUse)


michigan_pre <- full_join(michiganhosp,michiganbeds)

dateHosp = michigan_pre$date

webpage <- read_html("https://www.michigan.gov/coronavirus/0,9753,7-406-98159-523641--,00.html")
tbls <- html_nodes(webpage, "table")
tbls_ls <- webpage %>%
  html_nodes("table") %>%
  .[2] %>%
  html_table(fill = TRUE)

datetext <- html_nodes(webpage, "caption")
datetext_ls <- webpage %>%
  html_nodes("caption") %>%
  .[2]
update <- html_text(datetext_ls)


michiganhospt <- as_tibble(tbls_ls[[1]]) %>% select("HCC Region",Total)
michiganhospt2 <- spread(michiganhospt, key="HCC Region", value=Total) %>%
  select("Adult Confirmed-Positive\r\n\t\t\tCOVID","ICU Adult Confirmed-\r\n\t\t\tPositive COVID","Hospitalized Ped\r\n\t\t\tConfirmed-Positive") %>%
  dplyr::rename(adultCOVIDHosp ="Adult Confirmed-Positive\r\n\t\t\tCOVID",icuAdmDaily="ICU Adult Confirmed-\r\n\t\t\tPositive COVID",pedsCOVIDHosp="Hospitalized Ped\r\n\t\t\tConfirmed-Positive") %>%
  mutate(date=as.Date(dateHosp,format = "%m/%d/%Y")) %>%
  select(-adultCOVIDHosp,-pedsCOVIDHosp)

michigan <- full_join(michigan_pre,michiganhospt2) %>% mutate(hospDaily=as.integer(gsub(",","",hospDaily)),
                                                              allBeds=as.integer(gsub(",","",allBeds)),
                                                              inpatientBedOccupancy=as.integer(gsub(",","",inpatientBedOccupancy)),
                                                              icuBeds=as.integer(gsub(",","",icuBeds)),
                                                              icuBedsOccupancy=as.integer(gsub(",","",icuBedsOccupancy)),
                                                              ventilators=as.integer(gsub(",","",ventilators)),
                                                              ventilatorsInUse=as.integer(gsub(",","",ventilatorsInUse)),
                                                              statename="Michigan")
str(michigan)

rm(webpage,michiganbeds,dateHosp,michiganhospt,updatetext,michiganhosp,michiganhospt2,datetext,datetext_ls,tbls,tbls_ls,update,michigan_pre)

#####################  Texas   ####################################
url<-"https://dshs.texas.gov/coronavirus/TexasCOVID-19HospitalizationsOverTimebyTSA.xlsx"
Texas_hosptot<-openxlsx::read.xlsx(url,sheet=1)
names(Texas_hosptot) <- lapply(Texas_hosptot[2, ], as.character)
Texas_hosptot <- Texas_hosptot[25,]
Texas_hosptot <- rbind(names(Texas_hosptot), Texas_hosptot)
Texas_hosptot<-transpose(Texas_hosptot)
Texas_hosptot<- Texas_hosptot[-2,]
Texas_hosptot<- Texas_hosptot[-1,]
names(Texas_hosptot)[names(Texas_hosptot) == "V1"] <- "date"
names(Texas_hosptot)[names(Texas_hosptot) == "V2"] <- "hospTot"
Texas_hospital<-openxlsx::read.xlsx(url,sheet=4)
names(Texas_hospital) <- lapply(Texas_hospital[2, ], as.character)
Texas_hospital <- Texas_hospital[25,]
Texas_hospital <- rbind(names(Texas_hospital), Texas_hospital)
Hospital_transposed<-transpose(Texas_hospital)
Hospital_transposed<- Hospital_transposed[-2,]
Hospital_transposed<- Hospital_transposed[-1,]
names(Hospital_transposed)[names(Hospital_transposed) == "V1"] <- "date"
names(Hospital_transposed)[names(Hospital_transposed) == "V2"] <- "hospCum"
Texas_ICU<-openxlsx::read.xlsx(url,sheet=7)
names(Texas_ICU) <- lapply(Texas_ICU[2, ], as.character)
Texas_ICU<-Texas_ICU[25,]
Texas_ICU <- rbind(names(Texas_ICU), Texas_ICU)
ICU_transposed<-transpose(Texas_ICU)
ICU_transposed<-ICU_transposed[-2,]
ICU_transposed<-ICU_transposed[-1,]
names(ICU_transposed)[names(ICU_transposed) == "V1"] <- "date"
names(ICU_transposed)[names(ICU_transposed) == "V2"] <- "icu admissions"
Texas_beds<-openxlsx::read.xlsx(url,sheet=9)
names(Texas_beds) <- lapply(Texas_beds[2, ], as.character)
Texas_beds<-Texas_beds[25,]
Texas_beds <- rbind(names(Texas_beds), Texas_beds)
Texas_beds<-transpose(Texas_beds)
Texas_beds<-Texas_beds[-2,]
Texas_beds<-Texas_beds[-1,]
names(Texas_beds)[names(Texas_beds) == "V1"] <- "date"
names(Texas_beds)[names(Texas_beds) == "V2"] <- "icuBeds"
Texas<-dplyr::left_join(x =Hospital_transposed , y = ICU_transposed)
Texas<-dplyr::left_join(x=Texas, y=Texas_beds)
Texas<-dplyr::left_join(x =Texas , y = Texas_hosptot)
Texas$statename="Texas"
Texas1=Texas%>%
  mutate(date = as.Date(date,origin = "1899-12-30"))


keep5<-c("date","statename","hospTot")
Texas2<-Texas1[keep5] %>% mutate(hospTot=as.integer(hospTot))



rm(Hospital_transposed,ICU_transposed,keep1,keep5,link,local,Texas_beds,Texas,Texas_hospital,Texas_hosptot,Texas_ICU,Texas1,url)



################### MERGING ALL STATE DATA ######################


merge <- full_join(alaska,michigan) %>%
  full_join(Texas2) %>%
  mutate(date=as.Date(date,origin = "1899-12-30")) %>%
  select(date,statename,hospTot) %>%
  filter(!is.na(hospTot))


summary(merge)

#rm(alaska,California,michigan,keeps)



###############   ADDING PERCENT POSITIVE DATA TO HOSPTEST DATA     #################

positive <- read.csv("https://covidtracking.com/data/download/all-states-history.csv") %>%
  select(date,state,positive,negative,recovered,hospitalizedCumulative,hospitalizedCurrently,totalTestResults) %>%
  dplyr::rename(stateabb = state,hospTot=hospitalizedCumulative,hospDaily=hospitalizedCurrently,totaltests=totalTestResults) %>%
  filter(!stateabb=="AS", !stateabb == "PR", !stateabb == "GU", !stateabb == "VI", !stateabb == "MP")

names(positive)
# adding state codes
positive$state <- fips(positive$stateabb)


# adding statenames from abbreviated statenames
positive$statename <- abbr2state(positive$stateabb)


positive1 <- positive %>% filter(!statename=="") %>%
  select(-stateabb) %>%
  mutate(
    nation="",
    county="",
    percentPositive=round((positive/totaltests)*100,digits=2),
    below10pctPositive= ifelse(percentPositive<=10,"Yes","No")) %>%
  select(date,nation,county,state,statename,positive,percentPositive,below10pctPositive,everything()) %>%
  arrange(desc(date),state) %>%
  # dplyr::rename(testedPositive = positive,testedNegative=negative) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -1))


final_hosptest_ts_pre <- positive1 %>% filter(!statename=="Alaska",!statename=="Texas")

premerge <- positive1 %>%
  filter(statename=="Alaska"|statename=="Texas") %>%
  select(-hospTot)

merged <- join(premerge,merge)

final_hosptest_ts <- full_join(final_hosptest_ts_pre,merged)

# merged hosptest static data from jhu and testing data from COVIDTracker project
final_hosptest_static <- final_hosptest_ts %>% filter(date==max(date)) %>% mutate_if(is.numeric, ~replace(., is.na(.), -1))
#final_hosptest_static<-positive1%>%filter(date==max(date)) %>% mutate_if(is.numeric, ~replace(., is.na(.), -1))
#rm(positive,positive1,merged,premerge,final_hosptest_ts_pre,merge)



# STATES: Example for indicators which belong to same header : "VIRAL (RT-PCR) LAB TESTING: LAST WEEK"
states_example_df <- states_df_clean %>% 
  dplyr::select(file_name,date_of_file,
                S01,S02,state,county,
                S21,S22,S52,S62
  ) %>% 
  left_join(states_daterange_clean %>% 
              dplyr::filter(header == "VIRAL (RT-PCR) LAB TESTING: LAST WEEK") %>% 
              dplyr::select(file_name,header,daterange),
            by = "file_name")
states_example_df<-states_example_df[,-c(1,4,6,11)]

names(states_example_df)[1] <- "date"
names(states_example_df)[2] <- "statename"
names(states_example_df)[4] <- "percentPositive"
names(states_example_df)[5] <- "totaltests"
names(states_example_df)[6] <- "hospDaily"
names(states_example_df)[7] <- "percent7dayhospDaily"
states_example_df<-subset(states_example_df,!statename%in%"Guam"& !statename%in%"United States Virgin Islands"& !statename%in%"Commonwealth of the Northern Mariana Islands"& !statename%in%"American Samoa" & !statename%in%"Puerto Rico")
states_example_df$date<-anydate(states_example_df$date)
states_example_df<-states_example_df%>%
  filter(date>="2021-03-08")
states_example_df<-states_example_df[
  with(states_example_df, order(statename, date)),
]
states_example_df<-states_example_df[,-c(8)]
states_example_df$percentPositive<-states_example_df$percentPositive*100
states_example_df$percent7dayhospDaily<-states_example_df$percent7dayhospDaily*100
#################  MERGING HOSPITALIZATION DATA TO COVIDTIMESERIES  ######################

#etwd(box1)
#final_hosptest_ts <- read.csv("./DataUpload/Hospitalizations and testing/series_hosptest.csv")



our_ts <- read.csv("/Users/air/Desktop/lol/covidtimeseries00.csv") 


#merged_covidtimeseries <- join(our_ts,final_hosptest_ts)
final_hosptest_ts$date<-anydate(final_hosptest_ts$date)
final_hosptest_ts<-final_hosptest_ts[,-c(6,8,9,10,11)]
final_hosptest_ts[,9]<-""
names(final_hosptest_ts)[9]<-"percent7dayhospDaily"
final_hosptest_ts<-final_hosptest_ts[
  with(final_hosptest_ts, order(statename, date)),
]
final_hosptest_ts$hospDaily[which(final_hosptest_ts$hospDaily  == -1)] = NA
final_hosptest_ts$percent7dayhospDaily=100*(final_hosptest_ts$hospDaily-lag(final_hosptest_ts$hospDaily,7))/lag(final_hosptest_ts$hospDaily,14)
variable<-c("percent7dayhospDaily")
final_hosptest_ts[, variable][is.na(final_hosptest_ts[, variable])] <- -999
final_hosptest_ts[, variable][is.infinite(final_hosptest_ts[, variable])] <- -999
final_hosptest_ts[, variable][is.nan(final_hosptest_ts[, variable])] <- -999
which(is.na(final_hosptest_ts$percent7dayhospDaily))
which(is.infinite(final_hosptest_ts$percent7dayhospDaily))
which(is.nan(final_hosptest_ts$percent7dayhospDaily))
variable1<-c("hospDaily")
final_hosptest_ts[, variable1][is.na(final_hosptest_ts[, variable1])] <- -1

states_example_df[,8] <- ""
states_example_df[,9] <- ""
names(states_example_df)[8]<-"nation"
names(states_example_df)[9]<-"county"
final_hosptest_ts<-rbind(final_hosptest_ts,states_example_df)
final_hosptest_ts<-final_hosptest_ts[
  with(final_hosptest_ts, order(statename, date)),
]
final_hosptest_ts$percent7dayhospDaily<-round(final_hosptest_ts$percent7dayhospDaily,2)
final_hosptest_ts1<-subset(final_hosptest_ts,date<"2021-03-08")
final_hosptest_ts2<-subset(final_hosptest_ts,date>="2021-03-07")
final_hosptest_ts2$positivetoday=final_hosptest_ts2$totaltests*final_hosptest_ts2$percentPositive/100
detach("package:plyr", unload = TRUE)
final_hosptest_ts3<-final_hosptest_ts2%>%
  group_by(statename)%>%
  mutate(totaltests = cumsum(totaltests))
final_hosptest_ts3<-final_hosptest_ts3%>%
  group_by(statename)%>%
  mutate(positivetoday = cumsum(positivetoday))
final_hosptest_ts3$percentPositive=final_hosptest_ts3$positivetoday/final_hosptest_ts3$totaltests*100
final_hosptest_ts3<-final_hosptest_ts3[!(final_hosptest_ts3$date=="2021-03-07"),]
final_hosptest_ts3<-final_hosptest_ts3[,-c(10)]
final_hosptest_ts4<-rbind(final_hosptest_ts1,final_hosptest_ts3)
final_hosptest_ts4<-final_hosptest_ts4[
  with(final_hosptest_ts4, order(statename, date)),
]
#final_hosptest_ts2$totaltests[final_hosptest_ts2$date=="2021-03-08"]<-final_hosptest_ts2$totaltests[final_hosptest_ts2$date=="2021-03-08"]+final_hosptest_ts2$totaltests[final_hosptest_ts2$date=="2021-03-07"]
#final_hosptest_ts2$totaltests=final_hosptest_ts2$totaltests+lag(final_hosptest_ts2$totaltests)
final_hosptest_ts4$county<-as.integer(final_hosptest_ts4$county)
final_hosptest_ts4$nation<-as.integer(final_hosptest_ts4$nation)
our_ts$date<-anydate(our_ts$date)
merged_covidtimeseries <- left_join(our_ts,final_hosptest_ts4)


variables  <- c("totaltests","hospDaily","percentPositive","percent7dayhospDaily")

merged_covidtimeseries[, variables == ""] <- NA
merged_covidtimeseries[, variables][is.na(merged_covidtimeseries[, variables])] <- -1

rm(final_hosptest_ts,our_ts,variables)

setwd(box1)
hospBeds <- read.csv("./Hospitalizations and testing/hospitalbeds_historic.csv") %>% 
  filter(!statename=="Puerto Rico") 
names(hospBeds)

hospBeds$statename[hospBeds$statename == "District of C"] <- "District of Columbia"            
hospBeds$date<-anydate(hospBeds$date)
final_merged_covidtimeseries <- left_join(merged_covidtimeseries,hospBeds)
variables <- c("bedsAll","bedsCovid","bedsIcu")
final_merged_covidtimeseries[, variables][is.na(final_merged_covidtimeseries[, variables])] <- -1

# keeping the global environment clean 
rm(hospBeds,merged_covidtimeseries)

# reset directory
setwd(box1)


variables  <- c("percent14dayCases","percent14dayDailyCases","percent14dayDeaths","percent14dayDailyDeaths")

final_merged_covidtimeseries[,variables]
final_merged_covidtimeseries[, variables][is.na(final_merged_covidtimeseries[, variables])] <- -999


# final_merged_covidtimeseries$casesfig <- as.numeric(final_merged_covidtimeseries$casesfig)
# final_merged_covidtimeseries$hospTot <- as.numeric(final_merged_covidtimeseries$hospTot)
# final_merged_covidtimeseries$tests <- as.numeric(final_merged_covidtimeseries$tests)


###############  REGION DATA TO COVID TIME SERIES  #################

stateNames <- data.frame(stateNames)

setwd(box1)
regiondata <- read.csv("./Pooja/stateregiondata.csv")

stateregions <- merge(stateNames,regiondata,by.x=c("STATENAME","STATE"),by.y=c("statename","stateabb")) %>%
  arrange(STATEFP) %>%
  dplyr::rename(statename1=STATENAME,state=STATEFP) %>%
  select(-STATE) %>% 
  mutate(state=sub("^(-?)0", "\\1", sprintf("%s", state)),
         state=as.integer(state))

final_merged_covidtimeseries <- join(final_merged_covidtimeseries,stateregions,by="state") %>% select(-statename1)

final_merged_covidtimeseries <- final_merged_covidtimeseries %>% 
  select(date,nation,state,statename,county,countyname,region,division,Population,X_013_Urbanization,X_013_Urbanization_Code,urbanrural,everything())


names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization"] <- "_013_Urbanization"
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization_Code"] <- "_013_Urbanization_Code"


write.csv(final_merged_covidtimeseries,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/covidtimeseries.csv", na="", row.names=F)


########### EXPORTING VACCINE DATA ############

library(RJSONIO)
library(RCurl)
VaccineTracker<-read.csv("/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackertimeseries.csv")
VaccineTracker<-VaccineTracker[,-c(29:31)]
today<-getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")
casesdata <- jsonlite::fromJSON(today)[[2]] 
names(casesdata)[16]<-"percentVaccinatedDose1"
names(casesdata)[17]<-"percentVaccinatedDose2"
names(casesdata)[15]<-"Administered_Dose1"
names(casesdata)[26]<-"Administered_Dose2"
casesdata<-casesdata[,-c(6,18:25,27:30,39:46,48:50)]
casesdata<-casesdata[,-c(29:32)]  
names(casesdata)[names(casesdata) == "LongName"] <- "statename"
casesdata<-casesdata[,-c(2,3)]
casesdata$statename[casesdata$statename== "New York State"] <-"New York"
keep<-c("statename","Population")
population<-final_merged_nationalraw[keep]
population<-subset(population,!statename%in%c(""))
casesdata<-dplyr::left_join(x = casesdata , y = population)
casesdata$FIPS<-fips(casesdata$statename)
casesdata<-subset(casesdata,!statename%in%"American Samoa"& !statename%in%"Bureau of Prisons"& !statename%in%"Dept of Defense"& !statename%in%"Federated States of Micronesia"& !statename%in%"Guam"& !statename%in%"Indian Health Svc"& !statename%in%"Marshall Islands"& !statename%in%"Northern Mariana Islands"& !statename%in%"Puerto Rico"& !statename%in%"Republic of Palau"& !statename%in%"Veterans Health"& !statename%in%"Long Term Care"& !statename%in%"Virgin Islands")
#casesdata<-casesdata[!(casesdata$statename="American Samoa" & casesdata$statename=="Bureau of Prisons"& casesdata$statename=="Dept of Defense"& casesdata$statename=="Federated States of Micronesia"& casesdata$statename=="Guam"& casesdata$statename=="Indian Health Svc"& casesdata$statename=="Marshall Islands"& casesdata$statename=="Northern Mariana Islands"& casesdata$statename=="Puerto Rico"& casesdata$statename=="Republic of Palau"& casesdata$statename=="Veterans Health"& casesdata$statename=="Long Term Care"),]
#casesdata<-casesdata[-c(4, 6, 11, 14, 16, 20, 22,29, 33, 48, 50, 57, 58, 61, 67), ]
#duplicated(casesdata$statename)

casesdata<-casesdata[!duplicated(casesdata),]
casesdata<-casesdata[,c(1:2,28,3:27)]
#casesdata<-casesdata[,c(1:2,17,3:16)]
casesdata[is.na(casesdata)] <- -1
casesdata$FIPS[casesdata$FIPS==-1] <-"_nation"
all<-casesdata[!(casesdata$statename=="United States"),]
casesdata$Population[casesdata$statename=="United States"]<-sum(all$Population)
casesdata<-casesdata[!duplicated(casesdata),]
casesdata$AdministeredPartial<-casesdata$Administered_Dose1-casesdata$Series_Complete_Yes
casesdata$PercentAdministeredPartial<-casesdata$AdministeredPartial/casesdata$Census2019*100
casesdata$PercentAdministeredPartial<-round(casesdata$PercentAdministeredPartial,1)
casesdata$percentReceived<-casesdata$Doses_Administered/casesdata$Doses_Distributed
casesdata$percentReceived<-round(casesdata$percentReceived,1)
casesdata<-casesdata[,-c(26:27)]
casesdata<-casesdata[,-c(16)]

VaccineTracker1<-rbind(VaccineTracker,casesdata)
VaccineTracker1<-VaccineTracker1[
  with(VaccineTracker1, order(statename, Date)),
]
VaccineTracker1$FIPS[VaccineTracker1$FIPS==-1] <-"_nation"
VaccineTracker1$FIPS[VaccineTracker1$FIPS==""] <-"_nation"

detach("package:plyr", unload = TRUE)
VaccineTracker9<-VaccineTracker1%>%group_by(statename) %>%
  mutate(Dist_Per_100K_new = Dist_Per_100K - lag(Dist_Per_100K, default = 0))%>%
  mutate(Dist_Per_100K_new=ifelse (Dist_Per_100K_new<0,0,Dist_Per_100K_new))


VaccineTracker9<-VaccineTracker9[
  with(VaccineTracker9, order(statename, Date)),
]
VaccineTracker9$Dist_Per_100K_new=as.numeric(VaccineTracker9$Dist_Per_100K_new)

VaccineTracker0<-VaccineTracker9%>%group_by(statename) %>%
  mutate(Dist_new = Doses_Distributed - lag(Doses_Distributed, default = 0))%>%
  mutate(Dist_new=ifelse (Dist_new<0,0,Dist_new))

VaccineTracker0<-VaccineTracker0[
  with(VaccineTracker0, order(statename, Date)),
]
casesdata<-casesdata[
  with(casesdata, order(statename, Date)),
]
VaccineTracker0<-VaccineTracker0%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0,lag(Date),Date)) %>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0,lag(Dist_new),Dist_new))

VaccineTracker0<-VaccineTracker0%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0,lag(distDate),distDate)) %>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0,lag(Dist_new),Dist_new))

casesdata$Dist_Per_100K_new<-VaccineTracker0$Dist_Per_100K_new[VaccineTracker0$Date=="2021-03-29"]
casesdata$Dist_new<-VaccineTracker0$Dist_new[VaccineTracker0$Date=="2021-03-29"]
casesdata$distDate<-VaccineTracker0$distDate[VaccineTracker0$Date=="2021-03-29"]

VaccineTracker0$FIPS<-str_remove(VaccineTracker0$FIPS, "^0+")
VaccineTracker0$PercentAdministeredPartial<-round(VaccineTracker0$PercentAdministeredPartial,1)
VaccineTracker0$percentReceived<-round(VaccineTracker0$percentReceived,1)

raw<- jsonlite::fromJSON(today)[[2]] 
raw<-raw[,c(4,18:25,27:29,38:50)]
names(raw)[1]<-"statename"
raw$statename[raw$statename== "New York State"] <-"New York"
casesdata1<-left_join(casesdata,raw)
casesdata1[is.na(casesdata1)] <- -1

write.csv(VaccineTracker0,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackertimeseries.csv",row.names=FALSE,na="")
write.csv(casesdata1,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackerstatic.csv",row.names=FALSE,na="")


vacccounty_link <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data")
vacccounty_data <- jsonlite::fromJSON(vacccounty_link)[2][[1]]



