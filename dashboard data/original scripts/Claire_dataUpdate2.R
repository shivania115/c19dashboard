


###########     SETTING WORKING DIRECTORY   ####################################

#get your working directory
getwd()


box1 = "/Users/air/Box/COVID19_data_shared" 
# I recommend setting a local directory even when there's 
# going to be no file called from your local directory if you're linked to the box1.

local = "/Users/air/Downloads"
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
#################  CALIFORNIA  #####################

# California <- read.csv("https://data.ca.gov/dataset/efd6b822-7312-477c-922b-bccb82025fbe/resource/b6648a0d-ff0a-4111-b80b-febda2ac9e09/download/statewide_testing.csv") %>%
#   dplyr::rename(tests=tested) %>% mutate(date = as.Date(date,origin = "1899-12-30"), statename = "California") %>% arrange(desc(date))


California1 <- read.csv("https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv") %>%
  select(county,todays_date,hospitalized_covid_patients,all_hospital_beds,icu_available_beds,icu_covid_confirmed_patients) %>%
  dplyr::rename(hospTot= hospitalized_covid_patients,allBeds=all_hospital_beds,icuBeds=icu_available_beds,icuAdmDaily=icu_covid_confirmed_patients,date = todays_date) %>%
  mutate(countyname = paste(county,"County, CA", sep=" "),
         date = as.Date(date,origin = "1899-12-30"),
         state = 6) %>%
  select(-county) %>%
  arrange(desc(date),countyname)


California <- aggregate(hospTot~state+date,data=California1,FUN=sum)%>% mutate(statename="California") %>% select(-state)

rm(California1)

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



#####################  District of Columbia #################

columbiadate = str_to_title(format(Sys.Date()-1, '%B-%d-%Y'))
columbiadate<-gsub("(^|[^0-9])0+", "\\1", columbiadate)
columbiadatezip = format(Sys.Date()-1, '%m-%d-%Y')
link = paste("https://coronavirus.dc.gov/sites/default/files/dc/sites/coronavirus/page_content/attachments/DC-COVID-19-Data-for-",columbiadate,".xlsx",sep="")
destfilelink = paste("covid-19-dashboard-District of Columbia-", columbiadatezip,sep="")
download.file(path=local,link, destfile = destfilelink)
unzip(zipfile = destfilelink)
Columbia<-openxlsx::read.xlsx(destfilelink)
names(Columbia)<-sub("X2", "Date", names(Columbia))
Columbia$X1 <- NULL
Columbia$Date<- as.character(Columbia$Date)
Columbia$Date[Columbia$Date == "Total COVID-19 Patients in DC Hospitals"] <-"hospTot"
Columbia$Date[Columbia$Date == "Total COVID-19 Patients in ICU"] <-"icuAdm"
Columbia$Date[Columbia$Date == "ICU Beds Available"] <-"icuBeds"
Columbia <- rbind(names(Columbia), Columbia)
Columbia_transposed<-transpose(Columbia)
names(Columbia_transposed) <- lapply(Columbia_transposed[1, ], as.character)
Columbia_transposed <- Columbia_transposed[-1,]
keeps <- c("Date","hospTot","icuAdm","icuBeds")
Columbia_cleaned = Columbia_transposed[keeps]
Columbia_cleaned$statename="District of Columbia"
Columbia_cleaned=Columbia_cleaned%>%
  mutate(Columbia_cleaned, Date = as.Date(as.numeric(Date), origin = "1899-12-30"))


Columbia_test<-openxlsx::read.xlsx(destfilelink,sheet=8)
names(Columbia_test) <- lapply(Columbia_test[1, ], as.character)
Columbia_test <- Columbia_test[1:3,]
Columbia_test1<-transpose(Columbia_test)
Columbia_test1<-Columbia_test1[-1,]
names(Columbia_test1)[names(Columbia_test1) == "V1"] <- "Date"
names(Columbia_test1)[names(Columbia_test1) == "V2"] <- "testDaily"
names(Columbia_test1)[names(Columbia_test1) == "V3"] <- "positive"
Columbia_test1=Columbia_test1%>%
  mutate(Columbia_test1, Date = as.Date(as.numeric(Date), origin = "1899-12-30"))

District_of_Columbia<-dplyr::left_join(x = Columbia_cleaned , y = Columbia_test1)
names(District_of_Columbia)[names(District_of_Columbia) == "Date"] <- "date"


keep1<-c("date","statename","hospTot")
District_of_Columbia1<-District_of_Columbia[keep1] %>% mutate(hospTot=as.integer(hospTot))
str(District_of_Columbia1)


rm(Columbia,Columbia_cleaned,Columbia_test,Columbia_test1,Columbia_transposed,columbiadate,columbiadatezip,destfilelink,District_of_Columbia)

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


merge <- full_join(alaska,California) %>%
  #full_join(Texas2) %>%
  mutate(date=as.Date(date,origin = "1899-12-30")) %>%
  select(date,statename,hospTot) %>%
  filter(!is.na(hospTot))


summary(merge)

rm(alaska,California,michigan,keeps)



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


final_hosptest_ts_pre <- positive1 %>% filter(!statename=="California",!statename=="Alaska",!statename=="Texas")

premerge <- positive1 %>%
  filter(statename=="California"|statename=="Alaska"|statename=="Texas") %>%
  select(-hospTot)

merged <- join(premerge,merge)

final_hosptest_ts <- full_join(final_hosptest_ts_pre,merged)

# merged hosptest static data from jhu and testing data from COVIDTracker project
final_hosptest_static <- final_hosptest_ts %>% filter(date==max(date)) %>% mutate_if(is.numeric, ~replace(., is.na(.), -1))

rm(positive,positive1,merged,premerge,final_hosptest_ts_pre,merge)


########       !!! WRITE.CSV UNLOCK INSTRUCTIONS !!!        ############

## For MAC users: Highlight the lines you want to unlock and press Command + Shift + C
## For Windows users: Highlight the lines you want to unlock and press CTRL + Shift + C

################### ***UNLOCK TO EXPORT*** ######################
setwd(box1)
write.csv(final_hosptest_ts,"./DataUpload/Hospitalizations and testing/series_hosptest.csv",na="",row.names = FALSE)
write.csv(final_hosptest_static, "./DataUpload/Hospitalizations and testing/state_level.csv",na="",row.names = FALSE)





#################  MERGING HOSPITALIZATION DATA TO COVIDTIMESERIES  ######################

setwd(box1)
final_hosptest_ts <- read.csv("./DataUpload/Hospitalizations and testing/series_hosptest.csv")


setwd(box1)
our_ts <- read.csv("/Users/air/Desktop/lol/covidtimeseries00.csv") 


merged_covidtimeseries <- join(our_ts,final_hosptest_ts)

merged_covidtimeseries$hospTot[which(merged_covidtimeseries$hospTot  == -1)] = NA

merged_covidtimeseries$percent14dayhospTot<-100*(merged_covidtimeseries$hospTot-lag(merged_covidtimeseries$hospTot,14))/lag(merged_covidtimeseries$hospTot,14)
merged_covidtimeseries$percent14dayhospDaily<-100*(merged_covidtimeseries$hospDaily-lag(merged_covidtimeseries$hospDaily,14))/lag(merged_covidtimeseries$hospDaily,14)


variable<-c("percent14dayhospDaily")
merged_covidtimeseries[, variable][is.na(merged_covidtimeseries[, variable])] <- -999
merged_covidtimeseries[, variable][is.infinite(merged_covidtimeseries[, variable])] <- -999
merged_covidtimeseries[, variable][is.nan(merged_covidtimeseries[, variable])] <- -999
which(is.na(merged_covidtimeseries$percent14dayhospDaily))
which(is.infinite(merged_covidtimeseries$percent14dayhospDaily))
which(is.nan(merged_covidtimeseries$percent14dayhospDaily))

variable1<-c("hospTot")
merged_covidtimeseries[, variable1][is.na(merged_covidtimeseries[, variable1])] <- -1
str(merged_covidtimeseries)

variables  <- c("totaltests","hospTot","hospDaily","positive","percentPositive","negative","recovered","percent14dayhospTot","percent14dayhospDaily")

merged_covidtimeseries[, variables == ""] <- NA
merged_covidtimeseries[, variables][is.na(merged_covidtimeseries[, variables])] <- -1



#merged_covidtimeseries$percent14dayhospDaily<-100*(mean7daycases-lag14(mean7daycases))/lag14(mean7daycases)


# keep the global environment clean 
rm(final_hosptest_ts,our_ts,variables)


#######################  MERGING HOSPITAL BEDS DATA WITH COVID TIME SERIES #################

setwd(box1)
hospBeds <- read.csv("./Hospitalizations and testing/hospitalbeds_historic.csv") %>% 
  filter(!statename=="Puerto Rico") 
names(hospBeds)

hospBeds$statename[hospBeds$statename == "District of C"] <- "District of Columbia"            

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

################### ***UNLOCK TO EXPORT*** ###############
### WRITE.CSV UNLOCK INSTRUCTIONS 
## For MAC users: Highlight the lines you want to unlock and press Command + Shift + C 
## For Windows users: Highlight the lines you want to unlock and press CTRL + Shift + C 


# setwd(box1)
# write.csv(final_merged_covidtimeseries,"./DataUpload/Yubin/covidtimeseries.csv", na="", row.names=F)
# 

setwd(box1)
local = "/Users/air/Downloads"

setwd(box1)
write.csv(final_merged_covidtimeseries,"./DataUpload/Yubin/covidtimeseries.csv", na="", row.names=F)
setwd(local)
write.csv(final_merged_covidtimeseries,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/covidtimeseries.csv", na="", row.names=F)

####################################IOWA Issue####################################
setwd("/Users/air/Downloads/Community Profile")

filenames <- list.files(pattern=".xlsx")

df.list <- lapply(filenames, function(x) read_excel(x, sheet="Counties", col_names =T)[,c(1,2,6,27,29)])

colnames<-c("county","fips","state","cases","deaths")
df.list <-lapply(df.list, setNames, colnames)
df.list <-lapply(df.list, function(x) {x <- x[-1, ]})
df.list <-lapply(df.list, function(x) {x <- subset(x,state=="IA"|state=="OK")})

df.list <-lapply(df.list, function(x) {x <- x[!x$county%in%"Unallocated, IA",]})
df.list <-lapply(df.list, function(x) {x <- x[!x$county%in%"Unallocated, OK",]})
df.list <-lapply(df.list, function(x) {x<-x[
  with(x, order(county, cases)),
]})

df.list[1]<-lapply(df.list[1], function(x) within(x, date<-c("2021-03-08")))
df.list[2]<-lapply(df.list[2], function(x) within(x, date<-c("2021-03-09")))
df.list[3]<-lapply(df.list[3], function(x) within(x, date<-c("2021-03-10")))
df.list[4]<-lapply(df.list[4], function(x) within(x, date<-c("2021-03-11")))
df.list[5]<-lapply(df.list[5], function(x) within(x, date<-c("2021-03-12")))
df.list[6]<-lapply(df.list[6], function(x) within(x, date<-c("2021-03-14")))
df.list[7]<-lapply(df.list[7], function(x) within(x, date<-c("2021-03-15")))
df.list[8]<-lapply(df.list[8], function(x) within(x, date<-c("2021-03-16")))
df.list[9]<-lapply(df.list[9], function(x) within(x, date<-c("2021-03-17")))
df.list[10]<-lapply(df.list[10], function(x) within(x, date<-c("2021-03-18")))
df.list[11]<-lapply(df.list[11], function(x) within(x, date<-c("2021-03-19")))
df.list[12]<-lapply(df.list[12], function(x) within(x, date<-c("2021-03-21")))
df.list[13]<-lapply(df.list[13], function(x) within(x, date<-c("2021-03-22")))
df.list[14]<-lapply(df.list[14], function(x) within(x, date<-c("2021-03-23")))
df.list[15]<-lapply(df.list[15], function(x) within(x, date<-c("2021-03-24")))
a<-ldply (df.list, data.frame)
a$cases<-as.integer(a$cases)
a$deaths<-as.integer(a$deaths)
a$state<-abbr2state(a$state)
a$county<-gsub("(.*),.*", "\\1", a$county)
a$county<-sub(" .*", "", a$county)
a$fips<-as.integer(a$fips)
library(anytime)
a$date<-anydate(a$date)
str(a)
#setwd(local)
#library(openxlsx)
#iowa<-write.xlsx(a,'iowa.xlsx')

iowa<-write.csv(a,'/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Processed/iowa_oklahoma.csv')

####################################Hospitaliazation Issue####################################
setwd("/Users/air/Downloads/Community Profile")

filenames <- list.files(pattern=".xlsx")

hosp.list <- lapply(filenames, function(x) read_excel(x, sheet="", col_names =T)[,c(1,2,6,27,29)])


#################  MERGING HOSPITALIZATION DATA TO NATIONALRAW  ######################

setwd(local)

our_static <- read.csv("/Users/air/Desktop/lol/nationalraw0.csv")

our_static[which(our_static$state %in% c(19)),]
our_static$date
our_static$caserate7dayfig[our_static$state==19]
setwd(box1)
final_hosptest_ts <- read.csv("./DataUpload/Hospitalizations and testing/series_hosptest.csv") %>%
  filter(date==max(our_static$date)) 

merged_nationalraw <- join(our_static,final_hosptest_ts)

variables  <- c("totaltests","hospTot","hospDaily","positive","percentPositive","negative","recovered")

merged_nationalraw[, variables == ""] <- NA
merged_nationalraw[, variables][is.na(merged_nationalraw[, variables])] <- -1



# keep the global environment clean 
rm(our_static,final_hosptest_ts,variables)


########## REGION DATA TO NATIONALRAW ############

stateNames <- data.frame(stateNames)

setwd(box1)
regiondata <- read.csv("./Pooja/stateregiondata.csv")

stateregions <- merge(stateNames,regiondata,by.x=c("STATENAME","STATE"),by.y=c("statename","stateabb")) %>%
  arrange(STATEFP) %>%
  dplyr::rename(statename1=STATENAME,state=STATEFP) %>%
  select(-STATE) %>% 
  mutate(state=sub("^(-?)0", "\\1", sprintf("%s", state)),
         state=as.integer(state))

merge <- join(merged_nationalraw,stateregions,by="state") %>% select(-statename1)

final_merged_nationalraw <- merge %>% 
  select(date,nation,state,statename,county,countyname,region,Population,X_013_Urbanization,X_013_Urbanization_Code,everything())
#final_merged_nationalraw<-final_merged_nationalraw[!duplicated(final_merged_nationalraw)]
########## Merge CDC Chronic Condition Data to Nationalraw ############
setwd(box1)
Chronic_Conditions<-read_excel("./DataUpload/cdc_90519_DS1.xlsx")
#setwd(local)

#nationalraw<-read.csv("./nationalraw.csv")
Chronic_Conditions$CNTY_FIPS = str_remove(Chronic_Conditions$CNTY_FIPS, "^0+")
names(Chronic_Conditions)[names(Chronic_Conditions) == "CNTY_FIPS"] <- "county"
names(Chronic_Conditions)[names(Chronic_Conditions) == "STATE_FIPS"] <- "state"
Chronic_Conditions$county<-as.integer(Chronic_Conditions$county)
Chronic_Conditions
Chronic_Conditions$state = str_remove(Chronic_Conditions$state, "^0+")
#final_merged_nationalraw<-merge(final_merged_nationalraw, Chronic_Conditions,by=c("state","county"),all.x=TRUE)[, union(names(final_merged_nationalraw), names(Chronic_Conditions))]

final_merged_nationalraw<-join(final_merged_nationalraw,Chronic_Conditions)


names(final_merged_nationalraw)[names(final_merged_nationalraw) == "county_pop2018_18.and.older"] <- "countyPop201818andolder"

names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_prevalence"] <- "anyconditionPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_Lower 95% CI"] <- "anyconditionLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_Upper 95% CI"] <- "anyconditionUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_number"] <- "anyconditionNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_prevalence"] <- "anyconditionPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_prevalence"] <- "obesityPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_Lower 95% CI"] <- "obesityLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_Upper 95% CI"] <- "obesityUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_number"] <- "obesityNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_prevalence"] <- "heartdiseasePrevalence"

names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_Lower 95% CI"] <- "heartdiseaseLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_Upper 95% CI"] <- "heartdiseaseUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_number"] <- "heartdiseaseNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_prevalence"] <- "copdPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_Lower 95% CI"] <- "copdLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_Upper 95% CI"] <- "copdUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_number"] <- "copdNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_prevalence"] <- "diabetesPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_Lower 95% CI"] <- "diabetesLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_Upper 95% CI"] <- "diabetesUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_number"] <- "diabetesNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_prevalence"] <- "ckdPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_Lower 95% CI"] <- "ckdLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_Upper 95% CI"] <- "ckdUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_number"] <- "ckdNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "county_pop2018_18 and older"] <- "countyPop201818andolder"


final_merged_nationalraw<-final_merged_nationalraw %>%
  select(- c(COUNTY_NAME, STATE_NAME, FIPS,Urban_rural_code,STAB,anyconditionLower95CI,anyconditionUpper95CI,obesityLower95CI,obesityUpper95CI,heartdiseaseLower95CI,heartdiseaseUpper95CI,copdLower95CI,copdUpper95CI,diabetesLower95CI,diabetesUpper95CI,ckdLower95CI,ckdUpper95CI,diabetes,obesity))
# write.csv(merge,"/Users/poojanaik/Downloads/regiondataTOnationraw.csv",na="",row.names=FALSE)


##################  RENAMING URBANIZATION COLUMNS BEFORE EXPORTING  ################  


## keep the global environment clean 
rm(merged_nationalraw,black,age65over,diabetes,groupquater,hhincome,hispanic,male,merge,minority,natives,obesity,poverty,regiontoNR,stateNames,stateregions,regiondata,outcomes, ourraw)


################### ***UNLOCK TO EXPORT*** ###############
## WRITE.CSV UNLOCK INSTRUCTIONS
# For MAC users: Highlight the lines you want to unlock and press Command + Shift + C
## For Windows users: Highlight the lines you want to unlock and press CTRL + Shift + C

setwd(box1)
write.csv(final_merged_nationalraw,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/nationalraw.csv", na="", row.names=F)

# reset directory
setwd(local)
write.csv(final_merged_nationalraw,"./nationalraw.csv", na="", row.names=F)
 




########## NATIONAL REPORT: Highest 10 BAR CHARTS ###################


## Highest 10 CASES CHANGE 

setwd(local)
highcases <- as_tibble(read.csv("./nationalraw.csv")) %>%
  filter(!countyname=="") %>% 
  mutate(caserate7day=round(caserate7day),date=as.Date(date,origin = "1899-12-30")) %>%
  select(date,countyname,caserate7day)


Highest10cases <- highcases %>% 
  arrange(desc(caserate7day)) %>% 
  slice(1:10) %>% 
  mutate(variable = "caserate7day", rank = seq(1:10)) %>% 
  dplyr::rename(measure = caserate7day)



## Highest 10 DEATHS CHANGE 

setwd(local)
highdeaths <- as_tibble(read.csv("./nationalraw.csv")) %>%
  filter(!countyname=="") %>% 
  mutate(covidmortality7day=round(covidmortality7day),date=as.Date(date,origin = "1899-12-30")) %>%
  select(date,countyname,covidmortality7day)


Highest10deaths <- highdeaths %>% 
  arrange(desc(covidmortality7day)) %>%
  slice(1:10) %>%
  mutate(variable = "covidmortality7day", rank = seq(1:10)) %>% 
  dplyr::rename(measure = covidmortality7day)


# MERGE THE DATASET
merge <- full_join(Highest10cases,Highest10deaths) %>% select(date,countyname,variable,measure,rank)



################### ***UNLOCK TO EXPORT*** ###############
setwd(local)
#write.csv(merge,"./DataUpload/Yubin/Highest10barchart.csv",row.names=FALSE,na="")
write.csv(merge,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/Highest10barchart.csv",row.names=FALSE,na="")

# KEEP THE GLOBAL ENVIRONMENT CLEAN
rm(highcases,highdeaths,Highest10cases,Highest10deaths,merge)



########### NATIONAL REPORT: TRENDLINE PLOTS ####################


# CASE RATE 

setwd(local)
raw <- read.csv("./nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  select(countyname,percent14dayDailyCases) %>%
  arrange(desc(percent14dayDailyCases)) %>% slice(1:10) %>% 
  mutate(rank = seq(1:10)) %>% 
  select(-percent14dayDailyCases)


setwd(local)
data <- read.csv("/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/covidtimeseries.csv") %>% 
  left_join(raw) %>% 
  filter(!rank=="") %>%
  select(date,countyname,rank,caserate7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "caserate7day") %>%
  dplyr::rename(measure = caserate7day)


# DEATH RATE 

setwd(local)
death <- read.csv("./nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  select(countyname,percent14dayDailyDeaths) %>%
  arrange(desc(percent14dayDailyDeaths)) %>% slice(1:5) %>% 
  mutate(rank = seq(1:5)) %>% 
  select(-percent14dayDailyDeaths)


setwd(local)
deathraw <- read.csv("/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/covidtimeseries.csv") %>% 
  left_join(death) %>% 
  filter(!rank=="") %>%
  select(date,countyname,rank,covidmortality7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "covidmortality7day") %>%
  dplyr::rename(measure = covidmortality7day)


# MERGE DATA 

merge <- full_join(data,deathraw) %>% select(date,countyname,variable,measure,rank)


################### ***UNLOCK TO EXPORT*** ###############
setwd(local)
#write.csv(merge,"./DataUpload/Yubin/Highest10trendlines.csv",row.names=FALSE,na="")
write.csv(merge,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/Highest10trendlines.csv",row.names=FALSE,na="")

# Keep the global environment clean
rm(raw,data,deathraw,death,merge)



######################  MERGING CVI SCORE  ######################

ccvi_states <- openxlsx::read.xlsx("https://covid-static-assets.s3.amazonaws.com/US-CCVI/ccvi-US.xlsx",sheet=1) %>%
  dplyr::rename(statename=stateName,state=FIPS) %>%
  mutate(statename=str_to_title(statename), countyname="", countycode=NA)

statecode <- ccvi_states %>% select(statename,state)

ccvi_county <- openxlsx::read.xlsx("https://covid-static-assets.s3.amazonaws.com/US-CCVI/ccvi-US.xlsx",sheet=2) %>%
  dplyr::rename(statename=stateName,countyname=countyName,countycode=FIPS) %>%
  mutate(statename=str_to_title(statename)) %>% join(statecode)

CVI <- full_join(ccvi_county,ccvi_states) %>% select(-countyname)


CVI$statename[!is.na(CVI$countycode)] <- ""

setwd(local)
nationalraw <- read.csv("./nationalraw.csv")

CVImerged <- join(nationalraw,CVI)


################## ***UNLOCK TO EXPORT*** ###############

#setwd(box1)
#write.csv(CVImerged,"./DataUpload/Yubin/nationalraw.csv",row.names=FALSE,na="")
setwd(local)
write.csv(CVImerged,"./nationalraw.csv",row.names=FALSE,na="")


# keep the global environment clean
rm(ccvi_states,statecode,ccvi_county,CVI,nationalraw)


#######################  MERGING RESIDENTIAL SEGREGATION SCORE  ######################

setwd(box1)
resseg <- read.csv("./RaceData/residSEg.csv") 


setwd(box1)
setwd(local)

nationalraw <- read.csv("./nationalraw.csv")
# merge with nationalraw
ressegToNR <- left_join(nationalraw,resseg)
ressegToNR$X_013_Urbanization_Code  <-  factor(ifelse(ressegToNR$X_013_Urbanization=="",-1,
                                                      ifelse(ressegToNR$X_013_Urbanization=="Large Central Metro",6,
                                                             ifelse(ressegToNR$X_013_Urbanization=="Large Fringe Metro",5,
                                                                    ifelse(ressegToNR$X_013_Urbanization=="Medium Metro",4,
                                                                           ifelse(ressegToNR$X_013_Urbanization=="Small Metro",3,
                                                                                  ifelse(ressegToNR$X_013_Urbanization=="Micropolitan (Nonmetro)",2,1)))))))



ressegToNR$region_Code  <- factor(ifelse(ressegToNR$region=="",-1,
                                         ifelse(ressegToNR$region=="South",1,
                                                ifelse(ressegToNR$region=="West",2,
                                                       ifelse(ressegToNR$region=="Northeast",3,4)))))




################### ***UNLOCK TO EXPORT*** ###############
setwd(box1)
setwd(local)
write.csv(ressegToNR,"./nationalraw.csv",row.names=FALSE,na="")


setwd(box1)
write.csv(ressegToNR,"./DataUpload/Yubin/nationalraw.csv",row.names=FALSE,na="")


# keep the global environment clean
rm(resseg,ressegToNR,nationalraw)




########### HOW MANY STATES CONTRIBUTED? ############

setwd(box1)
setwd(local)
raw <- read.csv("./nationalraw.csv")%>% 
  filter(nation=="1"|county==""|!statename=="") %>% 
  select(date,state,nation,county,statename,cases,dailycases) %>%
  mutate(date = as.Date(date,origin = "1899-12-30"))
#raw1<-raw[!duplicated(raw[,c("statename","state")]),]

raw<-raw%>%
group_by(statename) %>%
  slice(n()) 




totnationcases <- as.numeric(raw$dailycases[raw$nation=="1"]) 
totalnationdailycases <- totnationcases[!is.na(totnationcases)]


sharedailycases <- raw %>%
  group_by(statename) %>%
  mutate(
    contri = totalnationdailycases*0.5) %>%
  select(date,nation,state,statename,dailycases,contri) %>% filter(!nation=="1"|!state=="") %>% arrange(desc(dailycases))

threshold <- max(sharedailycases$contri)

number_of_states <- as.numeric(length(which(cumsum(sharedailycases$dailycases) <= threshold)))

names <- as.data.frame(head(sharedailycases$statename,number_of_states))

contristates <- as_tibble(apply(names,2,paste,collapse=", ")) %>% dplyr::rename(statenames=value)


setwd(box1)
setwd(local)
jsonlite::write_json(contristates,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/contristates.json")
jsonlite::write_json(contristates,"./contristates.json")

rm(number_of_states,names,contristates,sharedailycases,raw,totnationcases,totalnationdailycases,threshold)


############  OUTCOME VS COUNTY CHARACTERISTICS DATA TO NATIONAL RAW #########


setwd(box1)
setwd(local)
ourraw <- read.csv("./nationalraw.csv")

names(ourraw)

ourraw$malegroup = quantcut(ourraw$male,5)
table(ourraw$malegroup)

ourraw$age65overgroup = quantcut(ourraw$age65over,5)
table(ourraw$age65overgroup)

ourraw$blackgroup = quantcut(ourraw$black,5)
table(ourraw$blackgroup)

ourraw$povertygroup = quantcut(ourraw$poverty,5)
table(ourraw$povertygroup)

ourraw$minoritygroup = quantcut(ourraw$minority,5)
table(ourraw$minoritygroup)

ourraw$groupquatergroup = quantcut(ourraw$groupquater,5)
table(ourraw$groupquatergroup)



ourraw$hispanicgroup = quantcut(ourraw$hispanic,5)
table(ourraw$hispanicgroup)

ourraw$nativesgroup = quantcut(ourraw$natives,5)
table(ourraw$nativesgroup)



ourraw$hhincomegroup = quantcut(ourraw$hhincome,5)
table(ourraw$hhincomegroup)

ourraw$cvigroup = quantcut(ourraw$ccvi,5)
table(ourraw$cvigroup)

ourraw$residseggroup = quantcut(ourraw$RS_blackwhite,5)
table(ourraw$residseggroup)

ourraw$anyconditiongroup = quantcut(ourraw$anyconditionPrevalence,5)
table(ourraw$anyconditiongroup)

ourraw$obesity2group = quantcut(ourraw$obesityPrevalence,5)
table(ourraw$obesity2group)

ourraw$diabetes2group = quantcut(ourraw$diabetesPrevalence,5)
table(ourraw$diabetes2group)

ourraw$heartdiseasegroup = quantcut(ourraw$heartdiseasePrevalence,5)
table(ourraw$heartdiseasegroup)

ourraw$copdgroup = quantcut(ourraw$copdPrevalence,5)
table(ourraw$copdgroup)

ourraw$ckdgroup = quantcut(ourraw$ckdPrevalence,5)
table(ourraw$ckdgroup)


###############  CASE RATE VS EXPOSURE CHARTS ################

black <- aggregate(cbind(caserate7day,caserate)~blackgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="black") %>% dplyr::rename(quintgroup=blackgroup)
male <- aggregate(cbind(caserate7day,caserate)~malegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="male") %>% dplyr::rename(quintgroup=malegroup)
poverty <- aggregate(cbind(caserate7day,caserate)~povertygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="poverty") %>% dplyr::rename(quintgroup=povertygroup)
age65over <- aggregate(cbind(caserate7day,caserate)~age65overgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="age65over") %>% dplyr::rename(quintgroup=age65overgroup)
minority <- aggregate(cbind(caserate7day,caserate)~minoritygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="minority") %>% dplyr::rename(quintgroup=minoritygroup)
groupquater <- aggregate(cbind(caserate7day,caserate)~groupquatergroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="groupquarter") %>% dplyr::rename(quintgroup=groupquatergroup)

hispanic <- aggregate(cbind(caserate7day,caserate)~hispanicgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hispanic") %>% dplyr::rename(quintgroup=hispanicgroup)
natives <- aggregate(cbind(caserate7day,caserate)~nativesgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="natives") %>% dplyr::rename(quintgroup=nativesgroup)
urbanrural <- aggregate(cbind(caserate7day,caserate)~X_013_Urbanization,ourraw,FUN="mean") %>% filter(!X_013_Urbanization=="") %>% mutate(quintileVar="urbanrural",lbl = X_013_Urbanization) %>% dplyr::rename(quintgroup=X_013_Urbanization)

hhincome <- aggregate(cbind(caserate7day,caserate)~hhincomegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hhincome") %>% dplyr::rename(quintgroup=hhincomegroup)
region <- aggregate(cbind(caserate7day,caserate)~region,ourraw,FUN="mean") %>% filter(!region=="") %>% mutate(quintileVar="region",lbl = region) %>% dplyr::rename(quintgroup=region)
cvi <- aggregate(cbind(caserate7day,caserate)~cvigroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CVI") %>% dplyr::rename(quintgroup=cvigroup)
residseg <- aggregate(cbind(caserate7day,caserate)~residseggroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="resSeg") %>% dplyr::rename(quintgroup=residseggroup)

anycondition <- aggregate(cbind(caserate7day,caserate)~anyconditiongroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="any condition") %>% dplyr::rename(quintgroup=anyconditiongroup)
diabetes2 <- aggregate(cbind(caserate7day,caserate)~diabetes2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="diabetes 2018") %>% dplyr::rename(quintgroup=diabetes2group)
obesity2 <- aggregate(cbind(caserate7day,caserate)~obesity2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="obesity 2018") %>% dplyr::rename(quintgroup=obesity2group)
heartdisease <- aggregate(cbind(caserate7day,caserate)~heartdiseasegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="heart disease") %>% dplyr::rename(quintgroup=heartdiseasegroup)
copd <- aggregate(cbind(caserate7day,caserate)~copdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="COPD") %>% dplyr::rename(quintgroup=copdgroup)
ckd <- aggregate(cbind(caserate7day,caserate)~ckdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CKD") %>% dplyr::rename(quintgroup=ckdgroup)


outcomes_pre <- full_join(black,male) %>% full_join(poverty) %>% full_join(age65over) %>%
  full_join(minority) %>% full_join(groupquater)  %>% full_join(hispanic) %>%
  full_join(natives) %>% full_join(urbanrural)  %>% full_join(hhincome) %>% full_join(region) %>%
  full_join(cvi) %>% full_join(residseg) %>% full_join(anycondition) %>% full_join(diabetes2) %>% full_join(obesity2) %>%
  full_join(heartdisease) %>% full_join(copd) %>% full_join(ckd) %>%
  select(caserate7day,everything())

outcomes <- gather(outcomes_pre,
                   key="variable",
                   value="measure",
                   c("caserate","caserate7day")) %>% select(quintileVar,quintgroup,lbl,variable,measure)


rm(outcomes_pre,black,male,poverty,age65over,minority,groupquater,hispanic,natives,urbanrural,hhincome,region,cvi,residseg)



###############  covidmortality VS EXPOSURE CHARTS ################


black_mort <- aggregate(cbind(covidmortality,covidmortality7day)~blackgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="black") %>% dplyr::rename(quintgroup=blackgroup)
male_mort <- aggregate(cbind(covidmortality,covidmortality7day)~malegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="male") %>% dplyr::rename(quintgroup=malegroup)
poverty_mort <- aggregate(cbind(covidmortality,covidmortality7day)~povertygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="poverty") %>% dplyr::rename(quintgroup=povertygroup)
age65over_mort <- aggregate(cbind(covidmortality,covidmortality7day)~age65overgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="age65over") %>% dplyr::rename(quintgroup=age65overgroup)
minority_mort <- aggregate(cbind(covidmortality,covidmortality7day)~minoritygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="minority") %>% dplyr::rename(quintgroup=minoritygroup)
groupquater_mort <- aggregate(cbind(covidmortality,covidmortality7day)~groupquatergroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="groupquarter") %>% dplyr::rename(quintgroup=groupquatergroup)
diabetes_mort <- aggregate(cbind(covidmortality,covidmortality7day)~diabetesgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="diabetes") %>% dplyr::rename(quintgroup=diabetesgroup)
hispanic_mort <- aggregate(cbind(covidmortality,covidmortality7day)~hispanicgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hispanic") %>% dplyr::rename(quintgroup=hispanicgroup)
natives_mort <- aggregate(cbind(covidmortality,covidmortality7day)~nativesgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="natives") %>% dplyr::rename(quintgroup=nativesgroup)
urbanrural_mort <- aggregate(cbind(covidmortality,covidmortality7day)~X_013_Urbanization,ourraw,FUN="mean") %>% filter(!X_013_Urbanization=="") %>% mutate(quintileVar="urbanrural",lbl = X_013_Urbanization) %>% dplyr::rename(quintgroup=X_013_Urbanization)
obesity_mort <- aggregate(cbind(covidmortality,covidmortality7day)~obesitygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="obesity") %>% dplyr::rename(quintgroup=obesitygroup)
hhincome_mort <- aggregate(cbind(covidmortality,covidmortality7day)~hhincomegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hhincome") %>% dplyr::rename(quintgroup=hhincomegroup)
region_mort <- aggregate(cbind(covidmortality,covidmortality7day)~region,ourraw,FUN="mean") %>% filter(!region=="") %>% mutate(quintileVar="region",lbl = region) %>% dplyr::rename(quintgroup=region)
cvi_mort <- aggregate(cbind(covidmortality,covidmortality7day)~cvigroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CVI") %>% dplyr::rename(quintgroup=cvigroup)
residseg_mort <- aggregate(cbind(covidmortality,covidmortality7day)~residseggroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="resSeg") %>% dplyr::rename(quintgroup=residseggroup)


anycondition_mort <- aggregate(cbind(covidmortality,covidmortality7day)~anyconditiongroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="any condition") %>% dplyr::rename(quintgroup=anyconditiongroup)
diabetes2_mort <- aggregate(cbind(covidmortality,covidmortality7day)~diabetes2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="diabetes 2018") %>% dplyr::rename(quintgroup=diabetes2group)
obesity2_mort <- aggregate(cbind(covidmortality,covidmortality7day)~obesity2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="obesity 2018") %>% dplyr::rename(quintgroup=obesity2group)
heartdisease_mort <- aggregate(cbind(covidmortality,covidmortality7day)~heartdiseasegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="heart disease") %>% dplyr::rename(quintgroup=heartdiseasegroup)
copd_mort <- aggregate(cbind(covidmortality,covidmortality7day)~copdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="COPD") %>% dplyr::rename(quintgroup=copdgroup)
ckd_mort <- aggregate(cbind(covidmortality,covidmortality7day)~ckdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CKD") %>% dplyr::rename(quintgroup=ckdgroup)


outcomes_mort_pre <- full_join(black_mort,male_mort) %>% full_join(poverty_mort) %>% full_join(age65over_mort) %>%
  full_join(minority_mort) %>% full_join(groupquater_mort) %>% full_join(hispanic_mort) %>%
  full_join(natives_mort) %>% full_join(urbanrural_mort)  %>% full_join(hhincome_mort) %>% full_join(region_mort) %>% full_join(cvi_mort) %>%
  full_join(residseg_mort) %>% full_join(anycondition_mort) %>% full_join(diabetes2_mort) %>% full_join(obesity2_mort) %>%
  full_join(heartdisease_mort) %>% full_join(copd_mort) %>% full_join(ckd_mort) %>%
  select(covidmortality7day,everything())


outcomes_mort <- gather(outcomes_mort_pre,
                        key="variable",
                        value="measure",
                        c("covidmortality","covidmortality7day")) %>% select(quintileVar,quintgroup,lbl,variable,measure)


rm(outcomes_mort_pre,ourraw,black_mort,male_mort,poverty_mort,age65over_mort,minority_mort,groupquater_mort,diabetes_mort,hispanic_mort,natives_mort,urbanrural_mort,obesity_mort,hhincome_mort,region_mort,cvi_mort,residseg_mort)


################### ***JOINING CASERATE AND COVID MORTALITY PLOTS DATA*** ###############

merged <- full_join(outcomes,outcomes_mort)

#indices_data[which(indices_data$nation == 1),]$anycondition <- mean(chronic_state$anycondition)
merged$quintileVar[merged$quintileVa=="CVI"] <- "ccvi"
merged$quintileVar[merged$quintileVa=="any condition"] <- "anycondition"
merged$quintileVar[merged$quintileVa=="diabetes 2018"] <- "diabetes"
merged$quintileVar[merged$quintileVa=="obesity 2018"] <- "obesity"
merged$quintileVar[merged$quintileVa=="heart disease"] <- "heartdisease"
merged$quintileVar[merged$quintileVa=="COPD"] <- "copd"
merged$quintileVar[merged$quintileVa=="CKD"] <- "ckd"




################### ***UNLOCK TO EXPORT*** ###############


write.csv(merged,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/lastbarcharts_merged.csv",na="",row.names=FALSE)


rm(outcomes_mort,outcomes)


########### INDICES DATA FROM OXFORD ############

#calling indices data from oxford github
oxford_indices <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>%
  filter(CountryCode=="USA") %>%
  transform(date = as.Date(as.character(Date), "%Y%m%d")) %>%
  select(date,CountryName,RegionName,StringencyIndex,GovernmentResponseIndex,ContainmentHealthIndex,EconomicSupportIndex)

states_indices <- oxford_indices %>%
  filter(!is.na(StringencyIndex),!is.na(GovernmentResponseIndex),!is.na(ContainmentHealthIndex),!is.na(EconomicSupportIndex)) %>%
  # group_by(RegionName) %>%
  # slice(n()) %>%
  dplyr::rename(statename=RegionName,country=CountryName,stringencyIndex=StringencyIndex,governmentResponseIndex=GovernmentResponseIndex,containmentHealthIndex=ContainmentHealthIndex,economicSupportIndex=EconomicSupportIndex) %>%
  mutate(nation=ifelse(statename=="",1,NA)) %>%
  select(-country) %>%
  select(date,nation,statename,everything())

#calling nationalraw

setwd(local)
ourraw <- read.csv("./nationalraw.csv")
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization"] <- "_013_Urbanization"
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization_Code"] <- "_013_Urbanization_Code"

# checking
# nr <- ourraw %>% select(date,nation,statename,state)
# jn <- join(nr,states_indices)

#merge with nationalraw

indices_data <- join(ourraw,states_indices)

indices_data<-indices_data %>%
  select(-c("urbanrural"))

names(indices_data )[names(indices_data ) == "X_013_Urbanization"] <- "urbanrural_text"
names(indices_data )[names(indices_data ) == "X_013_Urbanization_Code"] <- "urbanrural"
names(indices_data )[names(indices_data ) == "RS_blackwhite"] <- "resSeg"
names(indices_data )[names(indices_data ) == "region"] <- "region_text"
names(indices_data )[names(indices_data ) == "region_Code"] <- "region"
names(indices_data )[names(indices_data ) == "diabetesPrevalence"] <- "diabetes"
names(indices_data )[names(indices_data ) == "heartdiseasePrevalence"] <- "heartdisease"
names(indices_data )[names(indices_data ) == "ckdPrevalence"] <- "ckd"
names(indices_data )[names(indices_data ) == "copdPrevalence"] <- "copd"
names(indices_data )[names(indices_data ) == "anyconditionPrevalence"] <- "anycondition"
names(indices_data )[names(indices_data ) == "obesityPrevalence"] <- "obesity"

#final_merged_covidtimeseries$dailyDeathrate<-final_merged_covidtimeseries$dailydeaths/final_merged_covidtimeseries$Population
#dailydeathrate_subset<-subset(final_merged_covidtimeseries,!county=="NA")
#keep<-c("date","state","county","region","division","dailydeaths","dailyDeathrate","Population","urbanrural","fips")
#dailydeathrate_subset<-dailydeathrate_subset[keep]
#dailydeathrate_subset$year<-substring(dailydeathrate_subset$date,1,4)
#dailydeathrate_2020<-subset(dailydeathrate_subset,year=="2020")

#indices_data$anycondition [indices_data$anycondition== "NA"] <-"New York"
anycondition_state<-aggregate(anycondition_prevalence ~ STATE_NAME, Chronic_Conditions, mean)
obesity_state<-aggregate(Obesity_prevalence ~ STATE_NAME, Chronic_Conditions, mean)
heartdisease_state<-aggregate(`Heart disease_prevalence` ~ STATE_NAME, Chronic_Conditions, mean)
COPD_state<-aggregate(COPD_prevalence ~ STATE_NAME, Chronic_Conditions, mean)
diabetes_state<-aggregate(diabetes_prevalence ~ STATE_NAME, Chronic_Conditions, mean)
CKD_state<-aggregate(CKD_prevalence ~ STATE_NAME, Chronic_Conditions, mean)
anycondition_num<-aggregate(anycondition_number ~ STATE_NAME, Chronic_Conditions, mean)
obesity_num<-aggregate(Obesity_number ~ STATE_NAME, Chronic_Conditions, mean)
heartdisease_num<-aggregate(`Heart disease_number` ~ STATE_NAME, Chronic_Conditions, mean)
COPD_num<-aggregate(COPD_number ~ STATE_NAME, Chronic_Conditions, mean)
diabetes_num<-aggregate(diabetes_number ~ STATE_NAME, Chronic_Conditions, mean)
CKD_num<-aggregate(CKD_number ~ STATE_NAME, Chronic_Conditions, mean)
resSeg<-subset(indices_data,!countyname%in%"")
resSeg_state<-aggregate(resSeg ~ state, resSeg, mean)
ccvi_state<-aggregate(ccvi ~ state, resSeg, mean)

#resSeg_state$state<-as.numeric(resSeg_state$state)
#format(resSeg_state$state,'00')
#resSeg_state$state<-as.character(resSeg_state$state)


#resSeg_state$state<- convert_fips_to_names(
  #resSeg_state$state,
#states = NULL,
#geo_header = "STATE",
#in_states = NULL
#)
#ccvi_state$state<-fips(ccvi_state$state,to='Name')

#names(resSeg_state)[names(resSeg_state) == "state"] <- "statename"
#names(ccvi_state)[names(ccvi_state) == "state"] <- "statename"



chronic_state<-dplyr::left_join(x=anycondition_state,y=obesity_state)
chronic_state<-dplyr::left_join(x=chronic_state,heartdisease_state)
chronic_state<-dplyr::left_join(x=chronic_state,COPD_state)
chronic_state<-dplyr::left_join(x=chronic_state,diabetes_state)
chronic_state<-dplyr::left_join(x=chronic_state,CKD_state)
chronic_state<-dplyr::left_join(x=chronic_state,anycondition_num)
chronic_state<-dplyr::left_join(x=chronic_state,obesity_num)
chronic_state<-dplyr::left_join(x=chronic_state,heartdisease_num)
chronic_state<-dplyr::left_join(x=chronic_state,COPD_num)
chronic_state<-dplyr::left_join(x=chronic_state,diabetes_num)
chronic_state<-dplyr::left_join(x=chronic_state,CKD_num)


names(chronic_state)[names(chronic_state) == "STATE_NAME"] <- "statename"
names(chronic_state)[names(chronic_state) == "diabetes_prevalence"] <- "diabetes"
names(chronic_state)[names(chronic_state) == "Heart disease_prevalence"] <- "heartdisease"
names(chronic_state)[names(chronic_state) == "CKD_prevalence"] <- "ckd"
names(chronic_state )[names(chronic_state) == "COPD_prevalence"] <- "copd"
names(chronic_state)[names(chronic_state) == "anycondition_prevalence"] <- "anycondition"
names(chronic_state)[names(chronic_state) == "Obesity_prevalence"] <- "obesity"

names(chronic_state)[names(chronic_state) == "diabetes_number"] <- "diabetesNumber"
names(chronic_state)[names(chronic_state) == "Heart disease_number"] <- "heartdiseaseNumber"
names(chronic_state)[names(chronic_state) == "CKD_number"] <- "ckdNumber"
names(chronic_state )[names(chronic_state) == "COPD_number"] <- "copdNumber"
names(chronic_state)[names(chronic_state) == "anycondition_number"] <- "anyconditionNumber"
names(chronic_state)[names(chronic_state) == "Obesity_number"] <- "obesityNumber"
#resSeg_state$statename=as.character(resSeg_state$statename)
#ccvi_state$statename=as.character(ccvi_state$statename)
chronic_state$state<-fips(chronic_state$statename)
chronic_state$state<-str_remove(chronic_state$state, "^0+")
chronic_state$state<-as.numeric(chronic_state$state)
chronic_state<-dplyr::left_join(x=chronic_state,resSeg_state)
chronic_state<-dplyr::left_join(x=chronic_state,ccvi_state)
chronic_state<-chronic_state[,-14]
variable<-c("diabetes.y","heartdisease.y","ckd.y","copd.y","anycondition.y","obesity.y","diabetesNumber.y","heartdiseaseNumber.y","ckdNumber.y","copdNumber.y","anyconditionNumber.y","obesityNumber.y","resSeg.y","ccvi.y")
indices_data<-indices_data %>% subset(., select=which(!duplicated(names(.)))) 

indices_data$original_order <- 1:nrow(indices_data)

indices_data1 <- merge(indices_data, chronic_state, by = "statename", all = TRUE)

indices_data1<- indices_data1[order(indices_data1$original_order),]
indices_data1<-indices_data1[,c(2:4,1,5:111)]
indices_data=indices_data1

indices_data$anycondition.x <- ifelse(is.na(indices_data$anycondition.x), indices_data$anycondition.y, indices_data$anycondition.x)
names(indices_data)[names(indices_data) == "anycondition.x"] <- "anycondition"
indices_data$anycondition=round(indices_data$anycondition, 2)


indices_data$diabetes.x <- ifelse(is.na(indices_data$diabetes.x), indices_data$diabetes.y, indices_data$diabetes.x)
names(indices_data)[names(indices_data) == "diabetes.x"] <- "diabetes"
indices_data$diabetes=round(indices_data$diabetes, 2)


indices_data$heartdisease.x <- ifelse(is.na(indices_data$heartdisease.x), indices_data$heartdisease.y, indices_data$heartdisease.x)
names(indices_data)[names(indices_data) == "heartdisease.x"] <- "heartdisease"
indices_data$heartdisease=round(indices_data$heartdisease,2)

indices_data$ckd.x <- ifelse(is.na(indices_data$ckd.x), indices_data$ckd.y, indices_data$ckd.x)
names(indices_data)[names(indices_data) == "ckd.x"] <- "ckd"
indices_data$ckd=round(indices_data$ckd,2)

indices_data$copd.x <- ifelse(is.na(indices_data$copd.x), indices_data$copd.y, indices_data$copd.x)
names(indices_data)[names(indices_data) == "copd.x"] <- "copd"
indices_data$copd=round(indices_data$copd,2)

indices_data$obesity.x <- ifelse(is.na(indices_data$obesity.x), indices_data$obesity.y, indices_data$obesity.x)
names(indices_data)[names(indices_data) == "obesity.x"] <- "obesity"
indices_data$obesity=round(indices_data$obesity,2)

indices_data$obesityNumber.x <- ifelse(is.na(indices_data$obesityNumber.x), indices_data$obesityNumber.y, indices_data$obesityNumber.x)
names(indices_data)[names(indices_data) == "obesityNumber.x"] <- "obesityNumber"
indices_data$obesityNumber=round(indices_data$obesityNumber,2)

indices_data$diabetesNumber.x <- ifelse(is.na(indices_data$diabetesNumber.x), indices_data$diabetesNumber.y, indices_data$diabetesNumber.x)
names(indices_data)[names(indices_data) == "diabetesNumber.x"] <- "diabetesNumber"
indices_data$diabetesNumber=round(indices_data$diabetesNumber, 2)

indices_data$heartdiseaseNumber.x <- ifelse(is.na(indices_data$heartdiseaseNumber.x), indices_data$heartdiseaseNumber.y, indices_data$heartdiseaseNumber.x)
names(indices_data)[names(indices_data) == "heartdiseaseNumber.x"] <- "heartdiseaseNumber"
indices_data$heartdiseaseNumber=round(indices_data$heartdiseaseNumber,2)

indices_data$ckdNumber.x <- ifelse(is.na(indices_data$ckdNumber.x), indices_data$ckdNumber.y, indices_data$ckdNumber.x)
names(indices_data)[names(indices_data) == "ckdNumber.x"] <- "ckdNumber"
indices_data$ckdNumber=round(indices_data$ckdNumber,2)

indices_data$copdNumber.x <- ifelse(is.na(indices_data$copdNumber.x), indices_data$copdNumber.y, indices_data$copdNumber.x)
names(indices_data)[names(indices_data) == "copdNumber.x"] <- "copdNumber"
indices_data$copdNumber=round(indices_data$copdNumber,2)

indices_data$anyconditionNumber.x <- ifelse(is.na(indices_data$anyconditionNumber.x), indices_data$anyconditionNumber.y, indices_data$anyconditionNumber.x)
names(indices_data)[names(indices_data) == "anyconditionNumber.x"] <- "anyconditionNumber"
indices_data$anyconditionNumber=round(indices_data$anyconditionNumber, 2)

indices_data$resSeg.x <- ifelse(is.na(indices_data$resSeg.x), indices_data$resSeg.y, indices_data$resSeg.x)
names(indices_data)[names(indices_data) == "resSeg.x"] <- "resSeg"
indices_data$resSeg=round(indices_data$resSeg,2)

indices_data$ccvi.x <- ifelse(is.na(indices_data$ccvi.x), indices_data$ccvi.y, indices_data$ccvi.x)
names(indices_data)[names(indices_data) == "ccvi.x"] <- "ccvi"
indices_data$ccvi=round(indices_data$ccvi,2)

indices_data<-indices_data%>%select(-c("diabetes.y","heartdisease.y","ckd.y","copd.y","anycondition.y","obesity.y","diabetesNumber.y","heartdiseaseNumber.y","ckdNumber.y","copdNumber.y","anyconditionNumber.y","obesityNumber.y","resSeg.y","ccvi.y"))


indices_data[which(indices_data$statename %in% c("Alabama","Georgia")),]
indices_data$resSeg



#write.csv(chronic_state,"./DataUpload/Yubin/chronic_state.csv",row.names=FALSE,na="")

indices_data[which(indices_data$nation == 1),]$anycondition <- mean(chronic_state$anycondition)
indices_data[which(indices_data$nation == 1),]$obesity <- mean(chronic_state$obesity)
indices_data[which(indices_data$nation == 1),]$heartdisease <- mean(chronic_state$heartdisease)
indices_data[which(indices_data$nation == 1),]$copd <- mean(chronic_state$copd)
indices_data[which(indices_data$nation == 1),]$diabetes <- mean(chronic_state$diabetes)
indices_data[which(indices_data$nation == 1),]$ckd <- mean(chronic_state$ckd)
indices_data[which(indices_data$nation == 1),]$anyconditionNumber <- mean(chronic_state$anyconditionNumber )
indices_data[which(indices_data$nation == 1),]$obesityNumber  <- mean(chronic_state$obesityNumber )
indices_data[which(indices_data$nation == 1),]$heartdiseaseNumber  <- mean(chronic_state$heartdiseaseNumber )
indices_data[which(indices_data$nation == 1),]$copdNumber  <- mean(chronic_state$copdNumber )
indices_data[which(indices_data$nation == 1),]$diabetesNumber  <- mean(chronic_state$diabetesNumber )
indices_data[which(indices_data$nation == 1),]$ckdNumber  <- mean(chronic_state$ckdNumber )
indices_data[which(indices_data$nation == 1),]$resSeg  <- mean(resSeg_state$resSeg )
indices_data[which(indices_data$nation == 1),]$ccvi  <- mean(ccvi_state$ccvi )



########### EXPORTING INDICES DATA ############
setwd(box1)
write.csv(indices_data,"./DataUpload/Yubin/nationalraw.csv",row.names=FALSE,na="")
setwd(local)
write.csv(indices_data,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/nationalraw.csv",row.names=FALSE,na="")
##########Vaccination Data####################

#vaccine<-read.csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/raw_data/vaccine_data_us_state_timeline.csv")
#vaccine$fips=fips(vaccine$Province_State)
#names(vaccine)[names(vaccine) == "Province_State"] <- "statename"
#keep<-c("statename","Population")
#population<-final_merged_nationalraw[keep]
#population<-subset(population,!statename%in%c(""))
#vaccine1<-dplyr::left_join(x = vaccine , y = population)
#vaccine1$percentVaccinated<-vaccine1$people_total/vaccine1$Population
#vaccine1$people_total<-na.locf(vaccine1$people_total)
#variables<-c("doses_alloc_total","doses_alloc_moderna","doses_alloc_pfizer","doses_alloc_unknown","doses_shipped_total","doses_shipped_moderna","doses_shipped_pfizer","doses_shipped_unknown","doses_admin_total","doses_admin_moderna","doses_admin_pfizer","doses_admin_unknown","people_total","people_total_2nd_dose","percentVaccinated","Population")

#vaccine1[, variables][is.na(vaccine1[, variables])] <- -1
#vaccine1$percentVaccinated<-paste(round(100*vaccine1$percentVaccinated, 2), "%", sep="")
#vaccine1<-na_if(vaccine1,"-100%")
#vaccine1[, variables][is.na(vaccine1[, variables])] <- -1
#write.csv(vaccine1,"./DataUpload/Yubin/VaccineTracker.csv",row.names=FALSE,na="")

library(RJSONIO)
library(RCurl)

########### EXPORTING VACCINE DATA ############

VaccineTracker<-read.csv("/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackertimeseries.csv")
#write.csv(casesdata,"./DataUpload/Yubin/VaccineTracker.csv",row.names=FALSE,na="")
#VaccineTracker<-read.csv("./DataUpload/Yubin/VaccineTrackertimeseries.csv")
#VaccineTracker<- VaccineTracker[ ,c(1:2,17,3:16)]
VaccineTracker<-VaccineTracker[,-c(29:31)]
#VaccineTracker<-VaccineTracker[,-c(10,12)]
#VaccineTracker$Administered_Janssen<--1
#VaccineTracker<-VaccineTracker[,-c(26,27,28)]

########### TODAY'S VACCINE DATA ############
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
#VaccineTracker$Distributed_Pfizer<--1
#VaccineTracker$Distributed_Moderna<--1
#VaccineTracker$Distributed_Janssen<--1
#VaccineTracker$Distributed_Unk_Manuf<--1
#VaccineTracker$Series_Complete_Pfizer<--1
#VaccineTracker$Series_Complete_Moderna<--1
#VaccineTracker$Series_Complete_Janssen<--1
#VaccineTracker$Series_Complete_Unk_Manuf<--1

#casesdata$Dist_Per_100K[casesdata$statename=="United States"]<-0
#casesdata$Admin_Per_100K[casesdata$statename=="United States"]<-0
#casesdata$Administered_Dose1_Per_100K[casesdata$statename=="United States"]<-0
#casesdata$Administered_Dose2_Per_100K[casesdata$statename=="United States"]<-0
#casesdata$Population[casesdata$statename=="United States"]<-0
#casesdata$percentVaccinated[casesdata$statename=="United States"]<-0


#casesdata$Dist_Per_100K[casesdata$statename=="United States"]<-mean(casesdata$Dist_Per_100K)

#casesdata$Admin_Per_100K[casesdata$statename=="United States"]<-mean(casesdata$Admin_Per_100K)
#casesdata$Administered_Dose1_Per_100K[casesdata$statename=="United States"]<-mean(casesdata$Administered_Dose1_Per_100K)
#casesdata$Administered_Dose2_Per_100K[casesdata$statename=="United States"]<-mean(casesdata$Administered_Dose2_Per_100K)
#casesdata$Population[casesdata$statename=="United States"]<-sum(casesdata$Population)
#casesdata$percentVaccinated[casesdata$statename=="United States"]<-casesdata$Administered_Dose1[casesdata$statename=="United States"]/casesdata$Population[casesdata$statename=="United States"]*100
#casesdata$percentVaccinated<-gsub("%", "", casesdata$percentVaccinated)
#casesdata$percentVaccinated=as.numeric(casesdata$percentVaccinated)
#casesdata$percentVaccinated=round(casesdata$percentVaccinated, 2)

#names(casesdata)[names(casesdata) == "percentVaccinated"] <- "percentVaccinatedDose1"
#casesdata$percentVaccinatedDose2<-casesdata$Administered_Dose2/casesdata$Population*100
#casesdata$percentVaccinatedDose2=round(casesdata$percentVaccinatedDose2, 2)

casesdata

#casesdata$Dist_Per_100K_new<-VaccineTracker9$Dist_Per_100K_new[VaccineTracker9$Date=="2021-01-25"]
#VaccineTracker9$Dist_Per_100K_new[VaccineTracker9$Date=="2021-01-24"]

#casesdata<-casesdata[,-c(13,14)]
casesdata
names(casesdata)
names(VaccineTracker)
########### MERGE TODAY WITH PREVIOUS VACCINE DATA (CUMULATIVE) ############


casesdata$AdministeredPartial<-casesdata$Administered_Dose1-casesdata$Series_Complete_Yes
casesdata$PercentAdministeredPartial<-casesdata$AdministeredPartial/casesdata$Census2019*100
casesdata$PercentAdministeredPartial<-round(casesdata$PercentAdministeredPartial,1)
casesdata$percentReceived<-casesdata$Doses_Administered/casesdata$Doses_Distributed
casesdata$percentReceived<-round(casesdata$percentReceived,1)
casesdata<-casesdata[,-c(26:27)]
casesdata<-casesdata[,-c(16)]


VaccineTracker1<-rbind(VaccineTracker,casesdata)

#VaccineTracker1[order(VaccineTracker1[,2], VaccineTracker1[, 1]),]
VaccineTracker1<-VaccineTracker1[
  with(VaccineTracker1, order(statename, Date)),
]
#VaccineTracker1$percentVaccinated<-gsub("%", "", VaccineTracker1$percentVaccinated)
#VaccineTracker1$percentVaccinated=as.numeric(VaccineTracker1$percentVaccinated)


#VaccineTracker1[, replace][is.na(VaccineTracker1[, replace])] <- -1
VaccineTracker1$FIPS[VaccineTracker1$FIPS==-1] <-"_nation"
VaccineTracker1$FIPS[VaccineTracker1$FIPS==""] <-"_nation"


#VaccineTracker1<-unique(VaccineTracker1)
#VaccineTracker2<-subset(VaccineTracker1,statename!="United States")
#Dist_Per_100K_total<-aggregate(Dist_Per_100K~Date,data=VaccineTracker2,mean)
#nation<-subset(VaccineTracker1,statename=="United States")
#nation<-nation %>%
#select(-c("Dist_Per_100K"))
  #VaccineTracker3<-left_join(Dist_Per_100K_total,nation)

#VaccineTracker3<-VaccineTracker3 %>%
#select(-c("Admin_Per_100K"))
#Admin_Per_100K_total<-aggregate(Admin_Per_100K~Date,data=VaccineTracker2,mean)
#VaccineTracker4<-left_join(Admin_Per_100K_total,VaccineTracker3)

#VaccineTracker4<-VaccineTracker4 %>%
#select(-c("Administered_Dose1_Per_100K"))
#Administered_Dose1_Per_100K_total<-aggregate(Administered_Dose1_Per_100K~Date,data=VaccineTracker2,mean)
#VaccineTracker5<-left_join(Administered_Dose1_Per_100K_total,VaccineTracker4)

#VaccineTracker5<-VaccineTracker5 %>%
#select(-c("Administered_Dose2_Per_100K"))
#Administered_Dose2_Per_100K_total<-aggregate(Administered_Dose2_Per_100K~Date,data=VaccineTracker2,mean)
#VaccineTracker6<-left_join(Administered_Dose2_Per_100K_total,VaccineTracker5)

#VaccineTracker6<-VaccineTracker6 %>%
#select(-c("Population"))
#Population_total<-aggregate(Population~Date,data=VaccineTracker2,sum)
#VaccineTracker7<-left_join(Population_total,VaccineTracker6)
#VaccineTracker7$percentVaccinated=VaccineTracker7$Administered_Dose1/VaccineTracker7$Population*100
#VaccineTracker7<-VaccineTracker7[,-c(19)]
#VaccineTracker8<-rbind(VaccineTracker2,VaccineTracker7)

#VaccineTracker8$percentVaccinated=round(VaccineTracker8$percentVaccinated, 2)
#VaccineTracker8<-VaccineTracker8[
  #with(VaccineTracker8, order(statename, Date)),
#]
detach("package:plyr", unload = TRUE)
VaccineTracker9<-VaccineTracker1%>%group_by(statename) %>%
 mutate(Dist_Per_100K_new = Dist_Per_100K - lag(Dist_Per_100K, default = 0))%>%
  mutate(Dist_Per_100K_new=ifelse (Dist_Per_100K_new<0,0,Dist_Per_100K_new))


VaccineTracker9<-VaccineTracker9[
  with(VaccineTracker9, order(statename, Date)),
]
VaccineTracker9$Dist_Per_100K_new=as.numeric(VaccineTracker9$Dist_Per_100K_new)


#names(VaccineTracker9)[names(VaccineTracker9) == "percentVaccinated"] <- "percentVaccinatedDose1"
#VaccineTracker9$percentVaccinatedDose2<-VaccineTracker9$Administered_Dose2/VaccineTracker9$Population*100
#VaccineTracker9$percentVaccinatedDose2=round(VaccineTracker9$percentVaccinatedDose2, 2)
#VaccineTracker9<-VaccineTracker9[,-c(17)]

VaccineTracker0<-VaccineTracker9%>%group_by(statename) %>%
  mutate(Dist_new = Doses_Distributed - lag(Doses_Distributed, default = 0))%>%
  mutate(Dist_new=ifelse (Dist_new<0,0,Dist_new))

VaccineTracker0<-VaccineTracker0[
  with(VaccineTracker0, order(statename, Date)),
]
casesdata<-casesdata[
  with(casesdata, order(statename, Date)),
]
#VaccineTracker0<-VaccineTracker0[-c(361),]



#VaccineTracker0$Dist_Per_100K_new<-ifelse(VaccineTracker0$Dist_Per_100K_new==0,lag(VaccineTracker0$Dist_Per_100K_new),VaccineTracker0$Dist_Per_100K_new)

  
VaccineTracker0<-VaccineTracker0%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0,lag(Date),Date)) %>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0,lag(Dist_new),Dist_new))

VaccineTracker0<-VaccineTracker0%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0,lag(distDate),distDate)) %>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0,lag(Dist_new),Dist_new))

#VaccineTracker0<-VaccineTracker0%>%group_by(statename)%>%mutate(Series_Complete_Yes=ifelse(is.na(Series_Complete_Yes),Administered_Dose2,Series_Complete_Yes))


casesdata$Dist_Per_100K_new<-VaccineTracker0$Dist_Per_100K_new[VaccineTracker0$Date=="2021-03-24"]
casesdata$Dist_new<-VaccineTracker0$Dist_new[VaccineTracker0$Date=="2021-03-24"]
casesdata$distDate<-VaccineTracker0$distDate[VaccineTracker0$Date=="2021-03-24"]

VaccineTracker0$FIPS<-str_remove(VaccineTracker0$FIPS, "^0+")
VaccineTracker0$PercentAdministeredPartial<-round(VaccineTracker0$PercentAdministeredPartial,1)
VaccineTracker0$percentReceived<-round(VaccineTracker0$percentReceived,1)
#VaccineTracker0$percentReceived<-VaccineTracker0$Doses_Administered/VaccineTracker0$Doses_Distributed
#VaccineTracker0$percentReceived<-round(VaccineTracker0$percentReceived,2)

#casesdata1$PercentAdministeredPartial

#VaccineTracker0$Series_Complete_Yes<-NA
#VaccineTracker0$Series_Complete_Yes[VaccineTracker0$Date=="2021-03-15"]<-casesdata1$Series_Complete_Yes


#VaccineTracker0$AdministeredPartial<-VaccineTracker0$Administered_Dose1-VaccineTracker0$Series_Complete_Yes
#VaccineTracker0$PercentAdministeredPartial<-VaccineTracker0$AdministeredPartial/VaccineTracker0$Census2019*100
#VaccineTracker0$PercentAdministeredPartial<-round(VaccineTracker0$PercentAdministeredPartial,2)
#VaccineTracker0<-VaccineTracker0[,-c(10)]

raw<- jsonlite::fromJSON(today)[[2]] 
raw<-raw[,c(4,18:25,27:29,38:50)]
names(raw)[1]<-"statename"
raw$statename[raw$statename== "New York State"] <-"New York"
casesdata1<-left_join(casesdata,raw)
casesdata1[is.na(casesdata1)] <- -1
#casesdata2<-casesdata1[,-c(26:44,46:49)]


#write.csv(VaccineTracker0,"./VaccineTrackertimeseries.csv")
write.csv(VaccineTracker0,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackertimeseries.csv",row.names=FALSE,na="")
write.csv(casesdata1,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackerstatic.csv",row.names=FALSE,na="")
write.csv(VaccineTracker0,"./VaccineTrackertimeseries.csv",row.names=FALSE,na="")
write.csv(casesdata1,"./VaccineTrackerstatic.csv",row.names=FALSE,na="")




