
# Pooja's box sync folder path
box = "/Users/poojanaik/Box Sync/COVID19_data_shared"


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
if (!require("janitor")){
  install.packages("janitor")}


library(RJSONIO)
library(RCurl)
library(base)
library(tidyverse)
library(tidyr)
library(plyr)
library(readr)
library(dplyr)
library(data.table)
library(ggplot2)
library(tibble)
library(lubridate)
library(cdlTools)
library(openintro)
library(gtools)
library(janitor)


##### DOWNLOAD DATA : CASES BY RACE #########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")


casesdata <- jsonlite::fromJSON(raw_data)[[6]]   #for race by age data
casesdata$race_eth_new <- revalue(casesdata$race_eth_new, c("White, Non-Hispanic"="Non-Hispanic White", 
                                                            "Black, Non-Hispanic"="Non-Hispanic African American",
                                                            "American Indian / Alaska Native, Non-Hispanic" = "Non-Hispanic American Natives",
                                                            "Asian, Non-Hispanic" = "Non-Hispanic Asian",
                                                            "Native Hawaiian / Other Pacific Islander, Non-Hispanic" = "Non-Hispanic NHPI",
                                                            "Multiple/Other, Non-Hispanic"="Non-Hispanic Other Race",
                                                            "Unknown" = "Unknown",
                                                            "Hispanic/Latino" = "Hispanic"))

names(casesdata)

casesracecdc <- gather(casesdata, 
                       key = "age",
                       value = "cases",
                       c("0_4_years","5_17_years","18_29_years","30_39_years","40_49_years","50_64_years","65_74_years","75_84_years","Over_85_years","Unknown","Grand_Total")) %>%
  select(race_eth_new,age,cases) %>% mutate(age = gsub("_years","",age)) %>% dplyr::rename(demographic=race_eth_new)


casesracecdcper <- gather(casesdata, 
                          key = "age",
                          value = "percentCases",
                          c("col_per_0_4","col_per_5_17","col_per_18_29","col_per_30_39","col_per_40_49","col_per_50_64","col_per_65_74","col_per_75_84","col_per_Over_85","col_per_Unknown","col_per_Grand_Total")) %>%
  select(race_eth_new,age,percentCases) %>% mutate(age = gsub("col_per_","",age)) %>% dplyr::rename(demographic=race_eth_new)


casesbyrace <- join(casesracecdc,casesracecdcper)

table(casesbyrace$demographic)

casesbyrace %>% filter(age=="Grand_Total")


dttcases <- casesbyrace %>% filter(age=="Grand_Total") %>% group_by(age) %>% mutate(totalcases = sum(cases))
totalcases <- as.numeric(dttcases$totalcases[dttcases$age=="Grand_Total" & dttcases$demographic == "Unknown"])

cases <- casesbyrace %>% mutate("totalcases" = totalcases, missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA))


rm(casesdata,casesbyrace,totalcases,dttcases,casesracecdc,casesracecdcper,raw_data)



###################  DEATHS BY RACE DATA ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathdata <- jsonlite::fromJSON(raw_data)[[7]]   #for race by age data
deathdata$race_eth_new <- revalue(deathdata$race_eth_new, c("White, Non-Hispanic"="Non-Hispanic White", 
                                                            "Black, Non-Hispanic"="Non-Hispanic African American",
                                                            "American Indian / Alaska Native, Non-Hispanic" = "Non-Hispanic American Natives",
                                                            "Asian, Non-Hispanic" = "Non-Hispanic Asian",
                                                            "Native Hawaiian / Other Pacific Islander, Non-Hispanic" = "Non-Hispanic NHPI",
                                                            "Multiple/Other, Non-Hispanic"="Non-Hispanic Other Race",
                                                            "Hispanic/Latino" = "Hispanic"))

names(deathdata)

deathracecdc <- gather(deathdata, 
                       key = "age",
                       value = "deaths",
                       c("0_4_years","5_17_years","18_29_years","30_39_years","40_49_years","50_64_years","65_74_years","75_84_years","Over_85_years","Unknown","Grand_Total")) %>%
  select(race_eth_new,age,deaths) %>% mutate(age = gsub("_years","",age)) %>% dplyr::rename(demographic=race_eth_new)


deathracecdcper <- gather(deathdata, 
                          key = "age",
                          value = "percentDeaths",
                          c("col_per_0_4","col_per_5_17","col_per_18_29","col_per_30_39","col_per_40_49","col_per_50_64","col_per_65_74","col_per_75_84","col_per_Over_85","col_per_Unknown","col_per_Grand_Total")) %>%
  select(race_eth_new,age,percentDeaths) %>% mutate(age = gsub("col_per_","",age)) %>% dplyr::rename(demographic=race_eth_new)


deathsbyrace <- join(deathracecdc,deathracecdcper)

table(deathsbyrace$demographic)

deathsbyrace %>% filter(age=="Grand_Total")

dttdeath <- deathsbyrace %>% filter(age=="Grand_Total") %>% group_by(age) %>% mutate(totaldeaths = sum(deaths))
totaldeath <- as.numeric(dttdeath$totaldeaths[dttdeath$age=="Grand_Total" & dttdeath$demographic == "Unknown"])

deaths <- deathsbyrace %>% mutate("totaldeaths" = totaldeath, missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA))


rm(deathdata,deathsbyrace,totaldeath,dttdeath,deathracecdc,deathracecdcper,raw_data)


############## JOIN CASES AND DEATHS DATA #############

raceagecdc <- full_join(cases,deaths) %>% 
  mutate(demogLabel = gsub("Non-Hispanic ","",demographic)) 


rm(cases,deaths)


############  POPULATION DATA  #############


setwd(box)
raceageUS <- read.csv("./RaceData/raceagepopUS.csv") %>% dplyr::rename(demogPop=racePop,demographic=race)

raceageUS$popUS[raceageUS$popUS == 328239523] <- 327167439

raceUS <- full_join(raceagecdc,raceageUS) %>% filter(!age=="Unknown") %>% 
  mutate(age2 = ifelse(age=="Grand_Total","All age groups",ifelse(age=="Over_85","Over 85 years",gsub("_"," to ",age))), 
         agelbl = ifelse(age=="Grand_Total","All age groups",ifelse(age=="Over_85","Over 85 years",paste(age2,"years",sep=" ")))) %>% 
  select(-age2) %>%
  mutate(date=as.Date(Sys.Date(),origin="1988-01-01")) %>% select(date,everything())


rm(raceagecdc,raceageUS)


raceNation <- raceUS %>% filter(age=="Grand_Total") %>%
  mutate(caserate=round(100000*(cases/demogPop),2),
         deathrate=round(100000*(deaths/demogPop),2)) %>%
  select(-raceagePop,-agePop,-percentRaceageagePop,-percentRaceageracePop,-age,-agelbl)


whiteCR <- raceNation %>% 
  filter(demographic=="Non-Hispanic White") %>% 
  select(date,demographic,caserate) %>% 
  dplyr::rename(WhiteCR=caserate) %>% 
  select(-demographic)

crr = join(raceNation,whiteCR) %>% mutate(CaserateRatio = round(caserate/WhiteCR,2))

HwhiteDR <- raceNation %>% filter(demographic=="Non-Hispanic White") %>% select(date,demographic,deathrate) %>% dplyr::rename(WhiteDR=deathrate) %>% select(-demographic)

drr = join(raceNation,HwhiteDR) %>% mutate(DeathrateRatio = round(deathrate/WhiteDR,2))

raceUS_final <- join(crr,drr) %>% filter(!demographic=="Non-Hispanic Other Race") %>%
  select(-WhiteCR,-WhiteDR) %>% mutate(demographicVar="Race")


rm(crr,drr,HwhiteDR,raceNation,raceUS,whiteCR)


######## MERGING WITH PREVIOUS DATA #########
# 
# raceUS0 <- read.csv("./DataUpload/RaceData/RaceNation/CDCRaceAgeNationData.csv") %>% mutate(date=as.Date(date,origin="1988-01-01"))
# CDCraceUS <- full_join(raceUS,raceUS0) 
# 
# 
# 
# ##########  EXPORT DATA ##################
# 
# setwd(box)
# write.csv(CDCraceUS,"./DataUpload/RaceData/RaceNation/CDCRaceAgeNationData.csv",na="",row.names=F)

# setwd(box)
# write.csv(raceUS_final,"./DataUpload/RaceData/RaceNation/nationRaceData.csv",na="",row.names=F)




#########  DOWNLOAD DATA : CASES BY AGE  ###########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

casesdata <- jsonlite::fromJSON(raw_data)[[2]] %>% 
  select(-b) %>% 
  dplyr::rename(cases=count,demographic=age_group,percentCases=Percent) %>%
  mutate(totalcases = sum(cases)) 

casesage <- data.frame(adorn_totals(casesdata, fill = "",name="total", na.rm = TRUE)) %>%
  mutate(totalcases=ifelse(demographic=="total",totalcases/10,totalcases))

table(casesage$demographic)


####### CASES BY AGE: pulling population by age data from census ######

agepop <- read.csv("./DataUpload/RaceData/RaceNation/age_censusdata.csv") %>% 
  select(agecat,totPop) %>%
  dplyr::rename(demogPop=totPop,demographic=agecat) %>%
  slice(1:10)

str(agepop$demographic)
str(casesage$demographic)
agepop$demographic <- revalue(agepop$demographic, c("0 - 4 years"="0 - 4 Years", 
                                                    "18 - 29 years"="18 - 29 Years",
                                                    "30 - 39 years" = "30 - 39 Years",
                                                    "40 - 49 years" = "40 - 49 Years",
                                                    "5 - 17 years" = "5 - 17 Years",
                                                    "50 - 64 years"="50 - 64 Years",
                                                    "65 - 74 years" = "65 - 74 Years",
                                                    "75 - 84 years" = "75 - 84 Years",
                                                    "85+ years" = "85+ Years"))

########## CASES BY AGE: merging based on age groups ###############

agecase_merged <- join(casesage,agepop) %>%
  mutate(caserate=ifelse(demographic=="total",NA,round(100000*(cases/demogPop),2)),
         popUS=327167439,
         percentPop=round(demogPop/popUS*100,2),
         "totalcases" = totalcases, 
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),
         availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA)) %>%
  filter(!demographic=="total")


#########  DOWNLOAD DATA : DEATHS BY AGE  ###########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathsdata <- jsonlite::fromJSON(raw_data)[[3]] %>% 
  select(-b) %>% 
  dplyr::rename(deaths=count,demographic=age_group,percentDeaths=Percent) %>%
  mutate(totaldeaths = sum(deaths)) 

deathsage <- data.frame(adorn_totals(deathsdata, fill = "",name="total", na.rm = TRUE)) %>%
  mutate(totaldeaths=ifelse(demographic=="total",totaldeaths/10,totaldeaths))

table(deathsage$demographic)


####### DEATHS BY AGE: pulling population by age data from census ######

agepop <- read.csv("./DataUpload/RaceData/RaceNation/age_censusdata.csv") %>% 
  select(agecat,totPop) %>%
  dplyr::rename(demogPop=totPop,demographic=agecat) %>%
  slice(1:10)

str(agepop$demographic)
str(deathsage$demographic)
agepop$demographic <- revalue(agepop$demographic, c("0 - 4 years"="0 - 4 Years", 
                                                    "18 - 29 years"="18 - 29 Years",
                                                    "30 - 39 years" = "30 - 39 Years",
                                                    "40 - 49 years" = "40 - 49 Years",
                                                    "5 - 17 years" = "5 - 17 Years",
                                                    "50 - 64 years"="50 - 64 Years",
                                                    "65 - 74 years" = "65 - 74 Years",
                                                    "75 - 84 years" = "75 - 84 Years",
                                                    "85+ years" = "85+ Years"))

########## DEATHS BY AGE: merging based on age groups ###############

agedeaths_merged <- join(deathsage,agepop) %>%
  mutate(deathrate=ifelse(demographic=="total",NA,round(100000*(deaths/demogPop),2)),
         popUS=327167439,
         percentPop=round(demogPop/popUS*100,2),
         "totaldeaths" = totaldeaths, 
         missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),
         availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA)) %>%
  filter(!demographic=="total")



########## CASES AND DEATHS BY AGE: merging based on age groups ###############

covid_age <- join(agecase_merged,agedeaths_merged) %>% mutate(demogLabel=demographic,date=as.Date(Sys.Date(),origin="1988-01-01"))  %>%
  mutate(demographicVar="Age")

covid_age$demographic <- recode(covid_age$demographic,"Unknown"="Unknown Age")

########  remove unnecessary dataframes ########

rm(agedeaths_merged,agepop,deathsage,raw_data,agecase_merged,casesage,casesdata,deathsdata)

# setwd(box)
# write.csv(covid_age,"./DataUpload/RaceData/RaceNation/nationAgeData.csv",na="",row.names=F)

prefinal_data <- full_join(raceUS_final,covid_age) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  mutate(demogLabel=ifelse(demographicVar=="Age",gsub(" Years","",demogLabel),demogLabel)) %>%
  select(date,demographicVar,demographic,demogLabel,popUS,demogPop,percentPop,cases,totalcases,percentCases,availableCases,missingCases,caserate,deaths,totaldeaths,percentDeaths,availableDeaths,missingDeaths,deathrate) 





########## CASES  BY SEX: merging based on sex ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

casesdata_sex <- jsonlite::fromJSON(raw_data)[[4]] %>% 
  filter(!sex_new=="Grand_Total") %>%
  select(sex_new,Grand_Total,col_per_Grand_Total) %>% 
  dplyr::rename(cases=Grand_Total,demographic=sex_new,percentCases=col_per_Grand_Total) %>%
  mutate(totalcases = sum(cases)) 



casessex <- data.frame(adorn_totals(casesdata_sex, fill = "",name="total", na.rm = TRUE)) %>%
  mutate(totalcases=ifelse(demographic=="total",totalcases/4,totalcases))


####### CASES BY SEX: pulling population by sex data from census ######

sexpop <- read_csv("./DataUpload/RaceData/RaceNation/sex_censusdata.csv") %>% 
  dplyr::rename(demogPop=popUS,demographic=sex)



str(sexpop$demographic)
str(casessex$demographic)


########## CASES BY SEX: merging based on age groups ###############

sexcase_merged <- join(casessex,sexpop) %>%
  mutate(caserate=ifelse(demographic=="total",NA,round(100000*(cases/demogPop),2)),
         popUS=327167439,
         percentPop=round(demogPop/popUS*100,2),
         "totalcases" = totalcases, 
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),
         availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA)) %>%
  filter(!demographic=="total")



########## DEATHS  BY SEX: merging based on sex ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathsdata_sex <- jsonlite::fromJSON(raw_data)[[5]] %>% 
  filter(!sex_new=="Grand_Total") %>%
  select(sex_new,Grand_Total,col_per_Grand_Total) %>% 
  dplyr::rename(deaths=Grand_Total,demographic=sex_new,percentDeaths=col_per_Grand_Total) %>%
  mutate(totaldeaths = sum(deaths)) 

deathsTotSex <- as.numeric(jsonlite::fromJSON(raw_data)[[5]] %>% filter(sex_new=="Grand_Total") %>% 
                             select(Grand_Total))

deathssex <- data.frame(adorn_totals(deathsdata_sex, fill = "",name="total", na.rm = TRUE)) %>%
  mutate(totaldeaths=ifelse(demographic=="total",totaldeaths/4,totaldeaths))

deathssex$deaths[deathssex$demographic=="total"] <- deathsTotSex

deathssex$totaldeaths <- deathsTotSex

####### DEATHS BY SEX: pulling population by sex data from census ######

sexpop <- read_csv("./DataUpload/RaceData/RaceNation/sex_censusdata.csv") %>% 
  dplyr::rename(demogPop=popUS,demographic=sex)



str(sexpop$demographic)
str(casessex$demographic)


########## DEATHS BY SEX: merging based on sex groups ###############

sexdeaths_merged <- join(deathssex,sexpop) %>%
  mutate(deathsrate=ifelse(demographic=="total",NA,round(100000*(deaths/demogPop),2)),
         popUS=327167439,
         percentPop=round(demogPop/popUS*100,2),
         "totaldeaths" = totaldeaths, 
         missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),
         availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA)) %>%
  filter(!demographic=="total")

########## CASES AND DEATHS BY SEX: merging based on sex groups ###############

covid_sex <- join(sexcase_merged,sexdeaths_merged) %>% mutate(demogLabel=demographic,date=as.Date(Sys.Date(),origin="1988-01-01"))  %>%
  mutate(demographicVar="Sex")

covid_sex$demographic <- recode(covid_sex$demographic,"Unknown"="Unknown Sex")


########  remove unnecessary dataframes ########

rm(sexdeaths_merged,sexpop,deathssex,raw_data,sexcase_merged,casessex,casesdata_sex,deathsdata_sex)

# setwd(box)
# write.csv(covid_age,"./DataUpload/RaceData/RaceNation/nationAgeData.csv",na="",row.names=F)

final_data <- full_join(prefinal_data,covid_sex) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  # mutate(demogLabel=ifelse(demographicVar=="Age",gsub(" Years","",demogLabel),demogLabel)) %>%
  select(date,demographicVar,demographic,demogLabel,popUS,demogPop,percentPop,cases,totalcases,percentCases,availableCases,missingCases,caserate,deaths,totaldeaths,percentDeaths,availableDeaths,missingDeaths,deathrate) 


final_data$percentCases[final_data$demogLabel=="Unknown"] <- -9999
final_data$percentDeaths[final_data$demogLabel=="Unknown"] <- -9999
final_data$popUS[final_data$demogLabel=="Unknown"] <- -9999

setwd(box)
write.csv(final_data,"./DataUpload/RaceData/RaceNation/USDemogData.csv",na="",row.names=F)

rm(covid_age,covid_sex)

##############  VACCINATION BY RACE DATA ########################

cdcvacc <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_demographics_data")
cdcvacc2 <- jsonlite::fromJSON(cdcvacc)[[2]]

df <- cdcvacc2 %>% 
  dplyr::rename(date=Date,demogLabel=Demographic_category,admDose1=Administered_Dose1,pctAdmDose1=Administered_Dose1_pct_known,
                pctKnownAdmDose1=Administered_Dose1_pct_US,admDose2=Administered_Dose2,pctAdmDose2=Administered_Dose2_pct_known,
                pctKnownAdmDose2=Administered_Dose2_pct_US) %>%
  mutate(demographicVar = ifelse(str_detect(demogLabel, "Age"),"vaccineAge",ifelse(str_detect(demogLabel, "Race"),"race",ifelse(str_detect(demogLabel, "Sex"),"sex","total"))),
         demogLabel = ifelse(demographicVar=="vaccineAge",gsub("Ages_","",demogLabel),
                           ifelse(demographicVar=="race",gsub("Race_eth_","",demogLabel),
                                  ifelse(demographicVar=="sex",gsub("Sex_","",demogLabel),demogLabel))))

maxdate_v <- max(df$date)

df$admDose1[df$demogLabel=="US"] -> USTotDose1
df$admDose2[df$demogLabel=="US"] -> USTotDose2

head(df)

df[nrow(df) + 1,] <- c(maxdate_v,unknown,)




df2 <- df %>% mutate(demogLabel = ifelse(demographicVar=="vaccineAge",gsub("_yrs","",demogLabel),
                                       ifelse(demographicVar=="race",gsub("NH","",demogLabel),demogLabel)))


df2$demogLabel <- revalue(df2$demogLabel,c("<18yrs"="<18",
                                      "Age_known" = "Unknown",
                                      "Age_unknown" = "unknown",
                                      "known"="Unknown",
                                      "OPI" = "NHPI",
                                      "AIAN"="American Natives",
                                      "Black"="African American",
                                      "Mult_Oth"="Multiple/Other"))

finalvacc <- df2 %>% filter(!demogLabel=="US",!demogLabel=="unknown") %>%
  mutate(pctKnownAdmDose1 = ifelse(!demogLabel=="Unknown",-9999,pctKnownAdmDose1),
         pctKnownAdmDose2 = ifelse(!demogLabel=="Unknown",-9999,pctKnownAdmDose2),
         pctAdmDose1 = ifelse(demogLabel=="Unknown",-9999,pctAdmDose1),
         pctAdmDose2 = ifelse(demogLabel=="Unknown",-9999,pctAdmDose2)) %>%
  mutate(demographic = ifelse(demographicVar=="vaccineAge" & !demogLabel=="Unknown",paste(demogLabel,"years"),
                              ifelse(demographicVar=="race" & !demogLabel=="Unknown" & !demogLabel=="Hispanic",paste("Non-Hispanic",demogLabel),demogLabel)))


setwd(box)
popdata <- read.csv("./DataUpload/RaceData/RaceNation/USDemogData.csv") %>%
  mutate(demographicVar=tolower(demographicVar),
         demogLabel=ifelse(demographicVar=="age",gsub(" ","",demogLabel),demogLabel)) 

popdata$demographic[popdata$demographic=="Unknown Age"] <- "Unknown"
popdata$demographic[popdata$demographic=="Unknown Sex"] <- "Unknown"


numvar <- c("popUS","demogPop","percentPop","cases","totalcases","percentCases","availableCases","missingCases","caserate","deaths","totaldeaths","percentDeaths","availableDeaths","missingDeaths","deathrate")

maxdate_p <- max(popdata$date)

popdata[nrow(popdata) + 1,] = c(maxdate_p,"vaccineAge","<18 years","<18",-9999,-9999,6+16.42,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999)
popdata[nrow(popdata) + 1,] = c(maxdate_p,"vaccineAge","75+ years","75+",-9999,-9999,4.75+1.93,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999)


popdata[numvar] <- sapply(popdata[numvar],as.numeric)


mergedVacc <- full_join(finalvacc,popdata) %>%
  mutate(percentPop = ifelse(is.na(percentPop),-9999,percentPop)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  arrange(demographicVar)


rm(cdcvacc2,df,df2,final_data,finalvacc,popdata,prefinal_data,raceUS_final,cdcvacc,deathsTotSex,maxdate_p,maxdate_v,numvar,USTotDose1,USTotDose2)


setwd(box)
write.csv(mergedVacc,"./DataUpload/RaceData/RaceNation/USVaccineByDemog.csv",na="",row.names=F)


