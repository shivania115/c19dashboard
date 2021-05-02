

### Working on cleaning this
# Pooja's Onedrive sync folder path
loc = "/Users/poojanaik/Applications/OneDrive - Emory University/CovidHealthEquityDashboard/Data"

trial = "/Users/poojanaik/Desktop"

library(tidyverse)
library(tidyr)
library(plyr)
library(ggplot2)
library(tibble)
library(cdlTools)
library(dplyr)


rawpath <- "/Users/poojanaik/Applications/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Raw/RaceEthnicity"
setwd(trial)

# downloading data from github jhu
download.file(path=trial,"https://github.com/KFFData/COVID-19-Data/archive/refs/heads/kff_master.zip", destfile = "COVID-19-Data-kff_master.zip")
unzip(zipfile = "COVID-19-Data-kff_master.zip")


datadir = "./COVID-19-Data-kff_master/Race Ethnicity COVID-19 Data/Vaccines"
myfiles = list.files(path=datadir, pattern="*.csv", full.names=TRUE) %>%
  str_extract("./COVID-19-Data-kff_master/Race Ethnicity COVID-19 Data/Vaccines/[0-9]{8}_COVID19 Vaccinations by RE.csv") 
files <- complete.cases(myfiles)
finalfiles <- myfiles[files]


filedates = list.files(path=datadir, pattern="*.csv", full.names=TRUE) %>%
  str_extract("[0-9]{8}") %>%
  as.Date(format = '%Y%m%d') %>% unique()


kffvaccstate = as_tibble(ldply(finalfiles, read.csv)) %>% 
  select(-Footnotes) %>%
  mutate(date=rep(filedates,each=51)) %>% select(date,Location,everything()) %>%
  dplyr::rename(statename=Location,
                inclHispanic="Race.Categories.Include.Hispanic.Individuals",
                White="White...of.Vaccinations",
                African_American="Black...of.Vaccinations",
                Hispanic="Hispanic...of.Vaccinations",
                Asian="Asian...of.Vaccinations",
                American_Native="American.Indian.or.Alaska.Native...of.Vaccinations",
                NHPI="Native.Hawaiian.or.Other.Pacific.Islander...of.Vaccinations",
                Other_race="Other...of.Vaccinations",
                Known_race= "X..of.Vaccinations.with.Known.Race",                        
                Unknown_race= "X..of.Vaccinations.with.Unknown.Race" ,                      
                Known_ethnicity= "X..of.Vaccinations.with.Known.Ethnicity" ,                   
                Unknown_ethnicity= "X..of.Vaccinations.with.Unknown.Ethnicity" ) 

kffvaccstate[ , 3:13 ][ kffvaccstate[ ,3:13 ] == "<.01" ] <- "0.05"

str(kffvaccstate)  
varnam <- colnames(kffvaccstate)[4:14]
kffvaccstate[varnam] <- sapply(kffvaccstate[varnam],as.numeric)

kffvaccstate2 <- kffvaccstate %>%
  mutate_if(is.numeric, ~(.)*100) %>%
  # mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  mutate(inclNonhispanic = ifelse(inclHispanic=="" & Unknown_race==Unknown_ethnicity,1,NA),
         inclHispanic=ifelse(inclHispanic=="Yes",1,NA),
         Known_race_ethnicity=ifelse(inclNonhispanic==1,Known_race,NA))

kffvaccstate2 <- kffvaccstate2 %>% 
  select(date,statename,inclHispanic,inclNonhispanic,White,African_American,Asian,American_Native,NHPI, Other_race,everything())


kffvaccstate3 <- kffvaccstate2 %>% 
  mutate(sumRace = ifelse(inclHispanic==1,rowSums(.[5:10],na.rm=TRUE),NA),
         sumRaceEth = ifelse(inclNonhispanic==1,rowSums(.[5:11],na.rm=TRUE),NA),
         dataReported= case_when(
           is.na(White) ~ "No",
           TRUE ~ "Yes"),
         stateReports = case_when(
           dataReported=="No" ~ "None",
           dataReported=="Yes" & inclHispanic==1 ~ "Hisp and NonHisp Races",
           dataReported=="Yes" & inclNonhispanic==1 ~ "Non-Hispanic Races only", 
           TRUE ~ "Unknown")) 


vaccstate <- gather(kffvaccstate3,
                    key="race",
                    value="percentVaccinated",
                    5:16) %>%
  arrange(statename,race) %>%
  filter(!race=="Unknown_race",!race=="Unknown_ethnicity") %>%
  mutate(percentVaccinated=round(percentVaccinated,2),
         inclNonhispanic=ifelse(is.na(inclNonhispanic),0,inclNonhispanic),
         inclHispanic=ifelse(is.na(inclHispanic),0,inclHispanic),
         race=gsub("_"," ",race),
         racelbl = ifelse(inclNonhispanic==1 & !race=="Known ethnicity" & !race=="Known race" & !race=="Known race ethnicity" & !race=="Hispanic",paste("Non-Hispanic",race), race),
         pctVaccRace=ifelse(inclHispanic==1,percentVaccinated,NA),
         pctVaccRaceEthn=ifelse(inclNonhispanic==1,percentVaccinated,NA),
         pctKnownRace=ifelse(inclHispanic==1&race=="Known race",percentVaccinated,NA),
         pctKnownEthn=ifelse(inclHispanic==1&race=="Known ethnicity",percentVaccinated,NA),
         pctKnownRaceEthn=ifelse(inclNonhispanic==1&race=="Known race ethnicity",percentVaccinated,NA)) 


vaccstate2 <- vaccstate[!(vaccstate$race=="Known ethnicity"&vaccstate$inclNonhispanic==1),]
vaccstate3 <- vaccstate2[!(vaccstate2$race=="Known race"&vaccstate2$inclNonhispanic==1),]
vaccstate4 <- vaccstate3[!(vaccstate3$race=="Known race ethnicity"&vaccstate3$inclHispanic==1),]


x <- c("White","African American","American Native", "Asian","NHPI", "Other race","Hispanic","Known race","Known ethnicity","Known race ethnicity")


vaccstate_final <- vaccstate4 %>%
  mutate(race =  factor(race, levels = x)) %>%
  arrange(statename,race) %>%
  mutate(state=fips(statename, to = "FIPS")) %>%
  select(date,state,statename,inclHispanic,inclNonhispanic,race,racelbl,percentVaccinated,everything())



##################################

datadir = "./COVID-19-Data-kff_master/Race Ethnicity COVID-19 Data/Vaccines"
myfiles = list.files(path=datadir, pattern="*.csv", full.names=TRUE) %>%
  str_extract("./COVID-19-Data-kff_master/Race Ethnicity COVID-19 Data/Vaccines/[0-9]{8}_Distribution of Vaccinations, Cases, Deaths[ ,]? and Total Population by RaceEthnicity.csv") 
files <- complete.cases(myfiles)
finalfiles <- myfiles[files]
finalfiles

filedates = list.files(path=datadir, pattern="*.csv", full.names=TRUE) %>%
  str_extract("[0-9]{8}") %>%
  as.Date(format = '%Y%m%d') %>% unique()


raecdata = as_tibble(ldply(finalfiles, read.csv)) %>% 
  select(-Footnotes) %>% dplyr::rename(statename=State) %>% 
  join(read.csv("/Users/poojanaik/Applications/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/staticracedata.csv") %>% filter(race=="All Races Combined") %>% select(statename,state)) %>%
  filter(!is.na(state)) %>%
  mutate(date=rep(filedates,each=51)) %>%
  select(date,state,statename,everything()) %>% 
  gather("indicator","percent",4:19) %>% 
  mutate(race = sub(".Percent.of.*", "", indicator),
         metric= sub(".*.Percent.of.", "", indicator)) %>% select(-indicator) %>%
  spread(metric,percent) %>%
  dplyr::rename(percentPop=Total.Population,percentCases=Cases,percentDeaths=Deaths,vaccinations=Vaccinations)

raecdata[,5:8] <- sapply(raecdata[,5:8],as.numeric)


racevacc <- raecdata %>% 
  mutate_if(is.numeric, ~ round(. * 100)) %>%
  mutate(state=state/100,race=recode(race,"Black"="African American"))


fulldata_ts <- join(vaccstate_final,racevacc) %>%
  mutate_if(is.numeric,~replace(., is.na(.), -9999)) %>% 
  arrange(date,statename)


fulldata_static <- fulldata_ts %>% filter(date==max(date))


write.csv(fulldata_static,"/Users/poojanaik/Applications/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Processed/Kff Race and Ethnicity/kffstaterace_static.csv",row.names=F)
write.csv(fulldata_ts,"/Users/poojanaik/Applications/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Processed/Kff Race and Ethnicity/kffstaterace_ts.csv",row.names=F)



