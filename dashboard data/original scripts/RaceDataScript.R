

###########     SETTING WORKING DIRECTORY   ####################################

#get your working directory
getwd()

# Pooja's box sync folder path
box = "/Users/poojanaik/Box Sync/COVID19_data_shared"

box2 = "/Users/poojanaik/Box Sync/COVID19_data_shared/DataUpload"
# I recommend setting a local directory even when there's 
# going to be no file called from your local directory if you're linked to the box.

local = "/Users/poojanaik/Downloads"



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



########################      RACE DATA FROM COVID TRACKING PROJECT: FORMAT 1 (reporting Cases)      ###################

prerace <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR_xmYt4ACPDZCDJcY12kCiMiH0ODyx3E1ZvgOHB8ae1tRcjXbs_yWBOA4j4uoCEADVfC1PS2jYO68B/pub?gid=43720681&single=true&output=csv")

setwd(local)
racecategory <- read.csv("./racecategory.csv")


race <- full_join(prerace,racecategory) %>% 
  mutate(
    Cases_White = as.integer(Cases_White),
    totalCases = as.integer(Cases_Total),
    totalDeaths = Deaths_Total
  )

race$totalRaceCases <- apply(race[,c(4:11)], 1, sum,na.rm=TRUE)
race$totalEthnicityCases <- apply(race[,c(13:14)], 1, sum,na.rm=TRUE)
race$totalRaceDeaths <- apply(race[,c(17:24)], 1, sum,na.rm=TRUE)
race$totalEthnicityDeaths <- apply(race[,c(26:27)], 1, sum,na.rm=TRUE)

###################### HISPANIC:  FILTERING HISPANIC RACE DATA ##########################
race_hispanic <- race %>% filter(racecategory == "hispanic")


# date as character to data format 
race_hispanic <- transform(race_hispanic, Date = as.Date(as.character(Date), "%Y%m%d"))
race_hispanic <- dplyr::rename(race_hispanic,Cases_Hispanic=Cases_Ethnicity_Hispanic,
                               Cases_Nonhispanic = Cases_Ethnicity_NonHispanic ,
                               Deaths_Hispanic=Deaths_Ethnicity_Hispanic,
                               Deaths_Nonhispanic = Deaths_Ethnicity_NonHispanic,
                               Cases_unknownrace = Cases_Unknown,
                               Cases_unknownethnicity = Cases_Ethnicity_Unknown,
                               Deaths_unknownrace = Deaths_Unknown,
                               Deaths_unknownethnicity = Deaths_Ethnicity_Unknown,
                               date=Date)

table(racecategory$racecategory)

race_hispanic <- race_hispanic %>% 
  mutate(
    State = as.character(State),
    state = fips(State,to="FIPS"),
    county = "",
    nation = "",
    statename = abbr2state(State)
  ) %>% 
  filter(!statename=="") %>% 
  select(date,nation,state,statename,county,everything()) %>% 
  arrange(state) %>% 
  select(-State) 


Hcases <- gather(race_hispanic,
                 key = "race",
                 value = "cases",
                 c("Cases_Total":"Cases_unknownethnicity")) %>% 
  mutate(race <- as.character(race)) %>% 
  select(date,nation,state,statename,county,race,cases,racecategory,totalRaceCases,totalEthnicityCases,totalCases,totalDeaths) %>% 
  arrange(statename)



# renaming levels
Hcases$race <- revalue(Hcases$race, c("Cases_Total"="All Races Combined",
                                      "Cases_White"="White Alone", 
                                      "Cases_Black"="African American Alone",
                                      "Cases_Other"="Other Race Alone",
                                      "Cases_LatinX" = "LatinX Alone",
                                      "Cases_NHPI" = "NHPI Alone",
                                      "Cases_Multiracial" = "Two or more races Alone",
                                      "Cases_Asian" = "Asian Alone",
                                      "Cases_AIAN" = "American Natives Alone",
                                      "Cases_unknownrace" = "Race Missing",
                                      "Cases_Nonhispanic" = "Non Hispanic",
                                      "Cases_Hispanic"="Hispanic",
                                      "Cases_unknownethnicity" = "Ethnicity Missing"))                   


Hdeaths <- gather(race_hispanic,
                  key = "race",
                  value = "deaths",
                  c("Deaths_Total":"Deaths_unknownethnicity"))%>% 
  select(date,statename,race,deaths,racecategory,totalRaceDeaths,totalEthnicityDeaths,totalCases,totalDeaths) %>% 
  arrange(statename)



Hdeaths$race <- revalue(Hdeaths$race, c("Deaths_Total"="All Races Combined",
                                        "Deaths_White"="White Alone", 
                                        "Deaths_Black"="African American Alone",
                                        "Deaths_Other"="Other Race Alone",
                                        "Deaths_LatinX" = "LatinX Alone",
                                        "Deaths_NHPI" = "NHPI Alone",
                                        "Deaths_Multiracial" = "Two or more races Alone",
                                        "Deaths_Asian" = "Asian Alone",
                                        "Deaths_AIAN" = "American Natives Alone",
                                        "Deaths_unknownrace" = "Race Missing",
                                        "Deaths_Nonhispanic" = "Non Hispanic",
                                        "Deaths_Hispanic"="Hispanic",
                                        "Deaths_unknownethnicity" = "Ethnicity Missing"))                   


Hdata <- join(Hcases,Hdeaths) %>% 
  select(date,nation,state,statename,county,everything()) 




# keep the global environment clean 
rm(Hcases,Hdeaths,prerace,racecategory,race_hispanic)



################  HISPANIC: GETTING POPULATION DATA  ###################

setwd(box2)
deno <- read.csv("./RaceData/hispanicrace.csv")


HPop <- gather(deno,
               key="race",
               value="population",
               c("totalpopulation":"hispanic")) #%>% select(-racecategory)

names(deno)

HPop$race <- revalue(HPop$race, c("totalpopulation"="All Races Combined",
                                  "white"="White Alone", 
                                  "black"="African American Alone",
                                  "aian" = "American Natives Alone",
                                  "asian" = "Asian Alone",
                                  "nhpi" = "NHPI Alone",
                                  "otherrace"="Other Race Alone",
                                  "multirace" = "Two or more races Alone",
                                  "nonhispanic" = "Non Hispanic",
                                  "hispanic" = "Hispanic"))   



# keep the global environment clean 
rm(deno)


################ HISPANIC: MERGING POP AND RACE CASES DATA ####################



Hdata$cases <- as.integer(Hdata$cases)

Hrace4 <- left_join(Hdata,HPop)%>% 
  arrange(statename,desc(date)) %>%
  mutate(
    caserate = round((cases/population)*100000),
    deathrate = round((deaths/population)*100000)
  ) %>% filter(!race=="LatinX Alone")


whiteCR <- Hrace4 %>% filter(race=="White Alone") %>% select(date,statename,race,caserate) %>% dplyr::rename(WhiteCR=caserate) %>% select(-race)

Hrace_ts1 = join(Hrace4,whiteCR) %>% mutate(CaserateRatio = round(caserate/WhiteCR,2))

HwhiteDR <- Hrace4 %>% filter(race=="White Alone") %>% select(date,statename,race,deathrate) %>% dplyr::rename(WhiteDR=deathrate) %>% select(-race)

Hrace_ts2 = join(Hrace4,HwhiteDR) %>% mutate(DeathrateRatio = round(deathrate/WhiteDR,2))

Hrace_ts <- join(Hrace_ts1,Hrace_ts2)

table(Hrace_ts$race)

x <- c("All Races Combined","African American Alone","White Alone","American Natives Alone", "Asian Alone","NHPI Alone", "Other Race Alone","Two or more races Alone","Hispanic","Non Hispanic","Race Missing","Ethnicity Missing")

Hrace_ts <- Hrace_ts %>%
  mutate(race =  factor(race, levels = x)) %>%
  arrange(statename,date,race)


Htotal <- Hrace_ts %>% 
  group_by(statename) %>% 
  filter(row_number()==1) %>% select(statename,population)


Hfinal <- merge(Hrace_ts,Htotal,by.x="statename",by.y="statename")
Hfinal <- dplyr::rename(Hfinal,racePopulation=population.x,populationTot=population.y)



Hracetimeseries <- Hfinal %>% 
  mutate(
    percentPop = round((racePopulation/populationTot)*100),
    Show = ifelse(percentPop>1 & deaths>=30,"Yes","No"),
    dataType = ifelse(race == "Hispanic" | race == "Non Hispanic"|race=="Ethnicity Missing", "Ethnicity", "Race"),
    percentRaceCases =ifelse(race=="Race Missing" & dataType=="Race",round(100-((cases/totalCases)*100)), ifelse(!race=="All Races Combined" & dataType=="Race",round((cases/totalRaceCases)*100),NA)),
    percentEthnicityCases = ifelse(race=="Ethnicity Missing"& dataType=="Ethnicity",round(100-((cases/totalCases)*100)),ifelse(!race=="All Races Combined" & dataType=="Ethnicity",round((cases/totalEthnicityCases)*100),NA)),
    percentRaceDeaths = ifelse(race=="Race Missing"& dataType=="Race",round(100-((deaths/totalDeaths)*100)),ifelse(!race=="All Races Combined" & dataType=="Race",round((deaths/totalRaceDeaths)*100),NA)),
    percentEthnicityDeaths = ifelse(race=="Ethnicity Missing"& dataType=="Ethnicity",round(100-((deaths/totalDeaths)*100)),ifelse(!race=="All Races Combined" & dataType=="Ethnicity",round((deaths/totalEthnicityDeaths)*100),NA))
  ) %>% select(date,nation,county,state,statename,race,everything()) %>% arrange(statename,desc(date))



# keep the global environment clean 
rm(Hrace_ts,Htotal,HPop,Hdata,Hfinal,x)


Hstatic_race <- Hracetimeseries %>% filter(date==max(date))


###################### NON HISPANIC:  FILTERING NON HISPANIC RACE DATA ##########################
race_nonhispanic <- race %>% filter(racecategory == "nonhispanic")


# date as character to data format 
race_nonhispanic <- transform(race_nonhispanic, Date = as.Date(as.character(Date), "%Y%m%d"))
race_nonhispanic <- dplyr::rename(race_nonhispanic,Cases_Hispanic=Cases_Ethnicity_Hispanic,
                                  Cases_Nonhispanic = Cases_Ethnicity_NonHispanic ,
                                  Deaths_Hispanic=Deaths_Ethnicity_Hispanic,
                                  Deaths_Nonhispanic = Deaths_Ethnicity_NonHispanic,
                                  Cases_unknownrace = Cases_Unknown,
                                  Cases_unknownethnicity = Cases_Ethnicity_Unknown,
                                  Deaths_unknownrace = Deaths_Unknown,
                                  Deaths_unknownethnicity = Deaths_Ethnicity_Unknown,
                                  date=Date)


race_nonhispanic <- race_nonhispanic %>% 
  mutate(
    State = as.character(State),
    state = fips(State,to="FIPS"),
    county = "",
    nation = "",
    statename = abbr2state(State)
  ) %>% 
  filter(!statename=="") %>% 
  select(date,nation,state,statename,county,everything()) %>% 
  arrange(state) %>% 
  select(-State) 


NHcases <- gather(race_nonhispanic,
                  key = "race",
                  value = "cases",
                  c("Cases_Total":"Cases_Hispanic",-"Cases_LatinX",-"Cases_Nonhispanic")) %>% 
  mutate(race <- as.character(race)) %>% 
  select(date,nation,state,statename,county,race,cases,racecategory,totalRaceCases,totalEthnicityCases,totalCases,totalDeaths) %>% 
  arrange(statename)



# renaming levels 
NHcases$race <- revalue(NHcases$race, c("Cases_Total"="All Races Combined",
                                        "Cases_White"="Non-Hispanic White", 
                                        "Cases_Black"="Non-Hispanic African American",
                                        "Cases_Other"="Non hispanic Other Race",
                                        "Cases_NHPI" = "Non-Hispanic NHPI",
                                        "Cases_Multiracial" = "Non hispanic Two or more races",
                                        "Cases_Asian" = "Non-Hispanic Asian",
                                        "Cases_AIAN" = "Non-Hispanic American Natives",
                                        "Cases_unknownrace" = "Race & Ethnicity Missing",
                                        "Cases_Hispanic"="Hispanic"))                   


NHdeaths <- gather(race_nonhispanic,
                   key = "race",
                   value = "deaths",
                   c("Deaths_Total":"Deaths_Hispanic",-"Deaths_LatinX",-"Deaths_Nonhispanic"))%>% 
  select(date,statename,race,deaths,racecategory,totalRaceDeaths,totalEthnicityDeaths,totalDeaths) %>% 
  arrange(statename)



NHdeaths$race <- revalue(NHdeaths$race, c("Deaths_Total"="All Races Combined",
                                          "Deaths_White"="Non-Hispanic White", 
                                          "Deaths_Black"="Non-Hispanic African American",
                                          "Deaths_Other"="Non hispanic Other Race",
                                          "Deaths_NHPI" = "Non-Hispanic NHPI",
                                          "Deaths_Multiracial" = "Non hispanic Two or more races",
                                          "Deaths_Asian" = "Non-Hispanic Asian",
                                          "Deaths_AIAN" = "Non-Hispanic American Natives",
                                          "Deaths_unknownrace" = "Race & Ethnicity Missing",
                                          "Deaths_Hispanic"="Hispanic"))                  


NHdata <- join(NHcases,NHdeaths) %>% 
  select(date,nation,state,statename,county,everything())




# keep the global environment clean 
rm(NHcases,NHdeaths,race_nonhispanic)



################  NON HISPANIC: GETTING POPULATION DATA  ###################

setwd(box2)
deno <- read.csv("./RaceData/nonhispanicrace.csv")


NHPop <- gather(deno,
                key="race",
                value="population",
                c("totalpopulation":"nhmultirace")) #%>% select(-racecategory)

names(deno)

NHPop$race <- revalue(NHPop$race, c("totalpopulation"="All Races Combined",
                                    "nhwhite"="Non-Hispanic White", 
                                    "nhblack"="Non-Hispanic African American",
                                    "nhaian" = "Non-Hispanic American Natives",
                                    "nhasian" = "Non-Hispanic Asian",
                                    "nhnhpi" = "Non-Hispanic NHPI",
                                    "nhotherrace"="Non hispanic Other Race",
                                    "nhmultirace" = "Non hispanic Two or more races",
                                    "hispanic" = "Hispanic"))   



# keep the global environment clean 
rm(deno)


################ NON HISPANIC: MERGING POP AND RACE CASES DATA ####################



NHdata$cases <- as.integer(NHdata$cases)

NHrace4 <- left_join(NHdata,NHPop) %>% 
  arrange(statename,date) %>%
  mutate(
    caserate = round((cases/population)*100000),
    deathrate = round((deaths/population)*100000)
  )


NHwhiteCR <- NHrace4 %>% filter(race=="Non-Hispanic White") %>% select(date,statename,race,caserate) %>% dplyr::rename(WhiteCR=caserate) %>% select(-race)

NHrace_ts1 = join(NHrace4,NHwhiteCR) %>% mutate(CaserateRatio = round(caserate/WhiteCR,2))

NHwhiteDR <- NHrace4 %>% filter(race=="Non-Hispanic White") %>% select(date,statename,race,deathrate) %>% dplyr::rename(WhiteDR=deathrate) %>% select(-race)

NHrace_ts2 = join(NHrace4,NHwhiteDR) %>% mutate(DeathrateRatio = round(deathrate/WhiteDR,2))

NHrace_ts <- join(NHrace_ts1,NHrace_ts2)

table(NHrace_ts$race)

x <- c("All Races Combined","Non-Hispanic African American","Non-Hispanic White","Non-Hispanic American Natives", "Non-Hispanic Asian","Non-Hispanic NHPI", "Non hispanic Other Race","Non hispanic Two or more races","Hispanic","Race & Ethnicity Missing")

NHrace_ts <- NHrace_ts %>%
  mutate(race =  factor(race, levels = x)) %>%
  arrange(statename,date,race)


NHtotal <- NHrace_ts %>% 
  group_by(statename) %>% 
  filter(row_number()==1) %>% select(statename,population)


NHfinal <- merge(NHrace_ts,NHtotal,by.x="statename",by.y="statename")
NHfinal <- dplyr::rename(NHfinal,racePopulation=population.x,populationTot=population.y)

NHracetimeseries <- NHfinal %>% 
  mutate(
    percentPop = round((racePopulation/populationTot)*100),
    Show = ifelse(percentPop>1 & deaths>=30,"Yes","No"),
    dataType = ifelse(race == "Hispanic", "Ethnicity",ifelse(race == "Ethnicity Missing" | race == "Race Missing","", "Race & Ethnicity")),
    
    percentRaceEthnicityCases =ifelse(race=="Race & Ethnicity Missing" & dataType=="Race & Ethnicity",round(100-((cases/totalCases)*100)), ifelse(!race=="All Races Combined" & dataType=="Race & Ethnicity",round((cases/totalRaceCases)*100),NA)),
    percentEthnicityCases = ifelse(!race=="All Races Combined" & dataType=="Ethnicity",round((cases/totalEthnicityCases)*100),NA),
    percentRaceEthnicityDeaths = ifelse(race=="Race & Ethnicity Missing"& dataType=="Race & Ethnicity",round(100-((deaths/totalDeaths)*100)),ifelse(!race=="All Races Combined" & dataType=="Race & Ethnicity",round((deaths/totalRaceDeaths)*100),NA)),
    percentEthnicityDeaths = ifelse(!race=="All Races Combined" & dataType=="Ethnicity",round((deaths/totalEthnicityDeaths)*100),NA)
    
  ) %>% select(date,nation,county,state,statename,race,everything()) %>% arrange(statename,desc(date))


# keep the global environment clean 
rm(NHrace_ts,NHtotal,NHPop,NHdata,NHfinal,x)



NHstatic_race <- NHracetimeseries %>% filter(date==max(date))


#############  MERGING HISPANIC AND NON HISPANIC RACE DATA ######################


static_race <- full_join(Hstatic_race,NHstatic_race) %>%
  arrange(state) %>%
  mutate(caserateRace = ifelse(dataType == "Race", caserate, NA),
         caserateEthnicity = ifelse(dataType == "Ethnicity", caserate, NA),
         caserateRaceEthnicity = ifelse(dataType == "Race & Ethnicity", caserate, NA),
         deathrateRace = ifelse(dataType == "Race", deathrate, NA),
         deathrateEthnicity = ifelse(dataType == "Ethnicity", deathrate, NA),
         deathrateRaceEthnicity = ifelse(dataType == "Race & Ethnicity" , deathrate, NA),
         racePop = ifelse(dataType == "Race",racePopulation,NA),
         ethnicityPop = ifelse(dataType == "Ethnicity",racePopulation,NA),
         raceEthnicityPop = ifelse(dataType == "Race & Ethnicity",racePopulation,NA),
         percentCases = ifelse(dataType=="Race",percentRaceCases, ifelse(dataType== "Race & Ethnicity",percentRaceEthnicityCases,percentEthnicityCases)),
         percentDeaths = ifelse(dataType=="Race",percentRaceDeaths, ifelse(dataType== "Race & Ethnicity",percentRaceEthnicityDeaths,percentEthnicityDeaths)),
         racecategory = ifelse(racecategory=="hispanic","race alone, unknown ethnicity","race / ethnicity"),
         Show = ifelse(race=="Other Race Alone" |race=="All Races Combined" | race == "Two or more races Alone" |race=="Non hispanic Two or more races" | race=="Non hispanic Other Race","No",ifelse(race=="Race Missing"|race=="Ethnicity Missing"|race=="Race & Ethnicity Missing","0000",Show)),
         casesPopProportion = (percentCases-percentPop/((percentCases+percentPop)/2))*1,
         disparity = ifelse(casesPopProportion >= 33, "Yes", "No"),
         CaserateRatio=ifelse(racecategory=="race alone, unknown ethnicity" & race=="Hispanic" | race=="Non Hispanic",NA,CaserateRatio),
         DeathrateRatio=ifelse(racecategory=="race alone, unknown ethnicity" & race=="Hispanic" | race=="Non Hispanic",NA,DeathrateRatio)) %>%
  # select(date,nation,county,state,racecategory,statename,race,racePop,ethnicityPop,raceEthnicityPop,cases,percentCases,deaths,caserateRace,caserateEthnicity,caserateRaceEthnicity,deathrateRace,deathrateEthnicity,deathrateRaceEthnicity,populationTot,percentPop,propRate,Show,dataType) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  dplyr::rename(stateReports=racecategory) 

# %>%
#   filter(!Show=="No")


table(static_race_copy$race)

static_race2 <- full_join(Hstatic_race,NHstatic_race) %>%
  arrange(state) %>%
  mutate(caserateRace = ifelse(dataType == "Race", caserate, NA),
         caserateEthnicity = ifelse(dataType == "Ethnicity", caserate, NA),
         caserateRaceEthnicity = ifelse(dataType == "Race & Ethnicity", caserate, NA),
         deathrateRace = ifelse(dataType == "Race"& percentPop>1 & deaths>=30, deathrate, NA),
         deathrateEthnicity = ifelse(dataType == "Ethnicity"& percentPop>1 & deaths>=30, deathrate, NA),
         deathrateRaceEthnicity = ifelse(dataType == "Race & Ethnicity" & percentPop>1 & deaths>=30, deathrate, NA),
         racePop = ifelse(dataType == "Race",racePopulation,NA),
         ethnicityPop = ifelse(dataType == "Ethnicity",racePopulation,NA),
         raceEthnicityPop = ifelse(dataType == "Race & Ethnicity",racePopulation,NA),
         percentCases = ifelse(dataType=="Race",percentRaceCases, ifelse(dataType== "Race & Ethnicity",percentRaceEthnicityCases,percentEthnicityCases)),
         percentDeaths = ifelse(dataType=="Race",percentRaceDeaths, ifelse(dataType== "Race & Ethnicity",percentRaceEthnicityDeaths,percentEthnicityDeaths)),
         racecategory = ifelse(racecategory=="hispanic","race alone, unknown ethnicity","race / ethnicity"),
         Show = ifelse(race=="Other Race Alone" |race=="All Races Combined" | race == "Two or more races Alone" |race=="Non hispanic Two or more races" | race=="Non hispanic Other Race","No",ifelse(race=="Race Missing"|race=="Ethnicity Missing"|race=="Race & Ethnicity Missing","0000",Show)),
         casesPopProportion = (percentCases-percentPop/((percentCases+percentPop)/2))*1,
         disparity = ifelse(casesPopProportion >= 33, "Yes", "No"),
         CaserateRatio=ifelse(racecategory=="race alone, unknown ethnicity" & race=="Hispanic" | race=="Non Hispanic",NA,CaserateRatio),
         DeathrateRatio=ifelse(racecategory=="race alone, unknown ethnicity" & race=="Hispanic" | race=="Non Hispanic",NA,DeathrateRatio)) %>%
  # select(date,nation,county,state,racecategory,statename,race,racePop,ethnicityPop,raceEthnicityPop,cases,percentCases,deaths,caserateRace,caserateEthnicity,caserateRaceEthnicity,deathrateRace,deathrateEthnicity,deathrateRaceEthnicity,populationTot,percentPop,propRate,Show,dataType) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  dplyr::rename(stateReports=racecategory) %>%
  filter(!Show=="No",!Show=="0000")


table(static_race$race)






racetimeseries <- full_join(Hracetimeseries,NHracetimeseries) %>%
  arrange(desc(date),state) %>%
  mutate(caserateRace = ifelse(dataType == "Race", caserate, NA),
         caserateEthnicity = ifelse(dataType == "Ethnicity", caserate, NA),
         caserateRaceEthnicity = ifelse(dataType == "Race & Ethnicity", caserate, NA),
         deathrateRace = ifelse(dataType == "Race", deathrate, NA),
         deathrateEthnicity = ifelse(dataType == "Ethnicity", deathrate, NA),
         deathrateRaceEthnicity = ifelse(dataType == "Race & Ethnicity", deathrate, NA),
         racePop = ifelse(dataType == "Race",racePopulation,NA),
         ethnicityPop = ifelse(dataType == "Ethnicity",racePopulation,NA),
         raceEthnicityPop = ifelse(dataType == "Race & Ethnicity",racePopulation,NA),
         percentCases = ifelse(dataType=="Race",percentRaceCases, ifelse(dataType== "Race & Ethnicity",percentRaceEthnicityCases,percentEthnicityCases)),
         percentDeaths = ifelse(dataType=="Race",percentRaceDeaths, ifelse(dataType== "Race & Ethnicity",percentRaceEthnicityDeaths,percentEthnicityDeaths)),
         racecategory = ifelse(racecategory=="hispanic","race alone, unknown ethnicity","race / ethnicity"),
         Show = ifelse(race=="Other Race Alone" |race=="All Races Combined" | race == "Two or more races Alone" |race=="Non hispanic Two or more races" | race=="Non hispanic Other Race","No",ifelse(race=="Race Missing"|race=="Ethnicity Missing"|race=="Race & Ethnicity Missing","0000",Show)),
         casesPopProportion = (percentCases-percentPop/((percentCases+percentPop)/2))*1,
         disparity = ifelse(casesPopProportion >= 33, "Yes", "No"),
         CaserateRatio=ifelse(racecategory=="race alone, unknown ethnicity" & race=="Hispanic" | race=="Non Hispanic",NA,CaserateRatio),
         DeathrateRatio=ifelse(racecategory=="race alone, unknown ethnicity" & race=="Hispanic" | race=="Non Hispanic",NA,DeathrateRatio)) %>%
  # select(date,nation,county,state,statename,racecategory,race,racePop,ethnicityPop,raceEthnicityPop,cases,deaths,caserateRace,caserateEthnicity,caserateRaceEthnicity,deathrateRace,deathrateEthnicity,deathrateRaceEthnicity,populationTot,percentPop,Show,dataType) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  dplyr::rename(stateReports=racecategory) %>%
  filter(!Show=="No",!Show=="0000")


# static_race <- racetimeseries %>% filter(date==max(date))

dd <- static_race %>% select(statename,stateReports,race,CaserateRatio,DeathrateRatio)

# keep the global environment clean
rm(race, NHstatic_race, NHracetimeseries, Hracetimeseries, Hstatic_race)

datatype <- static_race %>% filter(!dataType=="Ethnicity") %>% select(statename,race,cases,percentPop,Show)
table(datatype$statename,datatype$Show,useNA="always")



########## ***UNLOCK TO EXPORT*** ##############

setwd(box)
# write.csv(racetimeseries,"./RaceData/racetimeseries.csv", na="", row.names=FALSE)
write.csv(static_race,"./RaceData/staticracedata.csv",na="",row.names=FALSE)
# write.csv(static_race_copy,"./RaceData/copy_staticracedata.csv", na="", row.names=FALSE)



setwd(box2)
write.csv(racetimeseries,"./RaceData/racetimeseries.csv", na="", row.names=FALSE)
write.csv(static_race,"./RaceData/staticracedata.csv",na="",row.names=FALSE)
# write.csv(static_race_copy,"./RaceData/copy_staticracedata.csv", na="", row.names=FALSE)


# variables <- names(static_race)
# racevariables = as.data.frame(variables)
# setwd(box2)
# write.csv(racevariables,"./RaceData/racedatadescription.csv",na="",row.names=FALSE)

