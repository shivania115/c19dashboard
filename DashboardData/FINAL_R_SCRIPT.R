

###########     SETTING WORKING DIRECTORY   ####################################

#get your working directory
getwd()

# Pooja's box sync folder path
box = "/Users/poojanaik/Box Sync/COVID19_data_shared"

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
if (!require("(openintro")){
  install.packages("(openintro")}


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


#####################        JHU HOSPITALIZATION DATA         ################################

# downloading data from github jhu
download.file(path=local,"https://github.com/CSSEGISandData/COVID-19/archive/master.zip", destfile = "COVID-19-master.zip")
unzip(zipfile = "COVID-19-master.zip")

datadir = "./COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports_us"
myfiles = list.files(path=datadir, pattern="*.csv", full.names=TRUE)
jhu_ht = as_tibble(ldply(myfiles, read_csv))


# renaming variables acc to our naming convention
jhu_ht <- dplyr::rename(jhu_ht,state=FIPS,statename=Province_State,
                        testingRate=Testing_Rate,hospRate=Hospitalization_Rate,
                        tests=People_Tested,hospTot=People_Hospitalized)


# Calling yesterday's date as the latest date for which JHU data is available
maxdate = today()-days(1)


# keeping only required states and variables and doing necessary cleaning
hosptest_ts <- jhu_ht %>% 
  filter(Country_Region=="US",!statename=="American Samoa", 
                 !statename == "Puerto Rico", !statename == "Guam", 
                 !statename == "Recovered", !statename == "Diamond Princess", 
                 !statename == "Grand Princess",!statename == "Virgin Islands",
                 !statename == "Northern Mariana Islands") %>% 
  select(-Country_Region,-Last_Update,-Long_,-Lat,-ISO3,-UID,-Confirmed,-Deaths,-Active,-Recovered,-Incident_Rate,-Mortality_Rate) %>% 
  mutate(nation="",county="", date = rep(seq(as.Date("2020-04-12"), as.Date(maxdate), by = "day"), each=51)) %>%
  select(date,nation,state,county,statename,everything()) %>% 
  arrange(statename) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), -1))


# subsetting static data from time series data
hosptest_static <- hosptest_ts %>% filter(date==max(date))



########       !!! WRITE.CSV UNLOCK INSTRUCTIONS !!!        ############

## For MAC users: Highlight the lines you want to unlock and press Command + Shift + C 
## For Windows users: Highlight the lines you want to unlock and press CTRL + Shift + C 

setwd(box)
# write.csv(hosptest_ts,"./Hospitalizations and testing/series_hosptest.csv",na="",row.names = FALSE)
# write.csv(hosptest_static, "./Hospitalizations and testing/state_level.csv",na="",row.names = FALSE)
# write.csv(hosptest_ts, "/Users/poojanaik/Box Sync/COVID19_data_shared/Yubin/tshosptest.csv",na="",row.names = FALSE)


# Keep the global environment clean
rm(jhu_ht,maxdate,myfiles,datadir)

#################  MERGING HOSPITALIZATION DATA TO COVIDTIMESERIES  ######################

setwd(box)
jhuhosp_ts <- read.csv("./Hospitalizations and testing/series_hosptest.csv")
our_ts <- read.csv("./Yubin/covidtimeseries0.csv")


merged_covidtimeseries <- join(our_ts,jhuhosp_ts)


variables  <- c("tests","hospTot","testingRate", "hospRate")
merged_covidtimeseries[, variables][is.na(merged_covidtimeseries[, variables])] <- -1


# keep the global environment clean 
rm(jhuhosp_ts,our_ts,variables)


# reset directory
setwd(box)


### WRITE.CSV UNLOCK INSTRUCTIONS 
## For MAC users: Highlight the lines you want to unlock and press Command + Shift + C 
## For Windows users: Highlight the lines you want to unlock and press CTRL + Shift + C 

# write.csv(merged_covidtimeseries,"./Yubin/covidtimeseries.csv", na="", row.names=F)


########################      RACE DATA FROM COVID TRACKING PROJECT: FORMAT 1 (reporting Cases)      ###################
 
# library(googlesheets4)
# sp <-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1xF1cdazAejukTt2M6-aAXWG6fivTHFqt8rurF8euXOE/edit#gid=902690690&single=true&output=csv",sheet =3)


race <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR_xmYt4ACPDZCDJcY12kCiMiH0ODyx3E1ZvgOHB8ae1tRcjXbs_yWBOA4j4uoCEADVfC1PS2jYO68B/pub?gid=43720681&single=true&output=csv")

# date as character to data format 
race <- transform(race, Date = as.Date(as.character(Date), "%Y%m%d"))
race <- dplyr::rename(race,Cases_Hispanic=Cases_Ethnicity_Hispanic,Deaths_Hispanic=Deaths_Ethnicity_Hispanic,date=Date)

race <- race %>%
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
  select(-State,-Cases_LatinX,-Cases_Asian,-Cases_AIAN,-Cases_NHPI,-Cases_Multiracial,
                            -Cases_Unknown,-Deaths_LatinX,-Deaths_Asian,-Deaths_AIAN,-Deaths_NHPI,-Deaths_Multiracial,-Deaths_Unknown) 



NEcases <- gather(race,
                  key = "race",
                  value = "cases",
                  c("Cases_Total":"Cases_Hispanic")) %>% 
            mutate(race <- as.character(race)) %>% 
            select(date,nation,state,statename,county,race,cases) %>% 
            arrange(statename)



# renaming levels
NEcases$race <- revalue(NEcases$race, c("Cases_Total"="All Races Combined",
                                        "Cases_White"="White", 
                                        "Cases_Black"="African American",
                                        "Cases_Other"="Other Race",
                                        "Cases_Hispanic"="Hispanic"))                   


NEdeaths <- gather(race,
                   key = "race",
                   value = "deaths",
                   c("Deaths_Total":"Deaths_Hispanic"))%>% 
            select(statename,race,deaths) %>% 
            arrange(statename)


NEdeaths$race <- revalue(NEdeaths$race, c("Deaths_Total"="All Races Combined",
                                          "Deaths_White"="White", 
                                          "Deaths_Black"="African American",
                                          "Deaths_Other"="Other Race",
                                          "Deaths_Hispanic"="Hispanic"))    


NEdata <- merge(NEcases,NEdeaths,by.x=c("statename","race"), by.y=c("statename","race"), sort = TRUE) %>% 
          select(date,nation,state,statename,county,everything())



# keep the global environment clean 
rm(race,NEcases,NEdeaths)




#######################     RACE DATA FROM COVID TRACKING PROJECT: FORMAT 2 (reporting Positives)  ###################

# race2 <- read.csv("./COVID/RaceNew.csv")
race2 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR_xmYt4ACPDZCDJcY12kCiMiH0ODyx3E1ZvgOHB8ae1tRcjXbs_yWBOA4j4uoCEADVfC1PS2jYO68B/pub?gid=43720681&single=true&output=csv")

## initial renaming variables
race2 <- dplyr::rename(race2,
                      date=X,
                      statename=X.1,
                      Positives=c("X.8","X.9", "X.10" ,"X.11","X.12","X.13", "X.14", "X.15","X.16","X.17","X.18","X.19","X.20","X.21"),
                      Deaths=c("X.22", "X.23","X.24","X.25","X.26","X.27","X.28","X.29", "X.30","X.31","X.32","X.33","X.34","X.35"),
                      Negatives=c("X.36","X.37", "X.38", "X.39", "X.40","X.41","X.42","X.43","X.44","X.45","X.46","X.47","X.48"))

# getting rid of the first blank row and unrequired columns
race2 <- race2[-1,-c(3:8,39:71)]

# converting some columns from factor to character for easing merging of rows with the column name
race2[,2:33] <- sapply(race2[,2:33],as.character)

# merging row 1 with column name for cleaning and easy identity
colnames(race2) <- paste(colnames(race2),(race2[1,]), sep = "_")

# checking new variables
names(race2)

# now we don't need this row, kick it out (That's very rude)
race2 <- race2[-1,]


# converting back from character to numeric for data analysis
race2[,3:32] <- sapply(race2[,3:32],as.numeric)

# checking data string
str(race2$date_NA)

# date as character to data format 
race2 <- transform(race2, date_NA = as.Date(as.character(date_NA), "%Y%m%d"))

# confirming string conversion
str(race2$date_NA)

# confirming string of other variables
str(race2)



############## gathering race data POSITIVE CASES only
race_cases <- gather(race2,
                 key = "race",
                 value = "cases",
                 c("Positives_Total":"Positives13_Unknown",-Positives10_..Unk.))

race_cases$race <- as.character(race_cases$race)


library(plyr)

# renaming levels
race_cases$race <- revalue(race_cases$race, c("Positives_Total"="All Races Combined",
                                      "Positives1_Known.White."="White", 
                                      "Positives2_Known.Black."="African American",
                                      "Positives3_Known.LatinX...Hispanic"="LatinHispanic",
                                      "Positives4_Known.Asian"="Asian",
                                      "Positives5_Known.AIAN"="AIAN",
                                      "Positives6_Known.NHPI"="NHPI",
                                      "Positives7_Known.Multiracial."="Multiracial",
                                      "Positives8_Other"="Other Race",
                                      "Positives9_Unknown"="Unknown Race",
                                      "Positives11_Known.Hispanic"="Hispanic",
                                      "Positives12_Known.Non.Hispanic"="Non-Hispanic",
                                      "Positives13_Unknown"="Unknown Ethnicity"))                   

# creating a separate racedata file for further merging
Pos_racedata <- race_cases %>% select(date_NA,statename_,race,cases) %>% arrange(statename_)



################ gathering race and ethnicity data DEATHS only
race_deaths <- gather(race2,
                 key = "race",
                 value = "deaths",
                 c("Deaths_Total":"Deaths13_Unknown",-Deaths10_..Unk.))

race_deaths$race <- as.character(race_deaths$race)


library(plyr)

# renaming levels
race_deaths$race <- revalue(race_deaths$race, c("Deaths_Total"="All Races Combined",
                                      "Deaths1_Known.White."="White", 
                                      "Deaths2_Known.Black."="African American",
                                      "Deaths3_Known.LatinX...Hispanic"="latin_Hispanic",
                                      "Deaths4_Known.Asian"="Asian",
                                      "Deaths5_Known.AIAN"="AIAN",
                                      "Deaths6_Known.NHPI"="NHPI",
                                      "Deaths7_Known.Multiracial."="Multiracial",
                                      "Deaths8_Other"="Other Race",
                                      "Deaths9_Unknown"="Unknown Race",
                                      "Deaths11_Known.Hispanic"="Hispanic",
                                      "Deaths12_Known.Non.Hispanic"="Non-Hispanic",
                                      "Deaths13_Unknown"="Unknown Ethnicity"))                  

# creating a separate racedata file for further merging
Deaths_racedata <- race_deaths %>% select(date_NA,statename_,race,deaths) %>% arrange(statename_)
death <- Deaths_racedata %>% select(deaths)

data <- cbind(Pos_racedata, death) %>% arrange(statename_)

data$county <- ""
data$nation <- ""
data <- dplyr::rename(data,date=date_NA,statename=statename_)
library(cdlTools)
data$state <- fips(data$statename,to="FIPS")

library(openintro)
data$statename <- abbr2state(data$statename)

NEdata <- data %>% filter(!statename=="",race=="All Races Combined"|race=="White"|race=="African American"|race=="Other Race"|race=="Hispanic") %>% select(date,nation,state,statename,county,everything()) %>% arrange(state,date)

# keep the global environment clean 
rm(data,death,Deaths_racedata,Pos_racedata,race_cases,race_deaths,race2)




################      GETTING AND CLEANING POPULATION DATA FOR COMPUTING RATES    ############


setwd(box)
deno <- read.csv("./RaceData/SErace.csv")


NEPop <- gather(deno,
                key="race",
                value="population",
                c("population":"Hispanic"))%>% 
  filter(!race=="nonhispanic",!race=="NHAIAN",!race=="NHAsian") %>% 
  select(-ACSwhite,-ACSBlack,-ACSAsian,-ACSotherace) %>% 
  arrange(acsstate) 

NEPop$race <- revalue(NEPop$race, c("population"="All Races Combined",
                                    "Nhwhite"="White", 
                                    "NHBlack"="African American",
                                    "Nhotherrace"="Other Race"))   

NEPop <- dplyr::rename(NEPop,state=acsfips,statename=acsstate)

# keep the global environment clean 
rm(deno)

###############      MERGING POPULATION DATA WITH RACE DATA      #################

race_ts <- merge(NEdata,NEPop,
                 by.x=c("state","statename","race"),
                 by.y=c("state","statename","race"),sort=TRUE) %>% 
          arrange(statename,date) %>%
           mutate(
               caserate = round((cases/population)*100000,digits=2),
               deathrate = round((deaths/population)*100000,digits=2)
               )


x <- c("All Races Combined","African American","White","Other Race","Hispanic")

race_ts <- race_ts %>%
  mutate(race =  factor(race, levels = x)) %>%
  arrange(statename,date,race)


total <- race_ts %>% 
  group_by(statename) %>% 
  filter(row_number()==1) %>% select(statename,population)


final3 <- merge(race_ts,total,by.x="statename",by.y="statename")
final3 <- dplyr::rename(final3,racePopulation=population.x,populationTot=population.y)

static_race <- final3 %>% 
  mutate(
    percentPop = round((racePopulation/populationTot)*100,digits=2),
    Show = ifelse(percentPop>10,"Yes","No")
    #percentCases = round((cases/populationTot)*100,digits=2),
    #percentDeaths = round((deaths/populationTot)*100,digits=2)
  ) %>% select(date,nation,county,state,statename,race,everything())


# keep the global environment clean 
rm(race_ts,total,NEPop,NEdata,final3,x)


# write.csv(static_race,"./RaceData/staticracedata.csv")



##################    MERGING STATIC RACE DATA WITH PREVIOUS RACETIMESERIES DATA   ################

raceprev_ts <- read.csv("./RaceData/racetimeseries0.csv") %>% mutate(
  date = as.Date(date, origin = "1982-01-01"),
  county = as.character(county),
  nation = as.character(nation)
)

racetimeseries <- full_join(raceprev_ts,static_race) %>% arrange(statename,date)


racevar <- c("cases","deaths","caserate","deathrate")
racetimeseries[, racevar][is.na(racetimeseries[, racevar])] <- -1

# keep the global environment clean 
rm(racevar,raceprev_ts)

# UNLOCK THIS CODE TO EXPORT BY PRESSING 'COMMAND + SHIFT + C' OR 'CTRL + SHIFT + C'
# write.csv(racetimeseries,"./RaceData/racetimeseries.csv", na="")


