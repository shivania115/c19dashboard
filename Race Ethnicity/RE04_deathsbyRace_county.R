

# setting up drive folder path

cpath = "/Users/poojanaik"
drivepath = "/OneDrive - Emory University/CovidHealthEquityDashboard"

dir = paste0(cpath,drivepath)
setwd(dir)

# Package names
packages <- c("base", "tidyverse", "tidyr", "plyr", "textreadr", "dplyr", "data.table", "ggplot2", "lubridate", "tibble", "cdlTools", "openintro", "janitor", "xts", "snakecase", "stringr","RJSONIO","RCurl")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages,installed_packages)


#### reading data from CDC #######

countydeathsrace_raw <- read.csv("https://data.cdc.gov/api/views/k8wy-p9cg/rows.csv?accessType=DOWNLOAD") %>%
  select(-6,-20,-21) 



countydeathsrace_clean<- countydeathsrace_raw %>%
  gather("race","percent",12:18) %>%
  mutate(percent=round(percent*100,1)) %>%
  spread(Indicator,percent) %>%
  mutate(statename=abbr2state(State),
         countyname=paste0(County.Name,", ",State)) %>%
  dplyr::rename(pctAllcauseDeaths="Distribution of all-cause deaths (%)",
                pctCovidDeaths="Distribution of COVID-19 deaths (%)",
                pctPop="Distribution of population (%)",covidDeaths="COVID.19.Deaths",
                totalDeaths="Total.deaths",state="FIPS.State",countycode="FIPS.Code",
                startDate="Start.Date",endDate="End.Date",dataAsOf="Data.as.of") %>%
  select(-State,-County.Name,-FIPS.County) %>%
  mutate(race=gsub("\\."," ",race))
  
countydeathsrace_clean$race <- revalue(countydeathsrace_clean$race,c("Non Hispanic American Indian or Alaska Native"="Non Hispanic American Native",
                               "Non Hispanic Native Hawaiian or Other Pacific Islander"="Non Hispanic NHPI"))



countydeathsrace_final <- countydeathsrace_clean%>% 
  mutate(race=gsub("Non Hispanic ","",race),
         dataAsOf=as.Date(dataAsOf,format="%m/%d/%Y"),
         endDate=as.Date(endDate,format="%m/%d/%Y")) %>%
  select(dataAsOf,endDate,state,countycode,statename,countyname,race,everything(),-startDate) %>%
  arrange(countycode)



