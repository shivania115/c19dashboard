
# setting up drive folder path

cpath = "/Users/poojanaik/Applications"
drivepath = "/OneDrive - Emory University/COVID19_data_shared"

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


#############  Race demog popdata ###########

rac <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/us_demographics.json")
racdemog <- as.data.frame(jsonlite::fromJSON(rac)[1]) 
colnames(racdemog) <- sub(".*_percent.", "", colnames(racdemog)) 
racdemog2 <- racdemog %>% 
  select(race_eth_new,col_per_Grand_Total) %>% 
  dplyr::rename(percentPop=col_per_Grand_Total)


############ Extracting Date variable from webpage: (FAILED) #############

# usdemogdate <- read_html("https://covid.cdc.gov/covid-data-tracker/#demographics") %>%
#   html_node("demographics-top-note") %>%
#   html_attr("viz030_note") %>%
#   str_extract("[A-Z][a-z]+ [0-9]+, [0-9]{4}") %>%
#   as.Date(format = '%B %d, %Y')


##### DOWNLOAD DATA : CASES BY RACE #########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")


casesdata <- jsonlite::fromJSON(raw_data)[[6]] %>% join(racdemog2)  #for race by age data
casesdata$race_eth_new <- revalue(casesdata$race_eth_new, c("White, Non-Hispanic"="Non-Hispanic White", 
                                                            "Black, Non-Hispanic"="Non-Hispanic African American",
                                                            "American Indian / Alaska Native, Non-Hispanic" = "Non-Hispanic American Native",
                                                            "Asian, Non-Hispanic" = "Non-Hispanic Asian",
                                                            "Native Hawaiian / Other Pacific Islander, Non-Hispanic" = "Non-Hispanic NHPI",
                                                            "Multiple/Other, Non-Hispanic"="Non-Hispanic Multiple/Other",
                                                            "Unknown" = "Unknown",
                                                            "Hispanic/Latino" = "Hispanic"))

names(casesdata)

casesdata_race <- casesdata %>% 
  select(Demographic,race_eth_new,Grand_Total,col_per_Grand_Total,percentPop) %>%
  dplyr::rename(demographicVar=Demographic,demographic=race_eth_new,cases=Grand_Total,percentCases=col_per_Grand_Total) %>% 
  mutate(totalcases = sum(cases),demographicVar="race",
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA))


rm(casesdata,raw_data)


###################  DEATHS BY RACE DATA ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathdata <- jsonlite::fromJSON(raw_data)[[7]] %>% join(racdemog2)  #for race by age data
deathdata$race_eth_new <- revalue(deathdata$race_eth_new, c("White, Non-Hispanic"="Non-Hispanic White", 
                                                            "Black, Non-Hispanic"="Non-Hispanic African American",
                                                            "American Indian / Alaska Native, Non-Hispanic" = "Non-Hispanic American Native",
                                                            "Asian, Non-Hispanic" = "Non-Hispanic Asian",
                                                            "Native Hawaiian / Other Pacific Islander, Non-Hispanic" = "Non-Hispanic NHPI",
                                                            "Multiple/Other, Non-Hispanic"="Non-Hispanic Multiple/Other",
                                                            "Hispanic/Latino" = "Hispanic"))
names(deathdata)

deathdata_race <- deathdata %>% 
  select(Demographic,race_eth_new,Grand_Total,col_per_Grand_Total,percentPop) %>%
  dplyr::rename(demographicVar=Demographic,demographic=race_eth_new,deaths=Grand_Total,percentDeaths=col_per_Grand_Total) %>% 
  mutate(totaldeaths = sum(deaths),demographicVar="race",
         missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA))


rm(deathdata,raw_data)



############## JOIN CASES AND DEATHS DATA #############

racecdc <- full_join(casesdata_race,deathdata_race) %>% 
  mutate(demogLabel = gsub("Non-Hispanic ","",demographic),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(Sys.Date(),origin="1988-01-01")) %>% select(date,everything())


rm(casesdata_race,deathdata_race)


############  CASERATE AND DEATHRATE  #############

raceNation <- racecdc %>% 
  mutate(caserate=round(100000*(cases/demogPop),2),
         deathrate=round(100000*(deaths/demogPop),2)) 


whiteCR <- raceNation %>% 
  filter(demographic=="Non-Hispanic White") %>% 
  select(date,demographic,caserate) %>% 
  dplyr::rename(WhiteCR=caserate) %>% 
  select(-demographic)

crr = join(raceNation,whiteCR) %>% mutate(CaserateRatio = round(caserate/WhiteCR,2))

HwhiteDR <- raceNation %>% filter(demographic=="Non-Hispanic White") %>% select(date,demographic,deathrate) %>% dplyr::rename(WhiteDR=deathrate) %>% select(-demographic)

drr = join(raceNation,HwhiteDR) %>% mutate(DeathrateRatio = round(deathrate/WhiteDR,2))

raceUS_final <- join(crr,drr) %>%
  select(-WhiteCR,-WhiteDR) %>% mutate(demographicVar="race")


rm(crr,drr,HwhiteDR,raceNation,whiteCR)


#############  age demog data ############

age <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/us_demographics.json")
agedemog <- as.data.frame(jsonlite::fromJSON(age)[3])
colnames(agedemog) <- sub(".*_percent.", "", colnames(agedemog))
agedemog2 <- agedemog %>% select(age_group,Percent) %>% 
  dplyr::rename(percentPop=Percent)


#########  DOWNLOAD DATA : CASES BY AGE  ###########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

casesdata <- jsonlite::fromJSON(raw_data)[[2]] %>% join(agedemog2) %>% 
  select(-b) %>% 
  dplyr::rename(cases=count,demographic=age_group,percentCases=Percent) %>%
  mutate(totalcases = sum(cases),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(Sys.Date(),origin="1988-01-01"),
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),
         availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA)) %>% select(date,everything()) 




#########  DOWNLOAD DATA : DEATHS BY AGE  ###########

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathsdata <- jsonlite::fromJSON(raw_data)[[3]] %>% join(agedemog2) %>% 
  select(-b) %>% 
  dplyr::rename(deaths=count,demographic=age_group,percentDeaths=Percent) %>%
  mutate(totaldeaths = sum(deaths),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(Sys.Date(),origin="1988-01-01"),
         missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),
         availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA)) %>% select(date,everything()) 



########## CASES AND DEATHS BY AGE: merging based on age groups ###############

covid_age <- join(casesdata,deathsdata) %>% mutate(demogLabel=demographic,date=as.Date(Sys.Date(),origin="1988-01-01"))  %>%
  mutate(demographicVar="age")

covid_age$demographic <- recode(covid_age$demographic,"Unknown"="Unknown Age")

########  remove unnecessary dataframes ########

rm(casesdata,deathsdata)


#############  sex demog data ############

sex <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/us_demographics.json")
sexdemog <- as.data.frame(jsonlite::fromJSON(sex)[2])
colnames(sexdemog) <- sub(".*_percent.", "", colnames(sexdemog))
sexdemog2 <- sexdemog %>% select(sex_new,col_per_Grand_Total) %>%
  mutate(percentPop=as.numeric(col_per_Grand_Total)) %>%select(-col_per_Grand_Total)


########## CASES  BY SEX: merging based on sex ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

casesdata_sex <- jsonlite::fromJSON(raw_data)[[4]] %>% join(sexdemog2) %>%
  filter(!sex_new=="Grand_Total") %>%
  select(sex_new,Grand_Total,col_per_Grand_Total,percentPop) %>% 
  dplyr::rename(cases=Grand_Total,demographic=sex_new,percentCases=col_per_Grand_Total) %>%
  mutate(totalcases = sum(cases),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(Sys.Date(),origin="1988-01-01"),
         missingCases = ifelse(demographic=="Unknown",ceiling(cases/totalcases*100), NA),
         availableCases = ifelse(demographic=="Unknown",floor((1-(cases/totalcases))*100), NA)) %>% select(date,everything()) 


########## DEATHS  BY SEX: merging based on sex ###############

raw_data <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")

deathsdata_sex <- jsonlite::fromJSON(raw_data)[[5]] %>% join(sexdemog2) %>%
  filter(!sex_new=="Grand_Total") %>%
  select(sex_new,Grand_Total,col_per_Grand_Total,percentPop) %>% 
  dplyr::rename(deaths=Grand_Total,demographic=sex_new,percentDeaths=col_per_Grand_Total) %>%
  mutate(totaldeaths = sum(deaths),
         popUS=328239523, #327167439
         demogPop=(round((percentPop/100)*popUS)),
         date=as.Date(Sys.Date(),origin="1988-01-01")) %>% select(date,everything()) 

#deaths count missing, adding using following code

deathsTotSex <- as.numeric(jsonlite::fromJSON(raw_data)[[5]] %>% filter(sex_new=="Grand_Total") %>% 
                             select(Grand_Total))

deathssex <- data.frame(adorn_totals(deathsdata_sex, fill = "",name="total", na.rm = TRUE)) %>%
  mutate(totaldeaths=ifelse(demographic=="total",totaldeaths/4,totaldeaths))
deathssex$deaths[deathssex$demographic=="total"] <- deathsTotSex
deathsdata_sex$totaldeaths <- deathsTotSex


deathsdata_sex <- deathsdata_sex %>% 
  mutate(missingDeaths = ifelse(demographic=="Unknown",ceiling(deaths/totaldeaths*100), NA),
         availableDeaths = ifelse(demographic=="Unknown",floor((1-(deaths/totaldeaths))*100), NA))


########## CASES AND DEATHS BY SEX: merging based on sex groups ###############

covid_sex <- join(casesdata_sex,deathsdata_sex) %>% 
  mutate(demogLabel=demographic,date=as.Date(Sys.Date(),origin="1988-01-01"))  %>%
  mutate(demographicVar="sex")

covid_sex$demographic <- recode(covid_sex$demographic,"Unknown"="Unknown Sex")


########  remove unnecessary dataframes ########

rm(casesdata_sex,deathsdata_sex)


final_data <- full_join(raceUS_final,covid_age) %>% full_join(covid_sex) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  mutate(demogLabel=ifelse(demographicVar=="age",gsub(" Years","",demogLabel),demogLabel)) %>%
  select(date,demographicVar,demographic,demogLabel,popUS,demogPop,percentPop,cases,totalcases,percentCases,availableCases,missingCases,caserate,deaths,totaldeaths,percentDeaths,availableDeaths,missingDeaths,deathrate) 


final_data$percentCases[final_data$demogLabel=="Unknown"] <- -9999
final_data$percentDeaths[final_data$demogLabel=="Unknown"] <- -9999
final_data$popUS[final_data$demogLabel=="Unknown"] <- -9999



##############  VACCINATION BY RACE DATA ########################


usdemog_link <- getURL("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/vaccination_demographics.json")
usdemog <- as.data.frame(jsonlite::fromJSON(usdemog_link)[1]) 
colnames(usdemog) <- sub(".*_percents.", "", colnames(usdemog)) 
usdemog2 <- usdemog %>% 
  select(-id,-Date) %>% 
  dplyr::rename(percentPop=US_Total) 



##############  VACCINATION BY RACE DATA ########################

vacclink <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_demographics_data")
vaccdata <- jsonlite::fromJSON(vacclink)[[2]]

df <- vaccdata %>% join(usdemog2) %>% select(-census) %>%
  dplyr::rename(date=Date,demogLabel=Demographic_category,admDose1=Administered_Dose1,pctAdmDose1=Administered_Dose1_pct_known,
                pctKnownAdmDose1=Administered_Dose1_pct_US,admDose2=Administered_Dose2,pctAdmDose2=Administered_Dose2_pct_known,
                pctKnownAdmDose2=Administered_Dose2_pct_US) %>%
  mutate(date=as.Date(date,origin="1988-01-01"),
         demographicVar = ifelse(str_detect(demogLabel, "Age"),"vaccineAge",ifelse(str_detect(demogLabel, "Race"),"vaccineRace",ifelse(str_detect(demogLabel, "Sex"),"vaccineSex","total"))),
         demogLabel = ifelse(demographicVar=="vaccineAge",gsub("Ages_","",demogLabel),
                             ifelse(demographicVar=="vaccineRace",gsub("Race_eth_","",demogLabel),
                                    ifelse(demographicVar=="vaccineSex",gsub("Sex_","",demogLabel),demogLabel))))

colnames(df) <- to_lower_camel_case(colnames(df))

df$admDose1[df$demogLabel=="US"] -> USTotDose1
df$admDose2[df$demogLabel=="US"] -> USTotDose2


df2 <- df %>% mutate(demogLabel = ifelse(demographicVar=="vaccineAge",gsub("_yrs","",demogLabel),
                                         ifelse(demographicVar=="vaccineRace",gsub("NH","",demogLabel),demogLabel)))


df2$demogLabel <- revalue(df2$demogLabel,c("<18yrs"="<18",
                                           "Age_known" = "Unknown",
                                           "Age_unknown" = "unknown",
                                           "known"="Unknown",
                                           "OPI" = "NHPI",
                                           "AIAN"="American Native",
                                           "Black"="African American",
                                           "Mult_Oth"="Multiple/Other"))

names(finalvacc)
finalvacc <- df2 %>% filter(!demogLabel=="US",!demogLabel=="unknown") %>%
  mutate(pctKnownAdmDose1 = ifelse(!demogLabel=="Unknown",-9999,pctKnownAdmDose1),
         pctKnownAdmDose2 = ifelse(!demogLabel=="Unknown",-9999,pctKnownAdmDose2),
         pctAdmDose1 = ifelse(demogLabel=="Unknown",-9999,pctAdmDose1),
         pctAdmDose2 = ifelse(demogLabel=="Unknown",-9999,pctAdmDose2),
         seriesCompletePopPctKnown = ifelse(demogLabel=="Unknown",-9999,seriesCompletePopPctKnown),
         seriesCompletePopPctUs = ifelse(!demogLabel=="Unknown",-9999,seriesCompletePopPctUs),
         administeredDose1PctAgegroup = ifelse(demogLabel=="Unknown"|!demographicVar=="vaccineAge",-9999,administeredDose1PctAgegroup),
         seriesCompletePopPctAgegroup = ifelse(demogLabel=="Unknown"|!demographicVar=="vaccineAge",-9999,seriesCompletePopPctAgegroup)) %>%
  mutate(demographic = ifelse(demographicVar=="vaccineAge" & !demogLabel=="Unknown",paste(demogLabel,"years"),
                              ifelse(demographicVar=="vaccineRace" & !demogLabel=="Unknown" & !demogLabel=="Hispanic",paste("Non-Hispanic",demogLabel),demogLabel)),
         percentPop = ifelse(is.na(percentPop),-9999,percentPop),
         demographic = ifelse(demographicVar=="age",paste(demogLabel,"years"),demographic)) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  arrange(demographicVar) 


finalvacc$popUS <- 327167439

mergedVacc23 <- na.locf(finalvacc, fromFirst = TRUE)

mergedVacc23 <- mergedVacc23 %>% 
  mutate(demogLabel=ifelse(demographicVar=="age",gsub("-"," - ",demogLabel),demogLabel)) %>%
  arrange(demographicVar)   

mergedVacc23$demographic[mergedVacc23$demographicVar=="age" & mergedVacc23$demographic=="Unknown years"] <- "Unknown"


vaccupload <- full_join(mergedVacc23,final_data) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), -9999)) %>%
  arrange(demographicVar,demographic)


write.csv(vaccupload,"/Users/poojanaik/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/USDemogData.csv",na="",row.names=F)

