library(plyr)
library(readr)
library(dplyr)
library(tidyr)

# importing and rbinding all the daily csv files 
mydir = "/Users/poojanaik/Downloads/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports_us"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
rec = ldply(myfiles, read_csv)

rec <- as_tibble(rec)

# keeping only required states and variables
rec <- rec %>% filter(Country_Region=="US",!Province_State=="American Samoa", !Province_State == "Puerto Rico", !Province_State == "Guam", !Province_State == "Recovered", !Province_State == "Diamond Princess", !Province_State == "Grand Princess",!Province_State == "Virgin Islands",!Province_State == "Northern Mariana Islands") %>% select(-Country_Region,-Last_Update,-Long_,-Lat,-ISO3,-UID,-Confirmed,-Deaths,-Active,-Recovered,-Incident_Rate,-Mortality_Rate)

# now we have daily data for 51 states for 46 days starting from April 12,2020 to May 27,2020
# adding date variable to each row using repeating each date 51 times i.e. for our 51 states
rec$date <- rep(seq(as.Date("2020/04/12"), as.Date("2020/06/28"), by = "day"), each=51)


# adding nation and county variable
rec$nation <- ""
rec$county <- ""

# renaming variables acc to our naming convention
rec <- dplyr::rename(rec,state=FIPS,statename=Province_State,testingRate=Testing_Rate,hospitalizationRate=Hospitalization_Rate,tests=People_Tested,hospitalizations=People_Hospitalized)

# ordering the columns 
rec <- dplyr::select(rec,date,nation,state,county,statename,everything())

rec <- rec %>% arrange(statename) %>% mutate_if(is.numeric, ~replace(., is.na(.), -1))
recstatic <- rec %>% filter(date==max(date)) %>% mutate_if(is.numeric, ~replace(., is.na(.), -1))


rec <- dplyr::rename(rec,hospTot=hospitalizations,hospRate=hospitalizationRate)
recstatic <- dplyr::rename(recstatic,hospTot=hospitalizations,hospRate=hospitalizationRate)

## MAC: Highlight the lines you want to unlock and press Command + Shift + C 
## Windows: Highlight the lines you want to unlock and press CTRL + Shift + C 

# write.csv(rec, "/Users/poojanaik/Box Sync/COVID19_data_shared/Hospitalizations and testing/series_hosptest.csv",row.names = FALSE)
# write.csv(recstatic, "/Users/poojanaik/Box Sync/COVID19_data_shared/Hospitalizations and testing/state_level.csv",row.names = FALSE)
# write.csv(rec, "/Users/poojanaik/Box Sync/COVID19_data_shared/Yubin/tshosptest.csv",row.names = FALSE)

