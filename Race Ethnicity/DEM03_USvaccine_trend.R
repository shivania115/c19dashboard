
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


#### reading trend data ########

trend <- getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_demographic_trends_data")


#### data cleaning #######

vacctrend <- jsonlite::fromJSON(trend)[[2]] %>% 
  mutate(Demographic_category=sub("Ages", "Age", Demographic_category),
         demogVar=sub("_.*", "", Demographic_category),
         demogCat=ifelse(demogVar=="Age",sub("Age_","", Demographic_category), sub(".*_", "", Demographic_category))) %>%
  mutate(demogCat=ifelse(demogVar=="Age",sub("(_)?yrs", "", demogCat),sub("NH", "", demogCat)),
         demogVar=tolower(demogVar),
         Date=as.Date(Date,origin="1988-01-01"))


vacctrend$demogCat = revalue(vacctrend$demogCat,c("AIAN"="American Native",
                                                  "Black"="African American",
                                                  "NHOPI"="NHPI",
                                                  "Oth"="Other",
                                                  "known"="Known",
                                                  "unknown"="Unknown"))

colnames(vacctrend) <- to_lower_camel_case(colnames(vacctrend))


##### renaming colnames #######

final_trend <- vacctrend %>% select(-demographicCategory) %>%
  dplyr::rename(pctPartiallyVacc=administeredDose1PctAgegroup,pctFullyVacc=seriesCompletePopPctAgegroup,demogPop=census) 

###### exporting dataset ######

write.csv(final_trend,"./Data/Upload/USvaccineDemog_ts.csv",na="",row.names=F)

