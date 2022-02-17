library(RCurl)
library(RJSONIO)

vaccination_county_link <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
vaccination_county <- jsonlite::fromJSON(vaccination_county_link)[2][[1]]

date <- unique(vaccination_county$Date)

write.csv(vaccination_county,paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/CDC_Covid Data Tracker_County Vaccination_",date,".csv"),row.names = FALSE)

f = list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations"))
f = f[regexpr("CDC_Covid Data Tracker_County Vaccination_",f)>0]

vaccination_ts <- map_dfr(f,.f=function(x){read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/",x)) %>% 
    mutate_at(vars(contains("Series")),~as.numeric(unlist(.)))})

countynames <- read.csv(paste0(path_c19dashboard_shared_folder,"/Data/Upload/nationalraw.csv")) %>% 
  distinct(nation,state,county)

vaccination_ts_cleaned <- vaccination_ts %>% 
  rename(date = Date,
         fips = FIPS,
         statename = StateName,
         state_abbreviation = StateAbbr,
         # countyname = County,
         nComplete18Plus = Series_Complete_18Plus,
         pctComplete18PlusInPop = Series_Complete_18PlusPop_Pct,
         nComplete65Plus = Series_Complete_65Plus,
         pctComplete65PlusInPop = Series_Complete_65PlusPop_Pct,
         nComplete = Series_Complete_Yes,
         pctCompleteInPop = Series_Complete_Pop_Pct,
         pctCompleteness = Completeness_pct) %>% 
  dplyr::select(-County)  %>% 
  mutate(date = lubridate::ymd(date),
         nation = NA,
         state = substr(fips,1,2) %>% as.numeric(),
         county = substr(fips,3,5) %>% as.numeric()) %>% 
  mutate(fips = as.numeric(fips)) %>% 
  dplyr::rename(countycode = fips) %>% 
  left_join(countynames,
            by = c("nation","state","county"))




saveRDS(vaccination_ts_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/vaccination_ts_cleaned.RDS"))
write.csv(vaccination_ts_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/vaccination_ts_cleaned.csv"),row.names=FALSE)
# haven::write_dta(vaccination_ts_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/vaccination_ts_cleaned.dta"),version=12)
write.csv(head(vaccination_ts_cleaned,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/EXAMPLE_vaccination_ts_cleaned.csv"),
          row.names = FALSE)
