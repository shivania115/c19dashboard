library(RCurl)
library(RJSONIO)

vaccination_county_link <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
vaccination_county <- jsonlite::fromJSON(vaccination_county_link)[2][[1]]

write.csv(vaccination_county,paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/CDC_Covid Data Tracker_County Vaccination_",Sys.Date(),".csv"),row.names = FALSE)

f = list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations"))
f = f[regexpr("CDC_Covid Data Tracker_County Vaccination_",f)>0]

vaccination_ts <- map_dfr(f,.f=function(x){read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/",x))})


vaccination_ts_cleaned <- vaccination_ts %>% 
  rename(date = Date,
         fips = FIPS,
         statename = StateName,
         state_abbreviation = StateAbbr,
         countyname = County,
         n_complete_18plus = Series_Complete_18Plus,
         pct_complete_18plus_in_pop = Series_Complete_18PlusPop_Pct,
         n_complete_65plus = Series_Complete_65Plus,
         pct_complete_65plus_in_pop = Series_Complete_65PlusPop_Pct,
         n_complete = Series_Complete_Yes,
         pct_complete_in_pop = Series_Complete_Pop_Pct,
         pct_completeness = Completeness_pct) %>% 
  mutate(date = lubridate::ymd(date),
         state = substr(fips,1,2) %>% as.numeric(),
         county = substr(fips,3,5) %>% as.numeric()) %>% 
  mutate(fips = as.numeric(fips))

saveRDS(vaccination_ts_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/vaccination_ts_cleaned.RDS"))
write.csv(head(vaccination_ts_cleaned,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/EXAMPLE_vaccination_ts_cleaned.csv"),
          row.names = FALSE)
