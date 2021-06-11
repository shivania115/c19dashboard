

raw_files <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports"))
file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
raw_files <- raw_files[!is.na(file_dates)]
latest_file <- raw_files[which.max(file_dates)]
folder_name = paste0("/",as.Date(max(file_dates),origin = "1970-01-01"),"/")

counties_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"counties_df_clean.RDS"))

counties_cpr_testing <- counties_df_clean %>%
  dplyr::select(state,county,date_of_file,V01,V02,V33,V34,V35,V56,V57,V66) %>% 
  mutate_at(vars(V33,V66),function(x) x*100) %>% 
  rename(date = date_of_file,
         countyname = V01,
         countycode = V02,
         # CHECKLine 169 of Jithin_code.R
         percentPositiveDailyV3 = V33, # Incorrectly called percentPositive since that was for cumsum proportion in aux02_states cpr testing 
         # CHECK Line 170 of Jithin_code.R ---------
         testsDaily = V34, # Incorrectly called totaltests
         # CHECK Line 174 of Jithin_code.R ---------
         testsPer100K = V35, # Incorrectly called positivePer100K
         hospDaily = V56,
         hospAdmissionper100beds = V57,
         percent7dayhospDaily = V66
         ) %>% 
  mutate(statename = cdlTools::fips(state,to="Name")) %>% 
  dplyr::filter(date >= "2021-03-08") %>% 
  
  arrange(countyname,date) %>% 
  # CHECK Line 181 imputes everything (even state, county) -------
  mutate_at(vars(-one_of("date","countyname","statename","state","county")), function(x) case_when(is.na(x) ~ -1,
                                                                                                   TRUE ~ x)) %>% 
  
  # CHECK: Line 231-234 -------
  # https://stackoverflow.com/questions/46130246/filling-missing-dates-in-a-grouped-time-series-a-tidyverse-way

  ungroup() %>% 
  tidyr::complete(nesting(state,county,statename,countyname),date = seq(min(date),max(date),by="day")) %>% 
  group_by(countyname) %>% 
  
  # CHECK: Lines 236-245: Is it correct to lag? -----
  # CHECK: Variable names are possibly incorrect 
  mutate_at(vars(
                 percentPositiveDaily,testsDaily,
                 hospDaily,percent7dayhospDaily,hospAdmissionper100beds,
                 testsPer100K
                 ),function(x) case_when(is.na(x) ~ dplyr::lag(x),
                                         TRUE ~ x))


# SAVE counties_cpr_testing ---------
saveRDS(counties_cpr_testing,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/counties_cpr_testing.RDS"))
write.csv(head(counties_cpr_testing,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/EXAMPLE_counties_cpr_testing.csv"),
          row.names = FALSE)

