# MERGING HOSPITALIZATION DATA TO COVIDTIMESERIES -------


source(paste0(path_c19dashboard_repo,"/DashboardData/aux02_states cpr testing.R"))
source(paste0(path_c19dashboard_repo,"/DashboardData/aux03_counties cpr testing.R"))

states_cpr_testing <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/states_cpr_testing.RDS"))
counties_cpr_testing <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/counties_cpr_testing.RDS"))

final_hosptest_ts <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/series_hosptest.RDS"))
covidtimeseries00 <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/covidtimeseries00.RDS"))

final_hosptest_ts4 <- final_hosptest_ts  %>% 
  # CHECK Line 334: Confirm that merging happens only @ Line 385
  # dplyr::select(-hospTot,-hospDaily,-totaltests) %>% 

  # Check --------
  # mutate(allBeds = "") %>% 
  rename(percent7dayhospDaily = allBeds) %>% 
  arrange(statename,date) %>% 
  mutate(hospDaily = case_when(hospDaily == -1 ~ NA_real_,
                               TRUE ~ hospDaily)) %>% 
  mutate(percent7dayhospDaily = case_when(is.na(hospDaily) ~ -999,
                                          is.na(dplyr::lag(hospDaily,7))~ -999,
                                          is.na(dplyr::lag(hospDaily,14)) ~ -999,
                                          TRUE ~ 100*(hospDaily - dplyr::lag(hospDaily,7))/(dplyr::lag(hospDaily,14)))) %>% 
  mutate(hospDaily = case_when(is.na(hospDaily) ~ -1,
                               TRUE ~ hospDaily)) %>% 
  bind_rows(states_cpr_testing,
            
            # CHECK Line 267 - 278: Why not bind it to final_hosptest_t6? -----------
            # Why did we exclude all pre 2021-03-07 dates?
            # Why read it in if we are not using it?
            counties_cpr_testing) 


merged_covidtimeseries <- covidtimeseries00 %>% 
  # Line 385: Why left_join() on covidtimeseries00
  left_join(final_hosptest_ts4,
            by=c("date","state","nation","county")) %>% 
  mutate_at(vars(totaltests,hospDaily,percentPositive,percent7dayhospDaily),
            .f=function(x) case_when(is.na(x) ~ -1,
                                     TRUE ~ x))

# SAVE merged_covidtimeseries ------------
saveRDS(merged_covidtimeseries,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/merged_covidtimeseries.RDS"))


write.csv(head(merged_covidtimeseries,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/EXAMPLE_merged_covidtimeseries.csv"),
          row.names = FALSE)


















