# MERGING HOSPITALIZATION DATA TO COVIDTIMESERIES -------


source(paste0(path_c19dashboard_repo,"/dashboard data/aux02_states cpr testing.R"))
source(paste0(path_c19dashboard_repo,"/dashboard data/aux03_counties cpr testing.R"))

states_cpr_testing <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/states_cpr_testing.RDS"))
counties_cpr_testing <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/counties_cpr_testing.RDS"))

final_hosptest_ts <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/series_hosptest.RDS"))
covidtimeseries00 <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/covidtimeseries00.RDS"))

names(states_cpr_testing)[!names(states_cpr_testing) %in% names(final_hosptest_ts)]
# Checking for missing variable names in final_hosptest_ts

# percentPositiveDaily --> incorrectly labelled as percentPositive in QC dataset
# testsDaily --> incorrectly labelled as totaltests in QC dataset
# testsPer100K --> incorrectly labelled as positivePer100K in QC dataset



final_hosptest_ts4 <- final_hosptest_ts  %>% 
  mutate_at(vars(positive,negative,recovered,hospTot,hospDaily,totaltests),
            function(x) case_when(x == -1 ~ NA_real_,
                                  TRUE ~ x)) %>% 

  # CHECK Line 334: Confirm that merging happens only @ Line 385
  # dplyr::select(-hospTot,-hospDaily,-totaltests) %>% 

  # Check --------
  # mutate(allBeds = "") %>% 
  rename(percent7dayhospDaily = allBeds) %>% 
  arrange(statename,date) %>% 
  group_by(statename) %>% 
  mutate(testsDaily = case_when(is.na(dplyr::lag(totaltests,1)) ~ totaltests,
                                TRUE ~ totaltests - dplyr::lag(totaltests,1)),
         positivetoday = case_when(is.na(dplyr::lag(positive,1)) ~ positive,
                                   TRUE ~ positive - dplyr::lag(positive,1)),
         ) %>% 
  ungroup() %>% 
# Pasted from aux02_states cpr testing.R -------------

 
  bind_rows(states_cpr_testing) %>% 
  # CHECK Imputing so that cumsum doesn't give NA
mutate(testsDaily_imputed = case_when(is.na(testsDaily) ~ 0,
                                      TRUE ~ testsDaily),
       positivetoday_imputed = case_when(is.na(positivetoday) ~ 0,
                                         TRUE ~ positivetoday),
       hospDaily_imputed = case_when(is.na(hospDaily) ~ 0,
                                     TRUE ~ hospDaily)) %>% 
  arrange(statename,date) %>% 
  group_by(statename) %>% 
  # IMPORTANT: Describe ---------
  mutate(# Line 367-369: final_hosptest_ts3<-final_hosptest_ts2 %>%   group_by(statename)%>% mutate(totaltests = cumsum(totaltests))
  totaltests = cumsum(testsDaily_imputed),
  
  # Line 370-372: final_hosptest_ts3<-final_hosptest_ts3 %>% group_by(statename)%>%  mutate(positivetoday = cumsum(positivetoday))
  # Used another variable 'totalpositives'
  totalpositives = cumsum(positivetoday_imputed)
  
  # Hospitalized_Cases__Cumulative_
  # hospTot = cumsum(hospDaily_imputed)
) %>% 
  mutate(hospDaily = case_when(hospDaily == -1 ~ NA_real_,
                               TRUE ~ hospDaily)) %>% 
  mutate(percent7dayhospDaily = case_when(is.na(hospDaily) ~ -999,
                                          is.na(dplyr::lag(hospDaily,7))~ -999,
                                          is.na(dplyr::lag(hospDaily,14)) ~ -999,
                                          TRUE ~ 100*(hospDaily - dplyr::lag(hospDaily,7))/(dplyr::lag(hospDaily,14)))) %>% 
  
  mutate(percentPositive = totalpositives*100/totaltests) %>%
  ungroup() %>% 
  # CHECK What is getting removed here? ---------
  # Line 208: final_hosptest_ts3<-final_hosptest_ts3[,-c(12)]
  arrange(statename,date) %>% 
  group_by(statename) %>% 
  mutate_at(vars(percentPositive,totaltests,totalpositives,hospTot,
                 # CHECK: Lines 220-228: Is it correct to lag? -------
                 # hospDaily, percent7dayhospDaily,hospAdmissionper100beds,
                 # testsPer100K
  ),function(x) case_when(is.na(x) ~ dplyr::lag(x),
                          TRUE ~ x))  %>% 
  ungroup()  %>% 
  tidyr::complete(nesting(statename,state),date = seq(min(date),max(date),by="day")) %>% 
  mutate_at(vars(negative,recovered,percentPositive,hospTot,hospDaily),
            .f=function(x) case_when(is.na(x) ~ -1,
                                     TRUE ~ x))  %>% 
  mutate_at(vars(positive),
            .f=function(x) case_when(is.na(x) ~ 0,
                                     TRUE ~ x))


final_hosptest_ts6 <- final_hosptest_ts4 %>% 
  bind_rows(# CHECK Line 267 - 278: Why not bind it to final_hosptest_t6? -----------
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


















