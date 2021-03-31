# MERGING HOSPITALIZATION DATA TO COVIDTIMESERIES -------

raw_files <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports"))
file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
raw_files <- raw_files[!is.na(file_dates)]
latest_file <- raw_files[which.max(file_dates)]
folder_name = paste0("/",as.Date(max(file_dates),origin = "1970-01-01"),"/")


states_daterange_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"states_date_range_clean.RDS"))
states_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"states_df_clean.RDS"))


states_cpr_testing <- states_df_clean %>% 
  dplyr::select(file_name,date_of_file,
                S01,state,
                S21,S22,S52,S62
  ) %>% 
  left_join(states_daterange_clean %>% 
              dplyr::filter(header == "VIRAL (RT-PCR) LAB TESTING: LAST WEEK") %>% 
              dplyr::select(file_name,daterange),
            by = "file_name") %>% 
  dplyr::select(-file_name) %>% 
  rename(date = date_of_file,
         statename = S01,
         percentPositiveDaily = S21,
         testsDaily = S22,
         hospDaily = S52,
         percent7dayhospDaily = S62
         ) %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(date >= "2021-03-08") %>% 
  arrange(statename,date) %>%
  # Why merge with states_daterange_clean in the first place? ---------
  dplyr::select(-daterange) %>% 
  
  # CHECK Creating percentPositive twice: Lines 322-323 && Lines 373 --------
  mutate(percentPositiveDaily = percentPositiveDaily*100,
         percent7dayhospDaily = percent7dayhospDaily*100) %>% 
  
  # Skipping ahead to code_Jithin.R >> Lines 365
  mutate(positivetoday = testsDaily*percentPositiveDaily/100) %>% 
  
  # CHECK Imputing so that cumsum doesn't give NA ---------
  mutate(testsDaily_imputed = case_when(is.na(testsDaily) ~ 0,
                                    TRUE ~ testsDaily),
         positivetoday_imputed = case_when(is.na(positivetoday) ~ 0,
                                           TRUE ~ positivetoday),
         hospDaily_imputed = case_when(is.na(hospDaily) ~ 0,
                                       TRUE ~ hospDaily)) %>% 
  
  # IMPORTANT: Describe ---------
  mutate(# Line 367-369: final_hosptest_ts3<-final_hosptest_ts2 %>%   group_by(statename)%>% mutate(totaltests = cumsum(totaltests))
         totaltests = cumsum(testsDaily_imputed),
         
         # Line 370-372: final_hosptest_ts3<-final_hosptest_ts3 %>% group_by(statename)%>%  mutate(positivetoday = cumsum(positivetoday))
         # Used another variable 'totalpositives'
         totalpositives = cumsum(positivetoday_imputed),
         
         # Hospitalized_Cases__Cumulative_
         hospTot = cumsum(hospDaily_imputed)
  ) %>% 
  mutate(percentPositive = totalpositives*100/totaltests) %>%
  
  # Line 375: final_hosptest_ts3<-final_hosptest_ts3[,-c(10)]
  # CHECK What is getting removed here? ---------
  
  # renaming positivetoday --> positive
  rename(positive = positivetoday)

# SAVE states_cpr_testing ---------
saveRDS(states_cpr_testing,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/states_cpr_testing.RDS"))
write.csv(head(states_cpr_testing,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/EXAMPLE_merged_states_cpr_testing.csv"),
          row.names = FALSE)

# 
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
  bind_rows(states_cpr_testing) 


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


















