
# Link with dd11_merging with covidtimeseries.R

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
                S21,S22,S23,S52,S53,S62
  ) %>% 
  dplyr::select(-file_name) %>% 
  rename(date = date_of_file,
         statename = S01,
         percentPositiveDaily = S21,
         testsDaily = S22,
         testsPer100K = S23, # Incorrectly labelled as positivePer100K -----
         hospDaily = S52,
         hospAdmission = S53,
         percent7dayhospDaily = S62
  ) %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(date >= "2021-03-08") %>% 
  arrange(statename,date) %>%
  # CHECK Line 128: Why merge with states_daterange_clean in the first place? ---------

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
  # CHECK What is getting removed here? ---------
  # Line 208: final_hosptest_ts3<-final_hosptest_ts3[,-c(12)]
  arrange(statename,date) %>% 
  tidyr::complete(nesting(statename,state),date = seq(min(date),max(date),by="day")) %>% 
  group_by(statename) %>% 
  mutate_at(vars(percentPositive,totaltests,totalpositives
                 # CHECK: Lines 220-228: Is it correct to lag? -------
                 # hospDaily, percent7dayhospDaily,hospAdmissionper100beds,
                 # testsPer100K
                 ),function(x) case_when(is.na(x) ~ dplyr::lag(x),
                                         TRUE ~ x))

# SAVE states_cpr_testing ---------
saveRDS(states_cpr_testing,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/states_cpr_testing.RDS"))
write.csv(head(states_cpr_testing,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/EXAMPLE_merged_states_cpr_testing.csv"),
          row.names = FALSE)