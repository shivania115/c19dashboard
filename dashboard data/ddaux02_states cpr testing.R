
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
         hospAdmissionper100beds = S53,
         percent7dayhospDaily = S62
  ) %>% 
  dplyr::filter(!state %in% c(60, 66, 69, 72, 78)) %>% 
  dplyr::filter(date >= "2021-03-08") %>% 
  arrange(statename,date) %>%
  # CHECK Line 128: Why merge with states_daterange_clean in the first place? ---------

  # CHECK Creating percentPositive twice: Lines 322-323 && Lines 373 --------
mutate(percentPositiveDaily = percentPositiveDaily*100,
       percent7dayhospDaily = percent7dayhospDaily*100) %>% 
  
  # Skipping ahead to code_Jithin.R >> Lines 365
  mutate(positivetoday = testsDaily*percentPositiveDaily/100) 

# SAVE states_cpr_testing ---------
saveRDS(states_cpr_testing,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/states_cpr_testing.RDS"))
write.csv(head(states_cpr_testing,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/EXAMPLE_merged_states_cpr_testing.csv"),
          row.names = FALSE)