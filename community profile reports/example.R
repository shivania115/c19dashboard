
# Using CPR data -----------------

states_daterange_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/2021-03-28/states_date_range_clean.RDS"))
states_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/2021-03-28/states_df_clean.RDS"))
counties_daterange_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/2021-03-28/counties_date_range_clean.RDS"))
counties_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/2021-03-28/counties_df_clean.RDS"))


# STATES: Example for indicators which belong to same header : "VIRAL (RT-PCR) LAB TESTING: LAST WEEK"
states_example_df <- states_df_clean %>% 
  dplyr::select(file_name,date_of_file,
                S01,S02,state,county,
                S21,S22,S23,S24,S25,
                ) %>% 
  left_join(states_daterange_clean %>% 
              dplyr::filter(header == "VIRAL (RT-PCR) LAB TESTING: LAST WEEK") %>% 
              dplyr::select(file_name,header,daterange),
            by = "file_name")

# COUNTIES: Example for indicators which belong to same header : "VIRAL (RT-PCR) LAB TESTING: LAST WEEK"
counties_example_df <- counties_df_clean %>% 
  dplyr::select(file_name,date_of_file,
                V01,V02,state,county,
                V33,V34,V35,V36,V37,
  ) %>% 
  left_join(counties_daterange_clean %>% 
              dplyr::filter(header == "VIRAL (RT-PCR) LAB TESTING: LAST WEEK") %>% 
              dplyr::select(file_name,header,daterange),
            by = "file_name")

