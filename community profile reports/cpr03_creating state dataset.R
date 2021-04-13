
path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports")
path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports")

f = list.files(path_cpr_raw)
f = f[regexpr("\\.xlsx",f)>0]


source(paste0(path_c19dashboard_repo,"/community profile reports/aux02_functions for cleaning cpr files.R"))


states_cpr_cleaned_list <- map(f,
                        function(x){
                          
                          state_cpr(x)
                          
                          
                        }
)

states_df_clean <- map_dfr(states_cpr_cleaned_list,function(x) x[[1]]) %>% 
  mutate(state = cdlTools::fips(S02),
         county = NA_real_)
  
states_date_range_clean <- map_dfr(states_cpr_cleaned_list,function(x) x[[2]])
states_error_list <- map_dfr(states_cpr_cleaned_list,function(x) x[[3]])


# Merging with headers
states_date_range_clean <- states_date_range_clean %>% 
  mutate(daterange = str_replace_all(daterange,"(\\(|\\))","")) %>% 
  mutate(header = trimws(header)) %>% 
  left_join(readxl::read_excel(paste0(path_cpr_processed,"/CPR Variable List.xlsx"),sheet = "Headers") %>% 
              mutate(header = trimws(header)) %>% 
              dplyr::filter(Level == "States"),
            by = "header"
  ) %>% 
  dplyr::select(-Level,-header) %>% 
  pivot_wider(names_from = "variable",values_from="daterange")

states_date_range_clean <- states_date_range_clean[,gtools::mixedsort(colnames(states_date_range_clean))]

states_df_clean <- states_df_clean %>% 
  left_join(states_date_range_clean,
            by=c("date_of_file","file_name"))


# states_date_range_clean <- readRDS(paste0(path_cpr_processed,folder_name,"/states_date_range_clean.RDS"))
# states_df_clean <- readRDS(paste0(path_cpr_processed,folder_name,"/states_df_clean.RDS"))
# states_error_list <- readRDS(paste0(path_cpr_processed,folder_name,"/states_error_list.RDS"))




# Save ----------------
write.csv(states_error_list,paste0(path_cpr_processed,folder_name,"/states_error_list.csv"))


saveRDS(states_df_clean,paste0(path_cpr_processed,folder_name,"/states_df_clean.RDS"))
saveRDS(states_date_range_clean,paste0(path_cpr_processed,folder_name,"/states_date_range_clean.RDS"))
saveRDS(states_error_list,paste0(path_cpr_processed,folder_name,"/states_error_list.RDS"))

haven::write_dta(states_df_clean,paste0(path_cpr_processed,folder_name,"/states_df_clean.dta"),version=12)
haven::write_dta(states_date_range_clean,paste0(path_cpr_processed,folder_name,"/states_date_range_clean.dta"),version=12)
haven::write_dta(states_error_list,paste0(path_cpr_processed,folder_name,"/states_error_list.dta"),version=12)

write.csv(states_df_clean,paste0(path_cpr_processed,folder_name,"/states_df_clean.csv"),row.names = FALSE)
write.csv(states_date_range_clean,paste0(path_cpr_processed,folder_name,"/states_date_range_clean.csv"),row.names = FALSE)
