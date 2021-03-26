
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

states_df_clean <- map_dfr(states_cpr_cleaned_list,function(x) x[[1]])
states_date_range_clean <- map_dfr(states_cpr_cleaned_list,function(x) x[[2]])
states_error_list <- map_dfr(states_cpr_cleaned_list,function(x) x[[3]])

# Save ----------------
write.csv(states_error_list,paste0(path_cpr_processed,folder_name,"/states_error_list.csv"))


saveRDS(states_df_clean,paste0(path_cpr_processed,folder_name,"/states_df_clean.RDS"))
saveRDS(states_date_range_clean,paste0(path_cpr_processed,folder_name,"/states_date_range_clean.RDS"))
saveRDS(states_error_list,paste0(path_cpr_processed,folder_name,"/states_error_list.RDS"))

haven::write_dta(states_df_clean,paste0(path_cpr_processed,folder_name,"/states_df_clean.dta"),version=12)
haven::write_dta(states_date_range_clean,paste0(path_cpr_processed,folder_name,"/states_date_range_clean.dta"),version=12)
haven::write_dta(states_error_list,paste0(path_cpr_processed,folder_name,"/states_error_list.dta"),version=12)
