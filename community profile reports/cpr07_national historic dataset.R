# Reading and assigning variable names ------------

path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports")
path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports")
raw_files <- list.files(path_cpr_raw)

# Just in case we want to try for latest file
# file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
# raw_files <- raw_files[!is.na(file_dates)]
# latest_file <- raw_files[which.max(file_dates)]  


f = list.files(path_cpr_raw)
f = f[regexpr("\\.xlsx",f)>0]


source(paste0(path_c19dashboard_repo,"/community profile reports/aux02_functions for cleaning cpr files.R"))


national_cpr_cleaned_list <- map(f,
                        function(x){
                          
                          national_cpr(x)
                          
                          
                        }
)

national_df_clean <- map_dfr(national_cpr_cleaned_list,function(x) x[[1]])
national_date_range_clean <- map_dfr(national_cpr_cleaned_list,function(x) x[[2]])
national_error_list <- map_dfr(national_cpr_cleaned_list,function(x) x[[3]])

# Save ----------------
write.csv(national_error_list,paste0(path_cpr_processed,"/national_error_list.csv"))


saveRDS(national_df_clean,paste0(path_cpr_processed,"/national_df_clean.RDS"))
saveRDS(national_date_range_clean,paste0(path_cpr_processed,"/national_date_range_clean.RDS"))
saveRDS(national_error_list,paste0(path_cpr_processed,"/national_error_list.RDS"))

haven::write_dta(national_df_clean,paste0(path_cpr_processed,"/national_df_clean.dta"),version=12)
haven::write_dta(national_date_range_clean,paste0(path_cpr_processed,"/national_date_range_clean.dta"),version=12)
haven::write_dta(national_error_list,paste0(path_cpr_processed,"/national_error_list.dta"),version=12)
