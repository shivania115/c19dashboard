

path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports")
path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports")

f = list.files(path_cpr_raw)
f = f[regexpr("\\.xlsx",f)>0]


# file_name = "Community Profile Report 20210314.xlsx"

source(paste0(path_c19dashboard_repo,"/community profile reports/aux02_functions for cleaning cpr files.R"))


cpr_cleaned_list <- map(f,
                    function(x){
                      
                      clean_cpr(x)
                      
                      
                    }
                    )

df_clean <- map_dfr(cpr_cleaned_list,function(x) x[[1]])
date_range_clean <- map_dfr(cpr_cleaned_list,function(x) x[[2]])
error_list <- map_dfr(cpr_cleaned_list,function(x) x[[3]])

# Save ----------------
write.csv(error_list,paste0(path_cpr_processed,folder_name,"/counties_error_list.csv"))

saveRDS(df_clean,paste0(path_cpr_processed,folder_name,"/counties_df_clean.RDS"))
saveRDS(date_range_clean,paste0(path_cpr_processed,folder_name,"/counties_date_range_clean.RDS"))
saveRDS(error_list,paste0(path_cpr_processed,folder_name,"/counties_error_list.RDS"))

haven::write_dta(df_clean,paste0(path_cpr_processed,folder_name,"/counties_df_clean.dta"),version=12)
haven::write_dta(date_range_clean,paste0(path_cpr_processed,folder_name,"/counties_date_range_clean.dta"),version=12)
haven::write_dta(error_list,paste0(path_cpr_processed,folder_name,"/counties_error_list.dta"),version=12)
