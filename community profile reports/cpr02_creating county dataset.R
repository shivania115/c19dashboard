

path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports")
path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports")

f = list.files(path_cpr_raw)
f = f[regexpr("\\.xlsx",f)>0]


# file_name = "Community Profile Report 20210314.xlsx"

source(paste0(path_c19dashboard_repo,"/community profile reports/aux02_functions for cleaning cpr files.R"))


cpr_cleaned_list <- map(f,
                    function(x){
                      
                      county_cpr(x)
                      
                      
                    }
                    )

# Removing Patch ------------
# Community_Profile_Report_20210409_Public.xlsx
# files_with_issues <- c("Community_Profile_Report_20210412_Public.xlsx","Community_Profile_Report_20210411_Public.xlsx","Community_Profile_Report_20210409_Public.xlsx","Community Profile Report 20210330.xlsx")
# index_files_with_issues <- which(f %in% files_with_issues)
# # for(i in index_files_with_issues){
#   cpr_cleaned_list[[i]][1][[1]]$V07 <- paste0("Region ",cpr_cleaned_list[[i]][1][[1]]$V07)
# }


df_clean <- map_dfr(cpr_cleaned_list,function(x) x[[1]]) %>% 
  mutate(fips = sprintf("%05d",V02)) %>%
  mutate(state = substr(fips,1,2) %>% as.numeric(),
         county = substr(fips,3,5) %>% as.numeric()) %>% 
  dplyr::select(-fips)

date_range_clean <- map_dfr(cpr_cleaned_list,function(x) x[[2]])
error_list <- map_dfr(cpr_cleaned_list,function(x) x[[3]])


# Merging with headers
date_range_clean <- date_range_clean %>% 
  mutate(daterange = str_replace_all(daterange,"(\\(|\\))","")) %>% 
  mutate(header = trimws(header)) %>% 
  left_join(readxl::read_excel(paste0(path_cpr_processed,"/CPR Variable List.xlsx"),sheet = "Headers") %>% 
              mutate(header = trimws(header)) %>% 
              dplyr::filter(Level == "Counties"),
            by = "header"
              ) %>% 
  dplyr::select(-Level,-header) %>% 
  pivot_wider(names_from = "variable",values_from="daterange")

date_range_clean <- date_range_clean[,gtools::mixedsort(colnames(date_range_clean))]

df_clean <- df_clean %>% 
  left_join(date_range_clean,
            by=c("date_of_file","file_name"))


# date_range_clean <- readRDS(paste0(path_cpr_processed,folder_name,"/counties_date_range_clean.RDS"))
# df_clean <- readRDS(paste0(path_cpr_processed,folder_name,"/counties_df_clean.RDS"))


# Save ----------------
write.csv(error_list,paste0(path_cpr_processed,folder_name,"/counties_error_list.csv"))

saveRDS(df_clean,paste0(path_cpr_processed,folder_name,"/counties_df_clean.RDS"))
saveRDS(date_range_clean,paste0(path_cpr_processed,folder_name,"/counties_date_range_clean.RDS"))
saveRDS(error_list,paste0(path_cpr_processed,folder_name,"/counties_error_list.RDS"))

haven::write_dta(df_clean,paste0(path_cpr_processed,folder_name,"/counties_df_clean.dta"),version=12)
haven::write_dta(date_range_clean,paste0(path_cpr_processed,folder_name,"/counties_date_range_clean.dta"),version=12)
haven::write_dta(error_list,paste0(path_cpr_processed,folder_name,"/counties_error_list.dta"),version=12)

write.csv(df_clean,paste0(path_cpr_processed,folder_name,"/counties_df_clean.csv"),row.names = FALSE)
write.csv(date_range_clean,paste0(path_cpr_processed,folder_name,"/counties_date_range_clean.csv"),row.names = FALSE)
