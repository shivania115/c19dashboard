


counties_update <- map(update_files,
                       function(x){
                         
                         clean_cpr(x)
                         
                         
                       }
)

counties_df_clean <- map_dfr(counties_update,function(x) x[[1]])
counties_date_range_clean <- map_dfr(counties_update,function(x) x[[2]])
counties_error_list <- map_dfr(counties_update,function(x) x[[3]])

# Appending counties data ------------

counties_df_clean_updated <- bind_rows(readRDS(paste0(path_cpr_processed,"/counties_df_clean.RDS")),
                                       counties_df_clean)
counties_date_range_clean_updated <- bind_rows(readRDS(paste0(path_cpr_processed,"/counties_date_range_clean.RDS")),
                                               counties_date_range_clean)
counties_error_list_updated <- bind_rows(readRDS(paste0(path_cpr_processed,"/counties_error_list.RDS")),
                                         counties_error_list)
if(nrow(counties_error_list)==0){
  replace_dataset(counties_df_clean_updated,
                  save_path = paste0(path_cpr_processed),
                  save_name = "counties_df_clean",save_type = ".RDS",all_formats = TRUE)
  replace_dataset(counties_date_range_clean_updated,
                  save_path = paste0(path_cpr_processed),
                  save_name = "counties_date_range_clean",save_type = ".RDS",all_formats = TRUE)
  replace_dataset(counties_error_list_updated,
                  save_path = paste0(path_cpr_processed),
                  save_name = "counties_error_list",save_type = ".RDS",all_formats = TRUE)
  
}


