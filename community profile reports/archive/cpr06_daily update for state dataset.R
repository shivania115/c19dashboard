

states_update <- map(update_files,
                     function(x){
                       
                       state_cpr(x)
                       
                       
                     }
)

states_df_clean <- map_dfr(states_update,function(x) x[[1]])
states_date_range_clean <- map_dfr(states_update,function(x) x[[2]])
states_error_list <- map_dfr(states_update,function(x) x[[3]])


# Appending states data ------------

states_df_clean_updated <- bind_rows(readRDS(paste0(path_cpr_processed,"/states_df_clean.RDS")),
                                       states_df_clean)
states_date_range_clean_updated <- bind_rows(readRDS(paste0(path_cpr_processed,"/states_date_range_clean.RDS")),
                                               states_date_range_clean)
states_error_list_updated <- bind_rows(readRDS(paste0(path_cpr_processed,"/states_error_list.RDS")),
                                         states_error_list)


if(nrow(states_error_list_updated) > 0){
  write.csv(states_error_list_updated,paste0(path_cpr_processed,"/states_error_list.csv"))
  replace_dataset(states_error_list_updated,
                  save_path = paste0(path_cpr_processed),
                  save_name = "states_error_list",save_type = ".RDS",all_formats = TRUE)
}
if(nrow(states_error_list_updated) == 0){
  replace_dataset(states_df_clean_updated,
                  save_path = paste0(path_cpr_processed),
                  save_name = "states_df_clean",save_type = ".RDS",all_formats = TRUE)
  replace_dataset(states_date_range_clean_updated,
                  save_path = paste0(path_cpr_processed),
                  save_name = "states_date_range_clean",save_type = ".RDS",all_formats = TRUE)
  replace_dataset(states_error_list_updated,
                  save_path = paste0(path_cpr_processed),
                  save_name = "states_error_list",save_type = ".RDS",all_formats = TRUE)
  
}
