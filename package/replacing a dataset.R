
replace_dataset <- function(df,save_path,save_name,save_type=".RDS",all_formats=TRUE){
  
  # What is the correct file name? Just in case save_type is already in the save_name
  file_name = ifelse(regexpr(pattern = save_type,save_name)>0,save_name,paste0(save_name,save_type))
  
  
  # When was the file created?
  old_file_info = file.info(paste0(save_path,"/",file_name))
  
  old_file_mdate = lubridate::date(old_file_info$mtime)
  
  # Create a folder to store the older version of the file
  if(!dir.exists(paste0(save_path,"/",old_file_mdate))){
    dir.create(paste0(save_path,"/",old_file_mdate))
  }
  
  if(all_formats==TRUE){
    types = c(".dta",".RDS")
  } else{types = type}
  
  # Copy the stata file and write the new stata file
  if(".dta" %in% types){
    dta_file_name = str_replace(file_name,save_type,".dta")
    
    file.copy(from = paste0(save_path,"/",dta_file_name),
              to=paste0(save_path,"/",old_file_mdate,"/",dta_file_name))
    
    haven::write_dta(df,paste0(save_path,"/",dta_file_name),version = 12)
  }
  
  # Copy the RDS file and write the new RDS file
  if(c(".RDS") %in% types){
    rds_file_name = str_replace(file_name,save_type,".RDS")
    file.copy(from = paste0(save_path,"/",rds_file_name),
              to=paste0(save_path,"/",old_file_mdate,"/",rds_file_name))
    
    saveRDS(df,paste0(save_path,"/",rds_file_name))
  }
  
  # Copy the csv file and write the new csv file
  if(c(".csv") %in% types){
    csv_file_name = str_replace(file_name,save_type,".csv")
    file.copy(from = paste0(save_path,"/",csv_file_name),
              to=paste0(save_path,"/",old_file_mdate,"/",csv_file_name))
    
    write.csv(df,paste0(save_path,"/",csv_file_name))
  }
  
  
}



