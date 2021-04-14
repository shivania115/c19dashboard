
path_scripts <- paste0(path_c19dashboard_shared_folder,"/Scripts")

file_list <- list.files(path_scripts)

for (f in file_list){
  
  file_info_f <- file.info(paste0(path_scripts,"/",f))
  
  if(file_info_f["mtime"][[1]] > paste0(Sys.Date()," 00:00:00 EDT") %>% lubridate::ymd_hms(.)){
    file.copy(from = paste0(path_scripts,"/",f),to = paste0(path_c19dashboard_repo,"/dashboard data/",f))
  }
}

