
# This script updates all Excess deaths related datasets in the Processed folder
# This script also saves the Raw datasets as per the date of running code
# folder_name is re-initialized in each script - if we need to change the Raw target folder, update it in each script

folder_name <- Sys.Date()

path_ed_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Excess Deaths/")
path_ed_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/")

# This creates a folder which is as per the latest file in Data/Raw/Community Profile Reports/
if(!dir.exists(paste0(path_ed_raw,folder_name))){
  dir.create(paste0(path_ed_raw,folder_name))
}


source(paste0(path_c19dashboard_repo,"/excess deaths/ed01_weekly deaths by jurisdiction and race.R"))
source(paste0(path_c19dashboard_repo,"/excess deaths/ed02_excess deaths associated with covid19.R"))
source(paste0(path_c19dashboard_repo,"/excess deaths/ed03_provisional covid19 death counts by county.R"))
source(paste0(path_c19dashboard_repo,"/excess deaths/ed04_provisional covid19 death counts by county x race.R"))

