library(tidyverse)
library(haven)
library(rvest)
library(httr)

# Defining key paths ------------
url = "https://beta.healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9"
path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports")
path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports")


# Read all available files in the above link -----------


cpr_url <- read_html(url) %>%
  # html_nodes(css="attachment")
  html_nodes(xpath='//script[@type="text/javascript"]') %>% 
  .[7]
# https://beta.healthdata.gov/api/views/gqxm-d9w9/files/58495215-54c1-4513-8daf-b9cdc42f8a71?download=true&filename=Community_Profile_Report_20210315_Public.xlsx
a <- cpr_url %>% 
  html_text()

b = str_split(a[[1]],'","') %>% 
  data.frame( .) 

names(b) <- c("var")

# Restrict to excel files -------------
b <- b %>% 
  dplyr::filter(regexpr("^href",var)>0) %>% 
  mutate(var = str_replace(var,'href":"',"")) %>% 
  mutate(file_name = str_match(var, "filename\\s*(.*?)\\s*(\\.xlsx|.pdf)")[,1]) %>% 
  dplyr::filter(str_detect(var,".xlsx"))

# Restrict to files which are not already available ------------

existing_files <- list.files(path_cpr_raw)
existing_files = existing_files[regexpr("\\.xlsx",existing_files)>0]

new_files <- b %>% 
  data.frame() %>% 
  dplyr::filter(!(file_name %in% paste0("filename=",existing_files)))


# Download files ----------

if(nrow(new_files)>0){
  for (f in 1:nrow(new_files)){
    # for (f in 1:3){
    file_name = str_replace(new_files$file_name[f],pattern = "filename=","")
    
    download.file(paste0("https://beta.healthdata.gov/",b$var[f]),
                  destfile = paste0(path_cpr_raw,"/",file_name),mode = "wb")
    
  }
  
}



# Reading and assigning variable names ------------


source(paste0(path_c19dashboard_repo,"/community profile reports/cpraux02_functions for cleaning cpr files.R"))

update_files <- list.files(path_cpr_raw)
update_files = update_files[regexpr("\\.xlsx",update_files)>0]
update_files = update_files[!update_files %in% existing_files]



if(length(update_files)>0){
  
  raw_files <- list.files(path_cpr_raw)
  file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
  raw_files <- raw_files[!is.na(file_dates)]
  latest_file <- raw_files[which.max(file_dates)]
  folder_name = paste0("/",as.Date(max(file_dates),origin = "1970-01-01"))
  
  # This creates a folder which is as per the latest file in Data/Raw/Community Profile Reports/
  if(!dir.exists(paste0(path_cpr_processed,folder_name))){
    dir.create(paste0(path_cpr_processed,folder_name))
  }
  
  # source(paste0(path_c19dashboard_repo,"/package/replacing a dataset.R"))
  source(paste0(path_c19dashboard_repo,"/community profile reports/cpr02_creating county dataset.R"))
  source(paste0(path_c19dashboard_repo,"/community profile reports/cpr03_creating state dataset.R"))
  source(paste0(path_c19dashboard_repo,"/community profile reports/cpr04_creating national historic dataset.R"))
  
}



