# Function for generating cleaned CPR datasets -------------

county_cpr <- function(file_name){
  path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports/")
  path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/")
  
  
  df = readxl::read_excel(paste0(path_cpr_raw,file_name),sheet = "Counties",skip = 1,col_names = TRUE)
  
  colname_ref <- readxl::read_excel(paste0(path_cpr_processed,"/CPR Variable List.xlsx"),sheet = "Counties")
  
  colname_df = readxl::read_excel(paste0(path_cpr_raw,file_name),sheet = "Counties",n_max=1,col_names = TRUE) %>% 
    pivot_longer(cols=everything(),names_to="header",values_to="colname")
  
  month_vec = paste0(month.name,collapse="|")
  upper_month_vec = paste0(str_to_upper(month.name),collapse="|")
  
  colname_df <- colname_df %>% 
    mutate(header = str_replace(header,"\\.\\.[0-9]+",NA_character_)) %>% 
    mutate(header = c("",zoo::na.locf(header))) %>% 
    mutate(header = case_when(colname == "County" ~ "County",
                              TRUE ~ header)) %>% 
    # Extract date range for header row
    mutate(daterange = str_extract(header,paste0("\\((",month_vec,").*\\)$"))) %>% 
    
    # Extract header values 
    mutate(header = str_replace(header,paste0("\\((",month_vec,").*\\)$"),"")) %>% 
    
    # Remove white spaces for merging
    mutate_if(is.character,~trimws(.)) %>% 
    left_join(colname_ref,
              by=c("header","colname")) %>% 
    # In some cases, header == FORECASTING RESULTS may not be parsed correctly. The below line is a catch-all for that.
    mutate(variable = case_when(colname == "Forecast case trajectory" ~ "V92",
                                TRUE ~ variable)) %>% 
    mutate(file_name = file_name) 
  
  # Extract unique date ranges from 1st header row
  unique_date_ranges <- colname_df %>% 
    dplyr::filter(!is.na(daterange)|colname == "Forecast case trajectory") %>% 
    distinct(header,daterange) %>% 
    mutate(file_name = file_name)
  
  # Extract error observations which didn't merge correctly
  error_list <- colname_df %>% 
    dplyr::filter(is.na(variable))
  
  names(df) <- colname_df$variable
  
  df$file_name = file_name
  date_of_file = str_extract(file_name,"[0-9]+")
  
  df$date_of_file = lubridate::ymd(date_of_file) - 1
  unique_date_ranges$date_of_file = lubridate::ymd(date_of_file) - 1
  return(list(df,unique_date_ranges,error_list))
  
  
}



state_cpr <- function(file_name){
  path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports/")
  path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/")
  
  
  df = readxl::read_excel(paste0(path_cpr_raw,file_name),sheet = "States",skip = 1,col_names = TRUE)  
  
  
  colname_ref <- readxl::read_excel(paste0(path_cpr_processed,"/CPR Variable List.xlsx"),sheet = "States")
  
  colname_df = readxl::read_excel(paste0(path_cpr_raw,file_name),sheet = "States",n_max=1,col_names = TRUE) %>% 
    pivot_longer(cols=everything(),names_to="header",values_to="colname")
  
  month_vec = paste0(month.name,collapse="|")
  upper_month_vec = paste0(str_to_upper(month.name),collapse="|")
  
  colname_df <- colname_df %>% 
    mutate(header = str_replace(header,"\\.\\.[0-9]+",NA_character_)) %>% 
    mutate(header = c("",zoo::na.locf(header))) %>% 
    mutate(header = case_when(colname == "State" ~ "State",
                              TRUE ~ header)) %>% 
    # Extract date range for header row
    mutate(daterange = str_extract(header,paste0("\\((",month_vec,").*\\)$"))) %>% 
    
    # Extract header values 
    mutate(header = str_replace(header,paste0("\\((",month_vec,").*\\)$"),"")) %>% 
    
    # Remove white spaces for merging
    mutate_if(is.character,~trimws(.)) %>% 
    left_join(colname_ref,
              by=c("header","colname")) %>% 
    
    # In some cases, header == FORECASTING RESULTS may not be parsed correctly. The below line is a catch-all for that.
    mutate(variable = case_when(colname == "Forecast case trajectory" ~ "V92",
                                TRUE ~ variable)) %>% 
    mutate(file_name = file_name) 
  
  unique_date_ranges <- colname_df %>% 
    dplyr::filter(!is.na(daterange)|colname == "Forecast case trajectory") %>% 
    distinct(header,daterange) %>% 
    mutate(file_name = file_name)
  
  error_list <- colname_df %>% 
    dplyr::filter(is.na(variable))
  
  names(df) <- colname_df$variable
  
  df$file_name = file_name
  date_of_file = str_extract(file_name,"[0-9]+")
  
  df$date_of_file = lubridate::ymd(date_of_file) - 1
  unique_date_ranges$date_of_file = lubridate::ymd(date_of_file) - 1
  return(list(df,unique_date_ranges,error_list))
  
  
}

# National CPR ------------

# Testing
# file_name = "Community_Profile_Report_20210108_Public.xlsx"


national_historic_nskip <- function(file_name){
  
  path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports/")
  path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/")
  df = readxl::read_excel(paste0(path_cpr_raw,file_name),sheet = "National Historic",skip = 0,col_names = TRUE)  
  
  s = which(df[,1]=="End Date")
  return(s)
  
}

national_cpr <- function(file_name){
  path_cpr_raw <- paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports/")
  path_cpr_processed <- paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/")
  
  
  s = national_historic_nskip(file_name)
  
  df = readxl::read_excel(paste0(path_cpr_raw,file_name),sheet = "National Historic",skip = s,col_names = TRUE)  
  print(file_name)
  
  colname_ref <- readxl::read_excel(paste0(path_cpr_processed,"/CPR Variable List.xlsx"),sheet = "National")
  
  colname_df = readxl::read_excel(paste0(path_cpr_raw,file_name),sheet = "National Historic",skip=s-1,n_max=1,col_names = TRUE) %>% 
    pivot_longer(cols=everything(),names_to="header",values_to="colname")
  
  month_vec = paste0(month.name,collapse="|")
  upper_month_vec = paste0(str_to_upper(month.name),collapse="|")
  
  colname_df <- colname_df %>% 
    mutate(header = str_replace(header,"\\.\\.[0-9]+",NA_character_)) %>% 
    
    # Different from other cpr cleaning functions since Date is the first variable ----
    mutate(header = c(zoo::na.locf(header))) %>% 
    mutate(header = case_when(colname == "State" ~ "State",
                              TRUE ~ header)) %>% 
    # Extract date range for header row
    mutate(daterange = str_extract(header,paste0("\\((",month_vec,").*\\)$"))) %>% 
    
    # Extract header values 
    mutate(header = str_replace(header,paste0("\\((",month_vec,").*\\)$"),"")) %>% 
    
    # Remove white spaces for merging
    mutate_if(is.character,~trimws(.)) %>% 
    left_join(colname_ref,
              by=c("header","colname")) %>% 
    
    # In some cases, colname is blank ----------------
    mutate(variable = case_when(is.na(colname) ~ "NA",
                                TRUE ~ variable)) %>% 
    mutate(file_name = file_name) 
  
  unique_date_ranges <- colname_df %>% 
    dplyr::filter(!is.na(daterange)|colname == "Forecast case trajectory") %>% 
    distinct(header,daterange) %>% 
    mutate(file_name = file_name)
  
  error_list <- colname_df %>% 
    dplyr::filter(is.na(variable)) %>% 
    dplyr::select(-starts_with("\\.\\."))
  
  names(df)[is.na(names(df))] <- "NA"
  
  names(df) <- colname_df$variable
  
  df$file_name = file_name
  date_of_file = str_extract(file_name,"[0-9]+")
  
  df$date_of_file = lubridate::ymd(date_of_file) - 1
  unique_date_ranges$date_of_file = lubridate::ymd(date_of_file) - 1
  return(list(df,unique_date_ranges,error_list))
  
  
}
