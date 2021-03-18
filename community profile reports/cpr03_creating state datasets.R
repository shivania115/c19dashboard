


f = list.files(paste0(path_cpr_folder,"/raw"))
f = f[regexpr("\\.xlsx",f)>0]

state_cpr <- function(file_name){
  path_cpr_folder <- paste0(path_c19dashboard_shared_folder,"/Community Profile Reports")
  
  
  df = readxl::read_excel(paste0(path_cpr_folder,"/raw/",file_name),sheet = "States",skip = 1,col_names = TRUE)  
  
  
  colname_ref <- readxl::read_excel(paste0(path_cpr_folder,"/CPR Variable List.xlsx"),sheet = "States")
  
  colname_df = readxl::read_excel(paste0(path_healthdata_gov_folder,"/raw/",file_name),sheet = "States",n_max=1,col_names = TRUE) %>% 
    pivot_longer(cols=everything(),names_to="header",values_to="colname")
  
  month_vec = paste0(month.name,collapse="|")
  upper_month_vec = paste0(str_to_upper(month.name),collapse="|")
  
  colname_df <- colname_df %>% 
    mutate(header = str_replace(header,"\\.\\.[0-9]+",NA_character_)) %>% 
    mutate(header = c("",zoo::na.locf(header))) %>% 
    mutate(header = case_when(colname == "State" ~ "State",
                              TRUE ~ header)) %>% 
    # Extract date range for header row
    mutate(daterange = str_extract(header,paste0("\\([(",month_vec,")\\s+0-9\\-]+\\)"))) %>% 
    
    # Extract header values 
    mutate(header = str_replace(header,paste0("\\([(",month_vec,")\\s+0-9\\-]+\\)"),"")) %>% 
    
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
  
  return(list(df,unique_date_ranges,error_list))
  
  
}


cpr_cleaned_list <- map(f,
                        function(x){
                          
                          state_cpr(x)
                          
                          
                        }
)

df_clean <- map_dfr(cpr_cleaned_list,function(x) x[[1]])
date_range_clean <- map_dfr(cpr_cleaned_list,function(x) x[[2]])
error_list <- map_dfr(cpr_cleaned_list,function(x) x[[3]])

# Save ----------------
write.csv(error_list,paste0(path_healthdata_gov_folder,"/Community Profile Reports/states/error_list.csv"))


saveRDS(df_clean,paste0(path_cpr_folder,"/states/df_clean.RDS"))
saveRDS(date_range_clean,paste0(path_cpr_folder,"/states/date_range_clean.RDS"))
saveRDS(error_list,paste0(path_cpr_folder,"/states/error_list.RDS"))

write_dta(df_clean,paste0(path_cpr_folder,"/states/df_clean.dta"),version=12)
write_dta(date_range_clean,paste0(path_cpr_folder,"/states/date_range_clean.dta"),version=12)
write_dta(error_list,paste0(path_cpr_folder,"/states/error_list.dta"),version=12)
