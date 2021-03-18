


ref_file <- readxl::read_excel(paste0(path_cpr_folder,"/raw/Community_Profile_Report_20210108_Public.xlsx"),
                               sheet="Counties",skip=1,col_names = TRUE)


month_vec = paste0(month.name,collapse="|")

county_colname_ref = readxl::read_excel(paste0(path_cpr_folder,
                                        "/raw/Community_Profile_Report_20210108_Public.xlsx"),
                                 sheet = "Counties",n_max=1,col_names = TRUE) %>% 
  pivot_longer(cols=everything(),names_to="header",values_to="colname") %>% 
  mutate(header = str_replace(header,"\\.\\.[0-9]+",NA_character_)) %>% 
  mutate(header = c("",zoo::na.locf(header))) %>% 
  mutate(daterange = str_extract(header,paste0("\\([",month_vec,"\\s+0-9\\-]+\\)"))) %>% 
  mutate(header = str_replace(header,paste0("\\([",month_vec,"\\s+0-9\\-]+\\)"),"")) %>% 
  mutate_if(is.character,~trimws(.))

# For initial run -----
# write.csv(county_colname_ref,paste0(path_cpr_folder,"/CPR COUNTY header.csv"),row.names = FALSE)

# STATES --------------
ref_file <- readxl::read_excel(paste0(path_cpr_folder,"/raw/Community_Profile_Report_20210108_Public.xlsx"),
                               sheet="States",skip=1,col_names = TRUE)


month_vec = paste0(month.name,collapse="|")

state_colname_ref = readxl::read_excel(paste0(path_cpr_folder,
                                        "/raw/Community_Profile_Report_20210108_Public.xlsx"),
                                 sheet = "States",n_max=1,col_names = TRUE) %>% 
  pivot_longer(cols=everything(),names_to="header",values_to="colname") %>% 
  mutate(header = str_replace(header,"\\.\\.[0-9]+",NA_character_)) %>% 
  mutate(header = c("",zoo::na.locf(header))) %>% 
  mutate(daterange = str_extract(header,paste0("\\([",month_vec,"\\s+0-9\\-]+\\)"))) %>% 
  mutate(header = str_replace(header,paste0("\\([",month_vec,"\\s+0-9\\-]+\\)"),"")) %>% 
  mutate_if(is.character,~trimws(.))

# For initial run -----
# write.csv(state_colname_ref,paste0(path_cpr_folder,"/CPR STATE header.csv"),row.names = FALSE)


