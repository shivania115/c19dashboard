# https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html 

raw_files <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports"))
file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
raw_files <- raw_files[!is.na(file_dates)]
latest_file <- raw_files[which.max(file_dates)]
folder_name = paste0("/",as.Date(max(file_dates),origin = "1970-01-01"))

# Extracting the data and creating fips codes ------------
est01_df <- read.csv(paste0(path_c19dashboard_personal_folder,"/Ad Hoc/Age standardization/USA Age adjusted death rate 5y.txt"),sep="\t") %>% 
  dplyr::filter(Notes == "") %>% 
  dplyr::select(-Notes) %>% 
  mutate(fips = sprintf("%05d",County.Code)) %>% 
  mutate(state = substr(fips,1,2) %>% as.numeric(),
         county = substr(fips,3,5) %>% as.numeric()
         ) %>% 
  mutate_at(vars(Deaths,Population),
            .funs = list(num =~as.numeric(as.character(.))),
            .names="{fn}_{col}"
            ) %>% 
  mutate(age4cat = case_when(Five.Year.Age.Groups.Code %in% c("1","1-4","5-9","10-14","15-19") ~ 1,
                                      Five.Year.Age.Groups.Code %in% c("20-24","25-29","30-34","35-39","40-44" )~ 2,
                                      Five.Year.Age.Groups.Code %in% c("45-49","50-54","55-59","60-64")~ 3,
                                      Five.Year.Age.Groups.Code %in% c("65-69","70-74","75-79","80-84",
                                                                       "85-89","90-94","95-99","100+")~ 4,
                                      TRUE ~ NA_real_
                                      
                                      ))

# Restricting to those age groups with population totals -----------
remove_est01 <- est01_df %>% 
  dplyr::filter(is.na(Population_num))

write.csv(remove_est01,paste0(path_c19dashboard_personal_folder,"/Ad Hoc/Age standardization/removed for not having population totals.csv"))

# Summarizing state population totals for those county x age groups with pop totals --------
state_pop <- est01_df %>% 
  dplyr::filter(!is.na(Population_num)) %>% 
  
  group_by(state,age4cat) %>% 
  summarize(state_Population = sum(Population_num,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(state_prop_Population = state_Population/sum(state_Population)) %>% 
  ungroup()
write.csv(state_pop,paste0(path_c19dashboard_personal_folder,"/Ad Hoc/Age standardization/state_population proportions.csv"),row.names = FALSE)


# Estimating adjusted death rates as per state population distribution --------
death_rates <- est01_df %>% 
  dplyr::filter(!is.na(Population_num)) %>% 
  left_join(state_pop,
            by = c("state","age4cat")) %>% 
  mutate(crude_rate = Deaths_num/Population_num) %>% 
  group_by(state,county) %>% 
  summarize(adjusted_rate = sum(crude_rate*state_prop_Population),
            n_agegroups = n())


