# https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html 

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
            )

# Restricting to those age groups with population totals -----------
remove_est01 <- est01_df %>% 
  dplyr::filter(is.na(Population_num))

# Summarizing state population totals for those county x age groups with pop totals --------
state_pop <- est01_df %>% 
  dplyr::filter(!is.na(Population_num)) %>% 
  group_by(state,Five.Year.Age.Groups.Code) %>% 
  summarize(state_Population = sum(Population_num,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(state_prop_Population = state_Population/sum(state_Population)) %>% 
  ungroup()

# Estimating adjusted death rates as per state population distribution --------
death_rates <- est01_df %>% 
  dplyr::filter(!is.na(Population_num)) %>% 
  left_join(state_pop,
            by = c("state","Five.Year.Age.Groups.Code")) %>% 
  mutate(crude_rate = Deaths_num/Population_num) %>% 
  group_by(state,county) %>% 
  summarize(adjusted_rate = sum(crude_rate*state_prop_Population),
            n_agegroups = n())


