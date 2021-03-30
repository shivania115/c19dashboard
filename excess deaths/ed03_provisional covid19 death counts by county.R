# URL: https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy
# API: https://data.cdc.gov/resource/kn79-hsxy.csv
# - https://dev.socrata.com/foundry/data.cdc.gov/kn79-hsxy

kn79_hsxy_url <- "https://data.cdc.gov/api/views/kn79-hsxy/rows.csv?accessType=DOWNLOAD"
kn79_hsxy <- read_csv(kn79_hsxy_url)

kn79_hsxy_cleaned <- kn79_hsxy %>% 
  rename(data_as_of = "Date as of",
         start_week = "Start Date",
         end_week = "End Date",
         state_abbreviation = "State", # Originally state_name
         county_name = "County name", 
         fips = "FIPS County Code", # Originally county_fips_code
         urban_rural_code = "Urban Rural Code",
         covid_death = "Deaths involving COVID-19",
         total_death = "Deaths from All Causes",
         footnote = "Footnote") %>% 
  mutate_at(vars(data_as_of,start_week,end_week),~lubridate::mdy(.)) %>% 
  mutate(fips = sprintf("%05d",fips)) %>% 
  mutate(state = substr(fips,1,2) %>% as.numeric(),
         county = substr(fips,3,5) %>% as.numeric()) %>% 
  mutate(fips = as.numeric(fips))


saveRDS(kn79_hsxy_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/Provisional_COVID-19_Death_Counts_in_the_United_States_by_County_cleaned.RDS"))
write.csv(head(kn79_hsxy_cleaned,n = 1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/EXAMPLE_Provisional_COVID-19_Death_Counts_in_the_United_States_by_County.csv"),row.names = FALSE)


