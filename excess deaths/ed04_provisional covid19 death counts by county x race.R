# URL: https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-County-and-Ra/k8wy-p9cg
# API: https://data.cdc.gov/resource/k8wy-p9cg.csv
# - https://dev.socrata.com/foundry/data.cdc.gov/k8wy-p9cg

folder_name <- Sys.Date()

k8wy_p9cg_url <- "https://data.cdc.gov/api/views/k8wy-p9cg/rows.csv?accessType=DOWNLOAD"

k8wy_p9cg <- read_csv(k8wy_p9cg_url) 

date_as_of <- k8wy_p9cg$`Data as of` %>% unique() %>% lubridate::mdy(.)

write.csv(k8wy_p9cg,paste0(path_c19dashboard_shared_folder,"/Data/Raw/Excess Deaths/",folder_name,
                           "/Provisional COVID-19 Death Counts by County and Race cleaned_",date_as_of,".csv"),
          row.names=FALSE)


k8wy_p9cg_cleaned <- k8wy_p9cg %>% 
  rename(data_as_of = "Data as of" ,
         start_week = "Start Date",
         end_week = "End Date",
         state_abbreviation = "State", # Originally state
         county_name = "County Name",
         urban_rural_code_numeric = "Urban Rural Code", # Originally urbanruralcode
         state = "FIPS State",
         county = "FIPS County",
         fips = "FIPS Code",
         indicator = "Indicator",
         all_deaths_total = "Total deaths",
         covid_19_deaths_total = "COVID-19 Deaths",
         non_hispanic_white = "Non-Hispanic White",
         non_hispanic_black = "Non-Hispanic Black",
         non_hispanic_american_indian = "Non-Hispanic American Indian or Alaska Native",
         non_hispanic_asian = "Non-Hispanic Asian",
         non_hispanic_nhopi = "Non-Hispanic Native Hawaiian or Other Pacific Islander",
         hispanic = "Hispanic",
         other = "Other",
         urban_rural_code = "Urban Rural Description", # Originally urbanruraldesc
         footnote = "Footnote") %>% 
  mutate_at(vars(data_as_of,start_week,end_week),~lubridate::mdy(.))


saveRDS(k8wy_p9cg_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/Provisional_COVID-19_Death_Counts_by_County_and_Race_cleaned.RDS"))
write.csv(head(k8wy_p9cg_cleaned,n = 1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/EXAMPLE_Provisional_COVID-19_Death_Counts_by_County_and_Race.csv"),row.names = FALSE)

