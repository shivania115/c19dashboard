# API: https://data.cdc.gov/resource/xkkf-xrst.json
# URL: https://data.cdc.gov/NCHS/Excess-Deaths-Associated-with-COVID-19/xkkf-xrst/
# - https://dev.socrata.com/foundry/data.cdc.gov/xkkf-xrst
# CSV: https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=

folder_name <- Sys.Date()

xkkf_xrst_url <- "https://data.cdc.gov/api/views/xkkf-xrst/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target="
xkkf_xrst <- read_csv(xkkf_xrst_url)

date_as_of <- xkkf_xrst$`Week Ending Date` %>% max(.)

write.csv(xkkf_xrst,paste0(path_c19dashboard_shared_folder,"/Data/Raw/Excess Deaths/",folder_name,
                           "/Excess Deaths Associated with COVID-19_",date_as_of,".csv"),
          row.names=FALSE)


xkkf_xrst_cleaned <- xkkf_xrst %>% 
  rename(week_ending_date =  "Week Ending Date",
         statename = "State" ,
         observed_number = "Observed Number",
         upper_bound_threshold = "Upper Bound Threshold",
         exceeds_threshold = "Exceeds Threshold",
         average_expected_count = "Average Expected Count",
         excess_lower_estimate = "Excess Lower Estimate",
         excess_higher_estimate = "Excess Higher Estimate",
         year = "Year",
         total_excess_lower_estimate_in_2020 = "Total Excess Lower Estimate in 2020",
         total_excess_higher_estimate_in_2020 = "Total Excess Higher Estimate in 2020",
         percent_excess_lower_estimate = "Percent Excess Lower Estimate",
         percent_excess_higher_estimate = "Percent Excess Higher Estimate",
         type = "Type",
         outcome = "Outcome",
         suppress = "Suppress",
         note = "Note") %>% 
  mutate(state = cdlTools::fips(statename),
         state_abbreviation = cdlTools::fips(statename,to="Abbreviation")) %>% 
  mutate(week_ending_date = lubridate::ymd(week_ending_date,tz = "America/New_York"))

saveRDS(xkkf_xrst_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/Excess_Deaths_Associated_with_COVID-19_cleaned.RDS"))
write.csv(head(xkkf_xrst_cleaned,n = 1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/EXAMPLE_Excess_Deaths_Associated_with_COVID-19.csv"),row.names = FALSE)


