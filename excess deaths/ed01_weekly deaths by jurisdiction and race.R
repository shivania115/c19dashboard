# URL: https://data.cdc.gov/NCHS/Weekly-counts-of-deaths-by-jurisdiction-and-race-a/qfhf-uhaa
# API: https://data.cdc.gov/resource/qfhf-uhaa.json
# - https://dev.socrata.com/foundry/data.cdc.gov/qfhf-uhaa
# CSV: https://data.cdc.gov/api/views/qfhf-uhaa/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target=

# Both unweighted and weighted (predicted) provisional counts are provided. 
# Weighting of provisional counts is done to account for potential underreporting in the most recent weeks. 
# However, data for the most recent week(s) are still likely to be incomplete. Only about 60% of deaths are reported within 10 days of the date of death, 
# and there is considerable variation by jurisdiction. The predicted numbers of deaths may be too low for some groups and possibly too high for others.


qfhf_uhaa_url <- "https://data.cdc.gov/api/views/qfhf-uhaa/rows.csv?accessType=DOWNLOAD&bom=true&format=true%20target="
qfhf_uhaa <- read_csv(qfhf_uhaa_url) # 100 MB file

qfhf_uhaa_cleaned <- qfhf_uhaa %>% 
  dplyr::rename(jurisdiction = Jurisdiction,
                week_ending_date = 'Week Ending Date',
                state_abbreviation = 'State Abbreviation',
                mmwryear = 'MMWRYear',
                mmwrweek = 'MMWRWeek',
                race_ethnicity = 'Race/Ethnicity',
                time_period = 'Time Period',
                suppress = 'Suppress',
                note = 'Note',
                outcome = 'Outcome',
                number_of_deaths = 'Number of Deaths',
                average_number_of_deaths = 'Average Number of Deaths in Time Period',
                difference_from_2015_2019 = 'Difference from 2015-2019 to 2020',
                percent_difference_from_2015 = 'Percent Difference from 2015-2019 to 2020',
                type = 'Type') %>% 
  mutate(state = cdlTools::fips(state_abbreviation)) %>% 
  mutate(week_ending_date = lubridate::mdy(week_ending_date,tz = "America/New_York"))

saveRDS(qfhf_uhaa_cleaned,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/Weekly_counts_of_deaths_by_jurisdiction_and_race_and_Hispanic_origin_cleaned.RDS"))
write.csv(head(qfhf_uhaa_cleaned,n = 1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/EXAMPLE_Weekly_counts_of_deaths_by_jurisdiction_and_race_and_Hispanic_origin.csv"),)
