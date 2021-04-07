
nytimes_national_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
nytimes_states_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
nytimes_counties_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"


nyt_national <- read_csv(nytimes_national_url)%>% 
  dplyr::mutate(nation = 1)
nyt_states <- read_csv(nytimes_states_url) %>% 
  dplyr::mutate(state = fips %>% as.numeric(.)) %>% 
  mutate(fips = as.numeric(fips))
nyt_counties <- read_csv(nytimes_counties_url) %>% 
  dplyr::mutate(state = substr(fips,1,2) %>% as.numeric(.),
                county = substr(fips,3,5) %>% as.numeric(.)) %>% 
  mutate(fips = as.numeric(fips))

# Save RAW ----------
national_last_date = max(nyt_national$date)
write.csv(nyt_national,paste0(path_c19dashboard_shared_folder,"/Data/Raw/NYT Covid19 data/nytimes us_",national_last_date,".csv"),row.names = FALSE)

states_last_date = max(nyt_states$date)
write.csv(nyt_states,paste0(path_c19dashboard_shared_folder,"/Data/Raw/NYT Covid19 data/nytimes states_",states_last_date,".csv"),row.names = FALSE)

counties_last_date = max(nyt_counties$date)
write.csv(nyt_counties,paste0(path_c19dashboard_shared_folder,"/Data/Raw/NYT Covid19 data/nytimes counties_",counties_last_date,".csv"),row.names = FALSE)


# covid_all_orig <- read_csv(paste0(path_c19dashboard_old_folder,"/covid_all.csv"))

covid_all <- bind_rows(
  nyt_national ,
  nyt_states,
  nyt_counties 
  
) %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(date == "2020-12-13")

# daily cases data -----
library(zoo)

county_daily <- nyt_counties %>% 
  arrange(fips,date) %>% 
  group_by(state,county) %>% 
  mutate(
         dailydeaths = case_when(date == min(date) ~ deaths,
                                 deaths < dplyr::lag(deaths) ~ 0,
                                 TRUE ~ deaths - dplyr::lag(deaths)),
         dailycases = case_when(date == min(date) ~ cases,
                                cases < dplyr::lag(cases) ~ 0,
                                TRUE ~ cases - dplyr::lag(cases))
         ) %>% 
  ungroup() %>% 
  arrange(fips,date) %>% 
  group_by(state,county) %>% 
  mutate(mean7daydeaths = rollapply(deaths,7,mean,partial=TRUE,align="r"),
         mean7daycases = rollapply(cases,7,mean,partial=TRUE,align="r"),
         ) %>% 
  mutate(percent14dayCases = case_when(is.na(dplyr::lag(cases,14)) ~ NA_real_,
                                       TRUE ~ 100*(cases/dplyr::lag(cases,14) - 1)),
         percent14dayDailyCases = case_when(is.na(dplyr::lag(mean7daycases,14)) ~ NA_real_,
                                            TRUE ~ 100*(mean7daycases/dplyr::lag(mean7daycases,14) - 1)),
         
         percent14dayDeaths = case_when(is.na(dplyr::lag(deaths,14)) ~ NA_real_,
                                       TRUE ~ 100*(deaths/dplyr::lag(deaths,14) - 1)),
         percent14dayDailyDeaths = case_when(is.na(dplyr::lag(mean7daydeaths,14)) ~ NA_real_,
                                            TRUE ~ 100*(mean7daydeaths/dplyr::lag(mean7daydeaths,14) - 1))) %>% 
  ungroup()


# states_daily -----------

state_daily <- nyt_states %>% 
  arrange(fips,date) %>% 
  group_by(state) %>% 
  mutate(
    dailydeaths = case_when(date == min(date) ~ deaths,
                            deaths < dplyr::lag(deaths) ~ 0,
                            TRUE ~ deaths - dplyr::lag(deaths)),
    dailycases = case_when(date == min(date) ~ cases,
                           cases < dplyr::lag(cases) ~ 0,
                           TRUE ~ cases - dplyr::lag(cases))
  ) %>% 
  ungroup() %>% 
  arrange(fips,date) %>% 
  group_by(state) %>% 
  mutate(mean7daydeaths = rollapply(deaths,7,mean,partial=TRUE,align="r"),
         mean7daycases = rollapply(cases,7,mean,partial=TRUE,align="r"),
  ) %>% 
  mutate(percent14dayCases = case_when(is.na(dplyr::lag(cases,14)) ~ NA_real_,
                                       TRUE ~ 100*(cases/dplyr::lag(cases,14) - 1)),
         percent14dayDailyCases = case_when(is.na(dplyr::lag(mean7daycases,14)) ~ NA_real_,
                                            TRUE ~ 100*(mean7daycases/dplyr::lag(mean7daycases,14) - 1)),
         
         percent14dayDeaths = case_when(is.na(dplyr::lag(deaths,14)) ~ NA_real_,
                                        TRUE ~ 100*(deaths/dplyr::lag(deaths,14) - 1)),
         percent14dayDailyDeaths = case_when(is.na(dplyr::lag(mean7daydeaths,14)) ~ NA_real_,
                                             TRUE ~ 100*(mean7daydeaths/dplyr::lag(mean7daydeaths,14) - 1))) %>% 
  ungroup()

# us_daily -----------

us_daily <- nyt_national %>% 
  arrange(date) %>% 
  mutate(
    dailydeaths = case_when(date == min(date) ~ deaths,
                            deaths < dplyr::lag(deaths) ~ 0,
                            TRUE ~ deaths - dplyr::lag(deaths)),
    dailycases = case_when(date == min(date) ~ cases,
                           cases < dplyr::lag(cases) ~ 0,
                           TRUE ~ cases - dplyr::lag(cases))
  ) %>% 
  arrange(date) %>% 
  mutate(mean7daydeaths = rollapply(deaths,7,mean,partial=TRUE,align="r"),
         mean7daycases = rollapply(cases,7,mean,partial=TRUE,align="r"),
  ) %>% 
  mutate(percent14dayCases = case_when(is.na(dplyr::lag(cases,14)) ~ NA_real_,
                                       TRUE ~ 100*(cases/dplyr::lag(cases,14) - 1)),
         percent14dayDailyCases = case_when(is.na(dplyr::lag(mean7daycases,14)) ~ NA_real_,
                                            TRUE ~ 100*(mean7daycases/dplyr::lag(mean7daycases,14) - 1)),
         
         percent14dayDeaths = case_when(is.na(dplyr::lag(deaths,14)) ~ NA_real_,
                                        TRUE ~ 100*(deaths/dplyr::lag(deaths,14) - 1)),
         percent14dayDailyDeaths = case_when(is.na(dplyr::lag(mean7daydeaths,14)) ~ NA_real_,
                                             TRUE ~ 100*(mean7daydeaths/dplyr::lag(mean7daydeaths,14) - 1)))

# mean7day_current ----------

mean7day_current <- bind_rows(
  us_daily,
  state_daily,
  county_daily
)  %>% 
  dplyr::filter(date == Sys.Date()-1) %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::select(date,state,county,nation,
                dailydeaths,mean7daydeaths,
                dailycases,mean7daycases
                ) %>%
  
  # CHECK - What is this for? =============
  mutate(zero = 0)

# covidtimeseries -----------
covidtimeseries <- bind_rows(
  us_daily,
  state_daily,
  county_daily
) %>% 
  dplyr::filter(date >= "2020-04-01") %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::select(date,state,county,nation,
                deaths,cases,
                dailydeaths,mean7daydeaths,
                dailycases,mean7daycases
  )
