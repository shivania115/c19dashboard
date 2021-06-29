# Includes population by race for state and nation ---------
population_by_race <- readxl::read_excel(paste0(path_c19dashboard_shared_folder,"/Data/Raw/US Census 2019 Population/Census_Single-RacePopulationEstimatesSTATES_2019.xlsx"),
                                         sheet = "Import") %>% 
  mutate(nation = NA) %>% 
  rename(state = 'States Code') %>% 
  bind_rows(readxl::read_excel(paste0(path_c19dashboard_shared_folder,"/Data/Raw/US Census 2019 Population/Census_Single-RacePopulationEstimatesNATION_2019.xlsx"),
                               sheet = "Import") %>% 
              mutate(nation = 1,
                     States = "United States",
                     state = NA)) %>% 
  mutate(race_eth_new = case_when(Ethnicity == "Hispanic or Latino" ~ "Hispanic",
                                  Race == "American Indian or Alaska Native" ~ "Non-Hispanic American Native",
                                  Race == "Asian" ~ "Non-Hispanic Asian",
                                  Race == "Black or African American" ~ "Non-Hispanic African American",
                                  Race == "Native Hawaiian or Other Pacific Islander" ~ "Non-Hispanic NHPI",
                                  Race == "White" ~ "Non-Hispanic White",
                                  Race == "More than one race" ~ "Non-Hispanic Multiple/Other",
                                  TRUE ~ Race)) %>% 
  group_by(nation,States,state,race_eth_new) %>% 
  summarize(Population = sum(Population,na.rm=TRUE)) %>% 
  dplyr::select(nation,States,state,race_eth_new,Population)



# STATE FROM NYTIMES -----------
covidtimeseries <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/covidtimeseries00.RDS")) %>% 
  dplyr::filter(lubridate::year(date) == 2021,is.na(county))  %>%
  dplyr::filter(date <= "2021-06-19") %>% 
  mutate(min_date = min(date),max_date = max(date)) %>% 
  group_by(nation,state,min_date,max_date,population) %>% 
  summarize(deaths = sum(dailydeaths)) %>% 
  ungroup() %>% 
  mutate(death_rate = (deaths/population)*100000*(12/6)) %>% 
  dplyr::select(nation,state,min_date,max_date,deaths,population,death_rate) %>% 
  mutate(State = cdlTools::fips(state,to="Name"))

# write.csv(covidtimeseries,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/State Death Data/qc02_covidtimeseries_",Sys.Date(),".csv"),row.names = FALSE)



# STATE FROM CDC -----------
stateDeath <-read.csv("https://data.cdc.gov/api/views/pj7m-y5uh/rows.csv?accessType=DOWNLOAD") %>% 
  dplyr::filter(Group == "By Year",Year == 2021) %>% 
  dplyr::select(Data.as.of,Start.Date,End.Date,State,Indicator,contains("Hispanic")) %>% 
  pivot_longer(cols=matches("(Hispanic|Other)"),
               names_to="race_ethnicity",values_to="percent") %>% 
  mutate(Indicator = case_when(Indicator == "Unweighted distribution of population (%)" ~ "pctPop",
                               Indicator == "Distribution of COVID-19 deaths (%)" ~ "pctCovidDeaths",
                               Indicator == "Count of COVID-19 deaths" ~ "countCovidDeaths",
                               Indicator == "Weighted distribution of population (%)" ~ "pctwtPop",
                               TRUE ~ NA_character_),
         
         race_ethnicity = str_replace_all(race_ethnicity,"\\."," ")) %>% 
  mutate(race_eth_new = case_when(race_ethnicity == "Hispanic or Latino" ~ "Hispanic",
                                  race_ethnicity == "Non Hispanic American Indian or Alaska Native" ~ "Non-Hispanic American Native",
                                  race_ethnicity == "Non Hispanic Asian" ~ "Non-Hispanic Asian",
                                  race_ethnicity == "Non Hispanic Black or African American" ~ "Non-Hispanic African American",
                                  race_ethnicity == "Non Hispanic Native Hawaiian or Other Pacific Islander" ~ "Non-Hispanic NHPI",
                                  race_ethnicity == "Non Hispanic White" ~ "Non-Hispanic White",
                                  race_ethnicity == "Non Hispanic more than one race" ~ "Non-Hispanic Multiple/Other",
                                  TRUE ~ race_ethnicity)) %>% 
  pivot_wider(names_from="Indicator",values_from="percent") %>% 
  mutate(state_fips = cdlTools::fips(State)) %>% 
  dplyr::filter(State!="New York City") %>% 
  left_join(population_by_race,
            by = c("state_fips"="state","race_eth_new")) %>% 
  mutate(death_rate = (countCovidDeaths/Population)*100000*(12/6))

stateDeath %>% group_by(nation,state_fips,Start.Date,End.Date,State) %>% 
       summarize(cdc_totaldeaths =sum(countCovidDeaths,na.rm=TRUE),
                 census2019_population = sum(Population,na.rm=TRUE)) %>% 
       mutate(death_rate = (cdc_totaldeaths/census2019_population)*100000*(12/6)) %>% 
  left_join(covidtimeseries %>% 
              dplyr::select(min_date,max_date,deaths,population,death_rate,state) %>% 
              rename(nyt_deaths=deaths,
                     nyt_min_date = min_date,
                     nyt_max_date = max_date,
                     nyt_death_rate=death_rate,
                     ref_population=population),
            by=c("state_fips"="state")) %>% 

  
  
write.csv(.,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/State Death Data/qc02_stateDeath summary_",Sys.Date(),".csv"),row.names = FALSE)
