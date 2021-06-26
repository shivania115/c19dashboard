library(httr)
library(RCurl)
library(RJSONIO)

racdemog <- 
  jsonlite::fromJSON("https://covid.cdc.gov/covid-data-tracker/Content/CoronaViewJson_01/us_demographics.json")[1] %>%
  data.frame() %>% 
  rename_all(~str_replace(.,".*_percent.", "")) %>% 
  select(race_eth_new,col_per_Grand_Total) %>% 
  dplyr::rename(percentPop=col_per_Grand_Total)

population_all <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/population_all.RDS"))  
# US -------------

deathdata <- jsonlite::fromJSON("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=demographic_charts")[[7]] %>% 
  data.frame() %>% 
  left_join(racdemog,
            by=c("race_eth_new")) %>% 
  mutate(race_eth_new = case_when(race_eth_new == "White, Non-Hispanic" ~ "Non-Hispanic White",
                                  race_eth_new == "Black, Non-Hispanic"~"Non-Hispanic African American",
                                  race_eth_new == "American Indian / Alaska Native, Non-Hispanic" ~ "Non-Hispanic American Native",
                                  race_eth_new == "Asian, Non-Hispanic" ~ "Non-Hispanic Asian",
                                  race_eth_new == "Native Hawaiian / Other Pacific Islander, Non-Hispanic" ~ "Non-Hispanic NHPI",
                                  race_eth_new == "Multiple/Other, Non-Hispanic"~"Non-Hispanic Multiple/Other",
                                  race_eth_new == "Hispanic/Latino" ~ "Hispanic",
                                  TRUE ~ "Unknown")) %>% 
  select(Demographic,race_eth_new,Grand_Total,col_per_Grand_Total,percentPop) %>%
  dplyr::rename(demographicVar=Demographic,
                demographic=race_eth_new,
                deaths=Grand_Total,
                percentDeaths=col_per_Grand_Total) %>% 
  mutate(totaldeaths = sum(deaths),
         demographicVar="race",
         missingDeaths = case_when(demographic == "Unknown" ~ ceiling(deaths*100/totaldeaths),
                                   TRUE ~ NA_real_),
         availableDeaths = case_when(demographic == "Unknown" ~ floor((1-(deaths/totaldeaths))*100),
                                     TRUE ~ NA_real_))

# STATE -----------
stateDeath <-read.csv("https://data.cdc.gov/api/views/pj7m-y5uh/rows.csv?accessType=DOWNLOAD") %>% 
  dplyr::filter(Group == "By Total") %>% 
  dplyr::select(Data.as.of,State,Indicator,contains("Hispanic")) %>% 
  pivot_longer(cols=matches("(Hispanic|Other)"),
               names_to="race_ethnicity",values_to="percent") %>% 
  mutate(Indicator = case_when(Indicator == "Unweighted distribution of population (%)" ~ "pctPop",
                               Indicator == "Distribution of COVID-19 deaths (%)" ~ "pctCovidDeaths",
                               Indicator == "Count of COVID-19 deaths" ~ "countCovidDeaths",
                               Indicator == "Weighted distribution of population (%)" ~ "pctwtPop",
                               TRUE ~ NA_character_),
         
         race_ethnicity = str_replace_all(race_ethnicity,"\\."," ")) %>% 
  mutate(race_ethnicity = case_when(race_ethnicity == "Non Hispanic American Indian or Alaska Native" ~ "Non Hispanic American Native",
                                    race_ethnicity == "Non Hispanic Native Hawaiian or Other Pacific Islander" ~ "Non Hispanic NHPI",
                                    TRUE ~ race_ethnicity)) %>% 
  pivot_wider(names_from="Indicator",values_from="percent") %>% 
  mutate(state_fips = cdlTools::fips(State)) %>% 
  left_join(population_all %>% 
              dplyr::filter(is.na(county)),
            by = c("state_fips"="state")) %>% 
  mutate(race_pop = (pctPop/100)*population) %>% 
  mutate(death_rate = (countCovidDeaths/race_pop)*100000*(12/16))

write.csv(stateDeath,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/State Death Data/qc_stateDeath.csv"),row.names = FALSE)

# COUNTY ------


countydeathsrace <- read.csv("https://data.cdc.gov/api/views/k8wy-p9cg/rows.csv?accessType=DOWNLOAD") %>%
  dplyr::select(Data.as.of,State,County.Name,FIPS.State,FIPS.County,
                COVID.19.Deaths, Total.deaths,
                Indicator,
                contains("Hispanic"),
                contains("Other")
                ) %>% 
  # mutate_at(vars(matches("(Hispanic|Other)")),function(x) round(x*100,1)) %>% 
  pivot_longer(cols=matches("(Hispanic|Other)"),
               names_to="race_ethnicity",values_to="percent") %>% 
  mutate(Indicator = case_when(Indicator == "Distribution of all-cause deaths (%)" ~ "pctAllcauseDeaths",
                               Indicator == "Distribution of COVID-19 deaths (%)" ~ "pctCovidDeaths",
                               Indicator == "Distribution of population (%)" ~ "pctPop",
                               TRUE ~ NA_character_),
         
         race_ethnicity = str_replace_all(race_ethnicity,"\\."," ")) %>% 
  mutate(race_ethnicity = case_when(race_ethnicity == "Non Hispanic American Indian or Alaska Native" ~ "Non Hispanic American Native",
                                    race_ethnicity == "Non Hispanic Native Hawaiian or Other Pacific Islander" ~ "Non Hispanic NHPI",
                                    TRUE ~ race_ethnicity)) %>% 
  pivot_wider(names_from="Indicator",values_from="percent") %>% 
  left_join(population_all,
            by=c("FIPS.State"="state",
                 "FIPS.County"="county")) %>% 
  mutate(deaths = COVID.19.Deaths*pctCovidDeaths,
         race_pop = pctPop*population) %>% 
  group_by(State,FIPS.State,race_ethnicity) %>% 
  summarize(deaths = sum(deaths,na.rm=TRUE),
            race_pop = sum(race_pop,na.rm=TRUE))
  


