

# Note:
# SAS.L353: population_all.RDS Contains other variables which are not available in R

population_all <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/population_all.RDS"))


# covid_pop -------------

covid_pop <- covid_all %>% 
  dplyr::filter(!is.na(nation)|!is.na(state)|!is.na(county)) %>% 
  full_join(population_all,
            by = c("nation","state","county")) %>% 
  full_join(mean7day_current %>% 
              dplyr::select(nation,state,county,
                            dailycases,dailydeaths,
                            mean7daycases,mean7daydeaths),
            by=c("nation","state","county")) %>% 
  mutate(
    covidmortality = (deaths/population)*(10^5),
    caserate = (cases/population)*(10^5),
    
    covidmortality7day = (mean7daydeaths/population)*(10^5),
    caserate7day = (mean7daycases/population)*(10^5)
    
  ) %>% 
  mutate_at(vars(cases,deaths,
                covidmortality,caserate,
                covidmortality7day,caserate7day),
         .funs=list(fig=~case_when(. == 0 | is.na(.) | . == Inf ~ -1,
                                             TRUE ~ .)),
         .names = "{fn}_{col}")

# data checking covid_pop --------

# if(view_data){
#   (view_covid_pop_summary <- covid_pop %>% 
#     dplyr::select(-date,-nation,-state,-fips,-county) %>% 
#     summarize_all(.funs=function(x) mean(x %in% c(0,NA)))) %>% 
#     View()
# }


# covidtimeseries_pop ---------------
covidtimeseries_pop <- covidtimeseries %>% 
  dplyr::filter(!is.na(nation)|!is.na(state)|!is.na(county)) %>% 
  left_join(population_all,
            by=c("nation","county","state")
) %>% 
  ungroup() %>% 
  mutate(
    covidmortality = (deaths/population)*(10^5),
    caserate = (cases/population)*(10^5),
    cfr = 100*(deaths/cases),
    covidmortality7day = (mean7daydeaths/population)*(10^5),
    caserate7day = (mean7daycases/population)*(10^5)
    
  ) %>% 
  mutate_at(vars(cases,deaths,cfr,
                 covidmortality,caserate,
                 covidmortality7day,caserate7day),
            .funs=list(fig=~case_when(. == 0 | is.na(.) | . == Inf ~ -1,
                                      TRUE ~ .)),
            .names = "{fn}_{col}") %>% 
  dplyr::filter(state != 0 | !(is.na(nation)&is.na(state)&is.na(county)))

saveRDS(covidtimeseries_pop,paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/covidtimeseries00.RDS"))
write.csv(head(covidtimeseries_pop,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/EXAMPLE_covidtimeseries00.csv"),row.names = FALSE)
