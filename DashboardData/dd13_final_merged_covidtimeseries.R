
# Lines 396 to 448 in code_Jithin.R
# COVID19_data_shared\Hospitalizations and testing\ has hospitalbeds_historic and hospitalization_historic

# covidtimeseries2: Lines 280: 283 contains merged_covidtimeseries and merged_covidtimeseries1 -------
# In dd11_merging with covidtimeseries.R, we did this in the same step
# As a result, we don't have to resort to transmute %>% coalesce %>% left_join

covidtimeseries2 <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/merged_covidtimeseries.RDS"))
hospitalization_historic <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Hospital Capacity/hospitalization_historic.RDS"))
urbancodes3 <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/urbancodes3.RDS"))

final_merged_covidtimeseries <- left_join(covidtimeseries2,
                                          hospitalization_historic %>% 
                                            dplyr::select(-statename),
                                          by = c("date","state","county","nation")) %>% 
  left_join(urbancodes3 %>% 
              dplyr::select(state,county,Urbanization_2013,Urbanization_Code_2013,urbanrural,
                            annualdeaths2018,annualmortality2018,crude_rate) %>% 
              rename(
                     # countyname = county_name,
                     # countycode = county_code,
                     # CHECK Are these being used ---------
                     X_013_Urbanization = Urbanization_2013,
                     X_013_Urbanization_Code = Urbanization_Code_2013
                     ),
            by = c("state","county")) %>% 
  
  mutate_at(vars(cases,deaths,
                 caserate,covidmortality,dailycases,
                 dailydeaths,mean7daycases,mean7daydeaths,
                 totaltests,hospDaily,cfr,
                 testsPer100K, # positivePer100K
                 percentPositive, percentPositiveDaily, # CHECK Ambiguous definition in aux02_states cpr testing.R vs aux03_counties cpr testing.R -------
                 percent7dayhospDaily,
                 # CHECK Where are the below 14 day variables coming from? ------
                 # percent14dayDailyCases, percent14dayDailyDeaths,
                 
                 # CHECK The below are already imputed in dd03_covidtimeseries00.R -------
                 caserate7day_fig,covidmortality7day_fig
                 
                 ), function(x) case_when(is.na(x) ~ -1,
                                          TRUE ~ x))


# Check name duplication
# names(final_merged_covidtimeseries)[!names(final_merged_covidtimeseries) %in% names(urbancodes3)]
# names(urbancodes3)[!names(urbancodes3) %in% names(final_merged_covidtimeseries)]



# SAVE final_merged_covidtimeseries ---------

saveRDS(final_merged_covidtimeseries,paste0(path_c19dashboard_shared_folder,"/Data/Processed/covidtimeseries/covidtimeseries.RDS"))
write.csv(final_merged_covidtimeseries,paste0(path_c19dashboard_shared_folder,"/Data/Processed/covidtimeseries/covidtimeseries.csv"),row.names=FALSE)


write.csv(head(final_merged_covidtimeseries,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/covidtimeseries/EXAMPLE_covidtimeseries.csv"),
          row.names = FALSE)


