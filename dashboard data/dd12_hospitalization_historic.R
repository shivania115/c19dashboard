
#   * Project: RWJF COVID 19 Dashboard
# * Date: May 2020
# * Data: COVID-19 Module Data Dashboard
# * Data Source: COVID-19 Module Data Dashboard: https://www.cdc.gov/nhsn/covid19/report-overview.html
# Code: Hospitalizations and testing.sas

covid19_NatEst <- read.csv("https://www.cdc.gov/nhsn/pdfs/covid19/covid19-NatEst.csv")

write.csv(covid19_NatEst,paste0(path_c19dashboard_shared_folder,"/Data/Raw/CDC Hospital Capacity/covid19-NatEst.csv"),row.names = FALSE)



hospitalization_historic <- covid19_NatEst %>% 
  dplyr::filter(!statename %in% c("State name")) %>% 
  mutate(statename = trimws(statename)) %>% 
  dplyr::select(state,statename,collectionDate,
                InBedsOccAnyPat__Numbeds_Est,
                InBedsOccCOVID__Numbeds_Est,
                ICUBedsOccAnyPat__N_ICUBeds_Est) %>% 
  rename(state_abbreviation = state,
         bedsAll = InBedsOccAnyPat__Numbeds_Est,
         bedsCovid = InBedsOccCOVID__Numbeds_Est,
         bedsICU = ICUBedsOccAnyPat__N_ICUBeds_Est
         ) %>% 
  mutate(state = cdlTools::fips(state_abbreviation),
         county = NA_real_,
         nation = case_when(statename == "United States" ~ 1,
                            TRUE ~ NA_real_)) %>% 
  # Correction for Delaware
  # mutate(state = case_when(statename == "Delaware" ~ 10,
  #                          TRUE ~ state)) %>% 
  
  mutate(date = lubridate::dmy(collectionDate)) %>% 
  dplyr::select(-collectionDate) %>% 
  arrange(state,date)


hospitalization_static <- hospitalization_historic %>% 
  group_by(state) %>% 
  dplyr::filter(date == max(date)) %>% 
  ungroup()

# SAVE hospitalization_historic and hospitalization_static ------------
saveRDS(hospitalization_historic,paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Hospital Capacity/hospitalization_historic.RDS"))
saveRDS(hospitalization_static,paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Hospital Capacity/hospitalization_static.RDS"))


write.csv(head(hospitalization_historic,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Hospital Capacity/EXAMPLE_hospitalization_historic.csv"),
          row.names = FALSE)
write.csv(hospitalization_static,
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Hospital Capacity/hospitalization_static.csv"),
          row.names = FALSE)















