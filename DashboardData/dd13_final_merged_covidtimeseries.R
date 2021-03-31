
# Lines 396 to 448 in code_Jithin.R
# COVID19_data_shared\Hospitalizations and testing\ has hospitalbeds_historic and hospitalization_historic

merged_covidtimeseries <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/merged_covidtimeseries.RDS"))
hospitalization_historic <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Hospital Capacity/hospitalization_historic.RDS"))
urbancodes3 <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/urbancodes3.RDS"))

final_merged_covidtimeseries <- left_join(merged_covidtimeseries,
                                          hospitalization_historic %>% 
                                            dplyr::select(-statename),
                                          by = c("date","state","county","nation")) %>% 
  left_join(urbancodes3 %>% 
              dplyr::select(state,county,county_name,county_code,Urbanization_2013,Urbanization_Code_2013,urbanrural,
                            annualdeaths2018,annualmortality2018,crude_rate) %>% 
              rename(countyname = county_name,
                     countycode = county_code,
                     # CHECK Are these being used ---------
                     X_013_Urbanization = Urbanization_2013,
                     X_013_Urbanization_Code = Urbanization_Code_2013
                     ),
            by = c("state","county"))


# Check name duplication
# names(final_merged_covidtimeseries)[!names(final_merged_covidtimeseries) %in% names(urbancodes3)]
# names(urbancodes3)[!names(urbancodes3) %in% names(final_merged_covidtimeseries)]



# SAVE final_merged_covidtimeseries ---------

saveRDS(final_merged_covidtimeseries,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/covidtimeseries.RDS"))


write.csv(head(final_merged_covidtimeseries,n=1000),
          paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/EXAMPLE_covidtimeseries.csv"),
          row.names = FALSE)


