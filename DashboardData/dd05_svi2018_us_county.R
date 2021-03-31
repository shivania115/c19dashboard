# *****************************************************************
#   CDC SVI 2018 Documentation - 1/31/2020
# *****************************************************************;

SVI <- haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/CDC SVI/svi2018_us_county.sas7bdat")) %>% 
  mutate(fips = sprintf("%05d",FIPS)) %>% 
  mutate(county = substr(fips,3,5) %>% as.numeric(),
         nation = NA_real_) %>% 
  rename(state = ST) %>% 
  dplyr::filter(!state %in% c(66,69,72,78)) %>% 
  dplyr::select(nation,state,county,fips,RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4)



# SAVE ---------

saveRDS(SVI,paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC SVI/SVI.RDS"))
write.csv(SVI,paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC SVI/SVI.csv"),row.names = FALSE)
write.csv(head(SVI,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC SVI/EXAMPLE_SVI.csv"),row.names = FALSE)



