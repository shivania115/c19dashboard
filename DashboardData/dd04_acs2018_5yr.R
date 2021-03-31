# *************************************************************;
# * Date: May 2020
# * Data: American Community Survey 2014-2018 5-Year Estimates, Nation, State and County
# * Data Source: Social explorer 
# *************************************************************;

# acs2018_5yr_county ----------

acs2018_5yr_county = haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/ACS 2018 5yr/acs2018_5yr_county.sas7bdat")) %>%
  dplyr::rename(fips = FIPS) %>% 
  mutate(popden = A00002_002,
         age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100,
         male      = A02001_002 / A00001_001 * 100,
         female    = A02001_003 / A00001_001 * 100,
         black     = A04001_004 / A00001_001 * 100,  
         hispanic  = A04001_010 / A00001_001 * 100,
         minority = (A00001_001-A04001_003)/ A00001_001 * 100,
         natives =   A03001_004/ A00001_001 * 100,
         groupquater = A19001_002/A00001_001 * 100,
         college =     A12001_005/A12001_001 * 100,
         hhincome = A14008_001,
         poverty = A13005_002 / A13005_001 * 100, 
         
         county=(substr(fips,3,5)) %>% as.numeric(),
         state=(substr(fips,1,2)) %>% as.numeric()) %>% 
  dplyr::select(county,state,fips,popden,
                age65over,male,female,
                black,hispanic,minority,natives,
                groupquater,college,hhincome,poverty)


# acs2018_5yr_state ------

acs2018_5yr_state = haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/ACS 2018 5yr/acs2018_5yr_state.sas7bdat")) %>%
  dplyr::rename(fips = FIPS) %>% 
  mutate(popden = A00002_002,
         age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100,
         male      = A02001_002 / A00001_001 * 100,
         female    = A02001_003 / A00001_001 * 100,
         black     = A04001_004 / A00001_001 * 100,  
         hispanic  = A04001_010 / A00001_001 * 100,
         minority = (A00001_001-A04001_003)/ A00001_001 * 100,
         natives =   A03001_004/ A00001_001 * 100,
         groupquater = A19001_002/A00001_001 * 100,
         college =     A12001_005/A12001_001 * 100,
         hhincome = A14008_001,
         poverty = A13005_002 / A13005_001 * 100, 
         
         county=NA_real_,
         state=(substr(fips,1,2)) %>% as.numeric()) %>% 
  dplyr::select(county,state,fips,popden,
                age65over,male,female,
                black,hispanic,minority,natives,
                groupquater,college,hhincome,poverty)

# acs2018_5yr_nation --------

acs2018_5yr_nation = haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/ACS 2018 5yr/acs2018_5yr_nation.sas7bdat")) %>%
  dplyr::rename(fips = FIPS) %>% 
  mutate(popden = A00002_002,
         age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100,
         male      = A02001_002 / A00001_001 * 100,
         female    = A02001_003 / A00001_001 * 100,
         black     = A04001_004 / A00001_001 * 100,  
         hispanic  = A04001_010 / A00001_001 * 100,
         minority = (A00001_001-A04001_003)/ A00001_001 * 100,
         natives =   A03001_004/ A00001_001 * 100,
         groupquater = A19001_002/A00001_001 * 100,
         college =     A12001_005/A12001_001 * 100,
         hhincome = A14008_001,
         poverty = A13005_002 / A13005_001 * 100, 
         
         county=NA_real_,
         state=NA_real_,
         nation = 1) %>% 
  dplyr::select(county,state,nation,fips,popden,
                age65over,male,female,
                black,hispanic,minority,natives,
                groupquater,college,hhincome,poverty)

# acs2018_5yr_all ----------

acs2018_5yr_all <- bind_rows(
  acs2018_5yr_nation,
  acs2018_5yr_state,
  acs2018_5yr_county
) %>% 
  dplyr::filter(!state %in% c(66,69,72,78))

# SAVE ---------

saveRDS(acs2018_5yr_all,paste0(path_c19dashboard_shared_folder,"/Data/Processed/ACS 2018 5yr/acs2018_5yr_all.RDS"))
write.csv(acs2018_5yr_all,paste0(path_c19dashboard_shared_folder,"/Data/Processed/ACS 2018 5yr/acs2018_5yr_all.csv"),row.names = FALSE)
write.csv(head(acs2018_5yr_all,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/ACS 2018 5yr/EXAMPLE_acs2018_5yr_all.csv"),row.names = FALSE)
