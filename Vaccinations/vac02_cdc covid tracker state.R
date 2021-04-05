library(RCurl)
library(RJSONIO)

# Saving Data/Raw ----------
vaccination_data_link <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data"
vaccination_data <- jsonlite::fromJSON(vaccination_data_link)[2][[1]]

date <- unique(vaccination_data$Date)

write.csv(vaccination_data,paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/CDC_Covid Data Tracker_State Vaccination_",date,".csv"),row.names = FALSE)
# Need to check if the Vaccination data can be traced into a time series
# As of now, it relies on multiple overwritten datasets
# g = list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations"))
# g = g[regexpr("CDC_Covid Data Tracker_State Vaccination_",g)>0]
# state_vaccination_ts <- map_dfr(g,.f=function(x){read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/",x))}) 


# Reading running datasets -----------

# nationalraw is read from Claire's data
nationalraw <- read.csv(paste0(path_c19dashboard_shared_folder,"/Data/Upload/nationalraw.csv"))

# VaccineTrackertimeseries is from Claire's data 
VaccineTracker <- read.csv(paste0(path_c19dashboard_shared_folder,"/Data/Upload/VaccineTrackertimeseries.csv")) %>% 
  dplyr::select(-Dist_Per_100K_new,-Dist_new,-distDate) %>% 
  mutate(Date = lubridate::ymd(as.character(Date)))

# Cleaning Raw -------

# Error in Line 1173 of Jithin_code.R --------
# names(casesdata)[26] <-"Administered_Dose2"


drop_cols <- names(vaccination_data)[c(6,18:27,29:30,39:46,48:50,53:56)]

# casesdata ----------
casesdata <- vaccination_data %>% 
  rename( percentVaccinatedDose1 = Administered_Dose1_Pop_Pct,
          percentVaccinatedDose2 = Administered_Dose2_Pop_Pct,
          Administered_Dose1 = Administered_Dose1_Recip,
          Administered_Dose2 = Administered_Dose2_Recip,
          statename = LongName,
          state_abbreviation = Location,
          ) %>% 
  dplyr::select(-one_of(drop_cols)) %>% 
  dplyr::filter(!state_abbreviation %in% c("AS","BP2","DD2","FM","GU","IH2","MH","MP",
                                           "PR","RP","VA2","VI","LTC")) %>% 
  mutate(state = cdlTools::fips(state_abbreviation),
         statename = case_when(state_abbreviation == "NY" ~ "New York",
                               TRUE ~ statename)) %>% 
  left_join(nationalraw %>% 
              dplyr::filter(!is.na(state) & is.na(county)) %>% 
              dplyr::select(state,Population) %>% 
              bind_rows(data.frame(state = NA,
                                   Population = sum(.$Population))),
            by = "state") %>% 
  mutate(FIPS = case_when(state_abbreviation == "US" ~ "_nation",
                          TRUE ~ as.character(state))) %>% 
  # MERGE TODAY WITH PREVIOUS VACCINE DATA (CUMULATIVE) ------
  mutate(AdministeredPartial = Administered_Dose1 - Series_Complete_Yes,
         # CHECK: Why is this different from percentVaccinatedDose2 - percentVaccinatedDose1 -------
         PercentAdministeredPartial = AdministeredPartial*100/Census2019 %>% round(.,1),
         
         #CHECK: Line 1201 of Jithin_code.R has possible multiplication by 100 missing
         percentReceived = Doses_Administered*100/Doses_Distributed %>% round(.,1),
         
  )  %>% 
  mutate(Date = lubridate::ymd(as.character(Date)))
  

VaccineTracker0 <- bind_rows(VaccineTracker,
                             casesdata) %>% 
  arrange(statename,Date) %>% 
  # Possibly unnecessary
  mutate(FIPS = case_when(FIPS %in% c("-1","") ~ "_nation",
                          TRUE ~ FIPS)) %>% 
  group_by(statename) %>% 
  mutate(
         Dist_Per_100K_new = case_when(Dist_Per_100K < dplyr::lag(Dist_Per_100K) ~ 0,
                                       TRUE ~ Dist_Per_100K - dplyr::lag(Dist_Per_100K,default = 0)),
         
         Dist_new = case_when(Doses_Distributed < dplyr::lag(Doses_Distributed) ~ 0,
                              TRUE ~ Dist_Per_100K - dplyr::lag(Dist_Per_100K,default = 0))
         )
  
  # CHECK: Lines 1240-1248: Why is this done?
  # Setting lag(,1) for distribution of vaccines

  
saveRDS(VaccineTracker0,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/VaccineTracker0.RDS"))
write.csv(head(VaccineTracker0,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/Vaccinations/EXAMPLE_VaccineTracker0.csv"),row.names=FALSE)


  
