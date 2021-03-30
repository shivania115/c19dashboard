
# Dataset details ---------------
# Identifiers: date, nation, county, state
# Additional: statename, countyname
# Note: All NA in data columns = -1
# positive
# percentPositive
# below10pctPositive
# negative
# recovered
# hospTot
# hospDaily
# totaltests: date range varies by county



raw_files <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports"))
file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
raw_files <- raw_files[!is.na(file_dates)]
latest_file <- raw_files[which.max(file_dates)]
folder_name = paste0("/",as.Date(max(file_dates),origin = "1970-01-01"),"/")


# Number of counties with test positivity data -----------

counties_df <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"counties_df_clean.RDS"))

gr01_df <- counties_df %>%
  dplyr::select(state,county,date_of_file,V01,V03,V06,V33,V34,V56) %>% 
  rename(date = date_of_file,
         countyname = V01,
         Urbanization_2013 = V03,
         state_abbreviation = V06,
         # test positivity rate in last 7 days : V33
         # total RT PCR in last 7 days: V34
         # confirmed COVID-19 admissions - last 7 days: V56
         hospDaily = V56
         ) %>% 
  arrange(state,county,date,countyname) %>% 
  mutate(positivetoday = V33*V34,
         statename = cdlTools::fips(state_abbreviation,to="Name")) %>% 
  group_by(state,county,countyname) %>% 
  
  mutate(V34_imputed = case_when(is.na(V34) ~ 0,
                         TRUE ~ V34),
         positivetoday_imputed = case_when(is.na(positivetoday) ~ 0,
                                           TRUE ~ positivetoday),
         hospDaily_imputed = case_when(is.na(hospDaily) ~ 0,
                                       TRUE ~ hospDaily)) %>% 
  mutate(totaltests = cumsum(V34_imputed),
         
         # final_hosptest_ts3<-final_hosptest_ts3 %>% group_by(statename)%>%  mutate(positivetoday = cumsum(positivetoday))
         totalpositives = cumsum(positivetoday_imputed),
         
         # Hospitalized_Cases__Cumulative_
         hospTot = cumsum(hospDaily_imputed)
         ) %>% 
  mutate(percentPositive = totalpositives*100/totaltests) %>% 
  mutate(below10pctPositive = ifelse(percentPositive <= 10,"Yes","No")) %>% 
  
  # renaming positivetoday --> positive
  rename(positive = positivetoday) %>% 
  
  dplyr::select(date,state,county,statename,countyname,
                positive,percentPositive,below10pctPositive,
                # negative, recovered,
                hospTot, hospDaily, totaltests
                ) %>% 
  mutate_at(vars(positive,percentPositive,hospTot,hospDaily,totaltests),~case_when(is.na(.) ~ -1,
                                                                                   TRUE ~ .))

write.csv(gr01_df,paste0(path_c19dashboard_shared_folder,"/Dashboard Features/Test positivity trends/gr01_PATCH for county level test positivity and hospitalization.csv"),row.names = FALSE)

# How many counties --------
gr01_df %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(county!=0) %>% 
  group_by(state,county) %>% 
  tally() %>% 
  View()

# Number of counties for which positive cases are available among 50 states and DC --------
gr01_df %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(county!=0) %>% 
  dplyr::filter(positive!=-1) %>% 
  group_by(state,county) %>% 
  tally() %>% 
  View()


# Number of counties for which percentPositive is available among 50 states and DC --------
gr01_df %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(county!=0) %>% 
  dplyr::filter(percentPositive!=-1) %>% 
  group_by(state,county) %>% 
  tally() %>% 
  View()

# Number of counties for which hospitalization is available among 50 states and DC --------
gr01_df %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(county!=0) %>% 
  dplyr::filter(hospDaily!=-1) %>% 
  group_by(state,county) %>% 
  tally() %>% 
  View()

# Number of counties for which cumulative hospitalization is available among 50 states and DC --------
gr01_df %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(county!=0) %>% 
  dplyr::filter(hospTot==0) %>% 
  group_by(state,county) %>% 
  tally() %>% 
  dplyr::filter(n == 94) %>% 
  View()





