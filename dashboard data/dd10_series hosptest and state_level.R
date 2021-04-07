#  ADDING PERCENT POSITIVE DATA TO HOSPTEST DATA --------




library(cdlTools)
positive <- read.csv("https://covidtracking.com/data/download/all-states-history.csv") %>%
  dplyr::select(date,state,positive,negative,recovered,hospitalizedCumulative,hospitalizedCurrently,totalTestResults) %>%
  dplyr::rename(stateabb = state,
                hospTot=hospitalizedCumulative,
                hospDaily=hospitalizedCurrently,
                totaltests=totalTestResults) %>%
  dplyr::filter(!stateabb %in% c("AS","PR","GU","VI","MP"),!is.na(stateabb)) %>% 
  mutate(state = fips(stateabb),
         nation = NA_real_,
         county = NA_real_,
         date = lubridate::ymd(as.character(date)),
         percentPositive=round((positive/totaltests)*100,digits=2),
         below10pctPositive= ifelse(percentPositive<=10,"Yes","No")
         ) %>% 
  left_join(cdlTools::stateNames %>% 
              dplyr::select(-STATEFP) %>% 
              rename(statename = STATENAME),
            by=c("stateabb"="STATE")) %>% 
  arrange(desc(date),state) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), -1))

# Remove Alaska, California, Texas
final_hosptest_ts_pre <- positive %>% 
  dplyr::filter(!statename %in% c("Alaska","California","Texas"))



# merged ------------
# PENDING: What are we joining on -------
# Joining, by = c("date", "hospTot", "statename") is incorrect since hospTot may not match up
# This would default to 'positive' as the source

# premerge <- positive1 %>% #positive with some pre-processing
#   dplyr::filter(statename %in% c("Alaska","California","Texas"))

# Original code
# merge <- full_join(alaska,California) %>%
#   #full_join(Texas2) %>%
#   mutate(date=as.Date(date,origin = "1899-12-30")) %>%
#   select(date,statename,hospTot) %>%
#   filter(!is.na(hospTot))

alaska <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/alaska.RDS"))
California <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/California.RDS"))
texas <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/texas.RDS"))

merged <- left_join(
  positive %>% 
    
    dplyr::filter(statename %in% c("Alaska","California","Texas")) %>% 
    dplyr::select(-hospTot),
  # QUERY for 'Claire_dataUpdate2.R': Why are we not including Texas here? 
  # RESOLVED: Updated code in 'code_Jithin.R'
  bind_rows(alaska,
            California,
            texas),
  by = c("date","statename")
  
)

# final_hosptest_ts ------
# full_join(final_hosptest_ts_pre,merged) will give the same results
# - technically incorrect since joins are usually reserved for two distinct datasets with overlapping columns
final_hosptest_ts <- bind_rows(final_hosptest_ts_pre,merged)


final_hosptest_static <- final_hosptest_ts %>% 
  dplyr::filter(date==max(date)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), -1))

# SAVE ------------
saveRDS(final_hosptest_ts,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/series_hosptest.RDS"))
saveRDS(final_hosptest_static,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/state_level.RDS"))


write.csv(final_hosptest_ts,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/series_hosptest.csv"),row.names = FALSE)
write.csv(final_hosptest_static,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Hospitalizations and testing/state_level.csv"),row.names = FALSE)

