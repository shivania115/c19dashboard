
# Used to create 'mean7daycurrent' ---------
# Please change to appropriate date
current_date = "2021-10-11"

# up to line 500 - Covidtimeseries_pop ---------------

view_data = FALSE # Used in dd03_covidtimeseries00.R 
source(paste0(path_c19dashboard_repo,"/dashboard data/dd01_nytimes.R"))
source(paste0(path_c19dashboard_repo,"/dashboard data/dd03_covidtimeseries00.R"))
rm(covidtimeseries,county_daily,nyt_counties)

# From dd02_urbancodes.R ---------
urbancodes3 <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/urbancodes3.RDS"))


covidtimeseries_pop <- covidtimeseries_pop  %>% 
  rename_at(vars(matches("_fig")),
            function(x) str_replace(x,"_fig","fig")) %>%
  left_join(urbancodes3 %>% 
              dplyr::select(state,county,Urbanization_2013,
                            Urbanization_Code_2013,urbanrural,
                            annualdeaths2018,annualmortality2018,
                            crude_rate),
            by = c("state","county"))  %>% 
  dplyr::rename(
    # countyname = county_name,
    # countycode = county_code,
    # CHECK Are these being used ---------
    X_2013_Urbanization = Urbanization_2013,
    X_2013_Urbanization_Code = Urbanization_Code_2013,
    Population = population,
    Crude_Rate = crude_rate
  )

saveRDS(covidtimeseries_pop,paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/covidtimeseries_pop.RDS"))
write.csv(head(covidtimeseries_pop,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/EXAMPLE_covidtimeseries_pop.csv"),row.names = FALSE)


# up to line 789 - mergedsocial -------------
# covidtimeseries_pop <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/NYT Covid19 data/covidtimeseries_pop.RDS"))

# source(paste0(path_c19dashboard_repo,"/dashboard data/dd08_mergedsocial.R"))


mean7daycurrent <- covidtimeseries_pop %>% 
  dplyr::filter(date == current_date)

nationalraw0 <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/mergedsocial.RDS")) %>% 
  left_join(mean7daycurrent,
            by=c("state","county","nation"))

# SAVE ---------

saveRDS(nationalraw0,paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/nationalraw0.RDS"))
haven::write_sas(nationalraw0,paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/nationalraw0.sas7bdat"))
write.csv(nationalraw0,paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/nationalraw0.csv"),row.names = FALSE)
write.csv(head(nationalraw0,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/EXAMPLE_nationalraw0.csv"),row.names = FALSE)


# Missing columns in mergedsocial ------
# [1] "X1"          "VAR1"        "County_Code" "countyname"  "countynum"   "countycode"  "statecode"   "statename"   "zero"   