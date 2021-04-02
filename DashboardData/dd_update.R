
# Saves nothing
source(paste0(path_c19dashboard_repo,"/DashboardData/dd01_nytimes.R"))

# No need to run dd02_urbancodes.R since it's a cross-sectional dataset 
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd02_urbancodes.R")) 

# Saves covidtimeseries00 (.RDS)
source(paste0(path_c19dashboard_repo,"/DashboardData/dd03_covidtimeseries00.R"))


# No need to run from dd04_ to dd08_ since they are cross-sectional datasets 
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd04_acs2018_5yr.R"))
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd05_svi2018_us_county.R"))
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd06_CDC diabetes surveillance.R"))
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd07_SAIHE.R"))
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd08_mergedsocial.R"))

# dd09_dataUpdate: To be run, but the datasets are not used going forward 
# Saves datasets for alaska, California, columbia, michigan, texas (.RDS) from state sources
# Now have transitioned to using CPR only
source(paste0(path_c19dashboard_repo,"/DashboardData/dd09_dataUpdate.R"))

# dd10_series hosptest and state_level: Uses files from dataUpdate (for alaska, California, texas) to update data from covidtracking.com
# Not used  going forward
# Saves datasets: series_hosptest (.RDS,.csv) and state_hosplevel (.RDS,.csv)
source(paste0(path_c19dashboard_repo,"/DashboardData/dd10_series hosptest and state_level.R"))


# dd11_merging with covidtimeseries: Creates merged_covidtimeseries
# This is an interim dataset which is used to create covidtimeseries
# Saves datasets: merged_covidtimeseries (.RDS)
# CHECK: It contains data from 2021-03-08 from CPR only? ----------
source(paste0(path_c19dashboard_repo,"/DashboardData/dd11_merging with covidtimeseries.R"))


# dd12_hospitalization_historic.R: No need to run this since it's an OLD time series dataset
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd12_hospitalization_historic.R"))

# dd13_final_merged_covidtimeseries.R: Creates covidtimeseries (.RDS,.csv)
source(paste0(path_c19dashboard_repo,"/DashboardData/dd13_final_merged_covidtimeseries.R"))






