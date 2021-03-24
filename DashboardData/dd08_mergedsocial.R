

source(paste0(path_c19dashboard_repo,"/DashboardData/dd04_acs2018_5yr.R"))
source(paste0(path_c19dashboard_repo,"/DashboardData/dd05_svi2018_us_county.R"))
source(paste0(path_c19dashboard_repo,"/DashboardData/dd06_CDC diabetes surveillance.R"))
source(paste0(path_c19dashboard_repo,"/DashboardData/dd07_SAIHE.R"))


mergedsocial <- acs2018_5yr_all %>% 
  full_join(SVI %>% dplyr::select(-fips),
            by = c("nation","state","county")) %>% 
  full_join(CDCdiabetes,
            by=c("nation","state","county")) %>% 
  full_join(sahie_2018_merge,
            by=c("nation","state","county")) %>% 
  dplyr::filter(!is.na(nation)|!is.na(state)|!is.na(county))

