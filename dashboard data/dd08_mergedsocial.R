

# source(paste0(path_c19dashboard_repo,"/DashboardData/dd04_acs2018_5yr.R"))
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd05_svi2018_us_county.R"))
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd06_CDC diabetes surveillance.R"))
# source(paste0(path_c19dashboard_repo,"/DashboardData/dd07_SAIHE.R"))


acs2018_5yr_all <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/ACS 2018 5yr/acs2018_5yr_all.RDS"))
SVI <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC SVI/SVI.RDS"))
CDCdiabetes <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Diabetes Surveillance/CDCdiabetes.RDS"))
sahie_2018_merge <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/SAHIE/sahie_2018_merge.RDS"))



mergedsocial <- acs2018_5yr_all %>% 
  full_join(SVI %>% dplyr::select(-fips),
            by = c("nation","state","county")) %>% 
  full_join(CDCdiabetes,
            by=c("nation","state","county")) %>% 
  full_join(sahie_2018_merge,
            by=c("nation","state","county")) %>% 
  dplyr::filter(!is.na(nation)|!is.na(state)|!is.na(county))


# mergedsocial is exported as nationalraw0.csv

# pending: Variable labelling  ---------
var_labels_mergedsocial <- read.table(file=paste0(path_c19dashboard_repo,"/dashboard data/aux01_variable labels.txt"),sep="=",
                                      col.names = c("variable","label")) %>% 
  mutate_all(~as.character(.) %>% trimws(.))


library(labelled)
var_label(mergedsocial) <- map(colnames(mergedsocial),
                               .f= function(x){
                                 x_label = var_labels_mergedsocial %>% 
                                   dplyr::filter(variable == x) %>% 
                                   dplyr::select(label) %>% 
                                   pull();
                                 x_label = ifelse(length(x_label) == 0,"",x_label);
                                 return(x_label)
                                 }) %>% unlist()
# SAVE ---------

saveRDS(mergedsocial,paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/mergedsocial.RDS"))
haven::write_sas(mergedsocial,paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/mergedsocial.sas7bdat"))
write.csv(mergedsocial,paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/mergedsocial.csv"),row.names = FALSE)
write.csv(head(mergedsocial,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/mergedsocial/EXAMPLE_mergedsocial.csv"),row.names = FALSE)



