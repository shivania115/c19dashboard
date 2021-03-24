df_clean <- readRDS(paste0(path_cpr_folder,"/states/df_clean.RDS"))

df_clean %>%
  dplyr::filter(date == 2020-12-17) %>% 
  head()
