df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/states_df_clean.RDS"))

df_clean %>%
  dplyr::filter(date == 2020-12-17) %>% 
  head()
