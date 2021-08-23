cause_of_death <- read.csv(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Census 2019 Data/Underlying Cause of Death, GA 2019.txt"),sep = "\t") %>% 
  dplyr::rename(state = 'State.Code',
                statename = 'State',
                county = 'County.Code',
                countyname = 'County',
                SingleYearAges = 'Single.Year.Ages',
                SingleYearAgesCode = 'Single.Year.Ages.Code',
                deaths = Deaths,
                population = Population,
                CrudeRate = 'Crude.Rate')

saveRDS(cause_of_death,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Census 2019 Data/Underlying Cause of Death, GA 2019.RDS"))
writexl::write_xlsx(cause_of_death,paste0(path_c19dashboard_shared_folder,"/Data/Processed/Census 2019 Data/Underlying Cause of Death, GA 2019.xlsx"))


