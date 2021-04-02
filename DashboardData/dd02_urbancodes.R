
# urbancodes3 ------------
urbancodes3 <- readxl::read_excel(paste0(path_c19dashboard_shared_folder,"/Data/Raw/CDC_Urban_Rural/Urban_Rural_2013_Classification.xlsx"),sheet="SAS") %>% 
  dplyr::rename(Urbanization_2013 = '2013 Urbanization',
                Urbanization_Code_2013 = '2013 Urbanization Code',
                county_code = 'County Code',
                county_name = 'County',
                deaths = 'Deaths',
                population = 'Population',
                crude_rate = 'Crude Rate') %>% 
  mutate(fips = sprintf("%05d",county_code),
         annualdeaths2018 = deaths,
         annualmortality2018 = crude_rate
         )%>% 
  dplyr::mutate(state = substr(fips,1,2) %>% as.numeric(.),
                county = substr(fips,3,5) %>% as.numeric(.)) %>% 
  mutate(fips = as.numeric(fips)) %>% 
  mutate(urbanrural = paste0(Urbanization_Code_2013,str_replace_all(Urbanization_2013," ",""))) %>% 
  mutate_at(vars(deaths,population,crude_rate),~as.numeric(.))
  
# statepopulation 3 --------------  
statepopulation3 <- readxl::read_excel(paste0(path_c19dashboard_shared_folder,"/Data/Raw/CDC_Urban_Rural/State_2018_pop.xlsx"),sheet="SAS") %>% 
  dplyr::rename(statename = 'State',
                state = 'State Code',
                deaths = 'Deaths',
                population = 'Population',
                crude_rate = 'Crude Rate'
                )

# nationalpop2 -----------
nationalpop2 <- statepopulation3 %>% 
  summarize_at(vars(deaths,population),~sum(.,na.rm = TRUE)) %>% 
  mutate(state = NA,
         county = NA,
         nation = 1)

# population_all -----------

population_all <- bind_rows(nationalpop2,
                            statepopulation3,
                            urbancodes3) %>% 
  rename(deaths_allcause = deaths) %>% 
  dplyr::select(nation,state,county,deaths_allcause,population)

saveRDS(urbancodes3,paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/urbancodes3.RDS"))
saveRDS(population_all,paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/population_all.RDS"))


