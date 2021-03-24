# ***************************************************************;
# *CDC diabetes Atlas data;
# ***************************************************************;

diabetescounty0 <- haven::read_sas(paste0(path_c19dashboard_old_folder,"/CDC Diabetes Surveillance/county.sas7bdat")) %>% 
  
  dplyr::rename(state = Fips) %>% 
  mutate(fips = sprintf("%05d",CountyFIPS)) %>% 
  mutate(county = substr(fips,3,5) %>% as.numeric()) %>% 
  dplyr::select(-County,-State,-CountyFIPS,-fips) %>% 
  dplyr::filter(!state %in% c(66,69,72,78))


diabetesstate0 <- diabetescounty0 %>% 
  group_by(state) %>% 
  summarize_at(vars(diabetes,obesity),~mean(.,na.rm=TRUE)) %>% 
  ungroup() 

diabetesnation0 <- diabetescounty0 %>% 
  summarize_at(vars(diabetes,obesity),~mean(.,na.rm=TRUE)) %>% 
  mutate(nation = 1)

CDCdiabetes <- bind_rows(
  diabetesnation0,
  diabetesstate0,
  diabetescounty0
)


