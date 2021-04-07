
# *******************************************************************;
# *SAIHE Health insurance files;
# *******************************************************************;


sahie_2018 <- haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/SAHIE/sahie_2018_new.sas7bdat")) %>% 
  mutate(county = case_when(countyfips == 0 ~ NA_real_,
                            TRUE ~ countyfips),
         nation = NA_real_
         ) %>% 
  rename(state = statefips) %>% 
  dplyr::select(nation,county,state,PCTUI,PCTIC)

# /*This file is not needed - state is covered in county;
# sahie_2018_state2 <- sahie_2018 %>% 
#   group_by(state) %>% 
#   summarize_at(vars(PCTUI,PCTIC),~mean(.,na.rm=TRUE)) %>% 
#   mutate(nation = 1,
#          county = NA_real_) %>% 
#   ungroup()



sahie_2018_nat2 <- sahie_2018 %>% 
  summarize_at(vars(PCTUI,PCTIC),~mean(.,na.rm=TRUE)) %>% 
  mutate(nation = 1,
         state = NA_real_,
         county = NA_real_)


sahie_2018_merge <- bind_rows(sahie_2018,sahie_2018_nat2)


# SAVE ---------

saveRDS(sahie_2018_merge,paste0(path_c19dashboard_shared_folder,"/Data/Processed/SAHIE/sahie_2018_merge.RDS"))
write.csv(sahie_2018_merge,paste0(path_c19dashboard_shared_folder,"/Data/Processed/SAHIE/sahie_2018_merge.csv"),row.names = FALSE)
write.csv(head(sahie_2018_merge,n=1000),paste0(path_c19dashboard_shared_folder,"/Data/Processed/SAHIE/EXAMPLE_sahie_2018_merge.csv"),row.names = FALSE)



