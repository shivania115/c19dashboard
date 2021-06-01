
path_emory_covid19_repo <- "https://raw.githubusercontent.com/yubin-park/emory-covid19/"
# https://stackoverflow.com/questions/51333430/how-to-get-public-github-json-into-r-as-a-list-of-lists

variable_mapping_vaccine <- rjson::fromJSON(file=paste0(path_emory_covid19_repo,"master/public/data/rawdata/variable_mapping_Vaccine.json"),simplify = TRUE) %>% 
  map_dfr(.,function(x){
   as.data.frame(x)
  })
variable_mapping <- rjson::fromJSON(file=paste0(path_emory_covid19_repo,"master/public/data/rawdata/variable_mapping.json"),simplify = TRUE) %>% 
  map_dfr(.,function(x){
    as.data.frame(x)
  })

bind_rows(variable_mapping %>% 
            mutate(file = "variable_mapping.json"),
          variable_mapping_vaccine %>% 
            mutate(file = "variable_mapping_vaccine.json")) %>% 
  dplyr::select(file,everything()) %>% 
  xlsx::write.xlsx(.,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/variable mapping from emory-covid19 repository.xlsx"),
                   sheetName = "variable mapping",row.names = FALSE)
