
file_list <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Upload/"))


colnames_file_list <- map_dfr(file_list,
                              function(x){
                                print(x);
                                if(str_detect(x,".json",negate=TRUE)){
                                  colnames_f = readr::read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Upload/",x)) %>% 
                                    colnames() %>% 
                                    data.frame(variable = .) %>% 
                                    mutate(dataset = x) %>% 
                                    dplyr::select(dataset,everything());
                                  
                                  
                                }
                                
                                if(str_detect(x,".json",negate=FALSE)){
                                  
                                  df = rjson::fromJSON(file=paste0(path_c19dashboard_shared_folder,"/Data/Upload/",x),simplify = TRUE);
                                  vec_depth_df = vec_depth(df);
                                  
                                  depth <- list();
                                  d = 1;
                                  
                                  while((d+2) <= vec_depth_df){
                                    depth[d] = length(attr(df,"names"))
                                    df = df[[1]]
                                    d = d + 1
                                  }
                                  
                                  colnames_f = df %>% 
                                    as.data.frame() %>% 
                                    colnames() %>% 
                                    data.frame(variable = .) %>% 
                                    mutate(dataset = x,
                                           depth = vec_depth_df,
                                           depth_list = paste0(depth,collapse=";")) %>% 
                                    dplyr::select(dataset,variable,everything())
                                  
                                }
                                
                                return(colnames_f)
                                
                              })



map(file_list,function(f){
  
  variables = c("dataset","variable");
  
  if(str_detect(f,".json")){
    variables = c(variables, "depth","depth_list")
  };
  
  colnames_file_list %>% 
    dplyr::filter(dataset == f) %>% 
    dplyr::select(one_of(variables)) %>% 
    mutate(variable_label = "") %>% 
    xlsx::write.xlsx(.,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/emory-covid19/colnames of files from OneDrive Data Upload.xlsx"),
                     sheetName = f,row.names = FALSE,append = TRUE);
  # print(f)
  })

