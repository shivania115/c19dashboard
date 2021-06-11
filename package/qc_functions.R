

row_check_total <- function(r_data,df2_data,grouping_vars = c("date")){
  r_rows = nrow(r_data)
  df2_rows = nrow(df2_data)
  
  r_date_summary <- r_data %>% 
    group_by_at(vars(one_of(grouping_vars))) %>% 
    tally() %>% 
    rename(r = n)
  
  df2_date_summary <- df2_data %>% 
    group_by_at(vars(one_of(grouping_vars))) %>% 
    tally() %>% 
    rename(qc = n)
  
  date_comparison <- full_join(r_date_summary,
                               df2_date_summary,
                               by=grouping_vars)
  
  
  return(date_comparison)
  
}

row_check_state <- function(r_data,df2_data,grouping_vars = c("date","state")){
  
  if(is.null(grouping_vars)){
    grouping_vars = c("date","state")
  }
  
  r_data <- r_data %>% 
    dplyr::filter(!is.na(state),is.na(county))
  
  df2_data <- df2_data %>% 
    dplyr::filter(!is.na(state),is.na(county))
  
  date_comparison <- row_check_total(r_data,df2_data,grouping_vars=grouping_vars)
  
  
  return(date_comparison)
  
}

row_check_county <- function(r_data,df2_data,grouping_vars = c("date","state","county")){
  
  r_data <- r_data %>% 
    dplyr::filter(!is.na(state),!is.na(county))
  
  df2_data <- df2_data %>% 
    dplyr::filter(!is.na(state),!is.na(county))
  
  date_comparison <- row_check_total(r_data,df2_data,grouping_vars)
  
  
  return(date_comparison)
  
}


summary_check_total <- function(r_data,df2_data,r_cols,df2_cols,name_format="{col}__{fn}"){
  
  separator = str_replace_all(name_format,"(\\{col\\}|\\{fn\\})",replacement = "")
  
  r_date_summary <- r_data %>% 
    dplyr::summarize(across(one_of(r_cols),
                        .fns = list(nobs = ~sum(!is.na(.)),
                                     nmiss = ~sum(is.na(.)),
                                     mean = ~mean(.,na.rm=TRUE),
                                     sd = ~sd(.,na.rm=TRUE),
                                     min = ~min(.,na.rm=TRUE),
                                     max = ~max(.,na.rm=TRUE)),
                     .names=name_format)) %>% 
    pivot_longer(cols=everything(),names_to = "variable",values_to="value") %>% 
    separate(variable,into=c("name","fn"),sep=separator)
  
  df2_date_summary <- df2_data %>% 
    dplyr::summarize(across(one_of(df2_cols),
                            .fns = list(nobs = ~sum(!is.na(.)),
                                        nmiss = ~sum(is.na(.)),
                                        mean = ~mean(.,na.rm=TRUE),
                                        sd = ~sd(.,na.rm=TRUE),
                                        min = ~min(.,na.rm=TRUE),
                                        max = ~max(.,na.rm=TRUE)),
                            .names=name_format)) %>% 
    pivot_longer(cols=everything(),names_to = "variable",values_to="value") %>% 
    separate(variable,into=c("name","fn"),sep=separator)
  
  date_comparison <- left_join(r_date_summary %>% 
                                 rename(r = value),
                               df2_date_summary %>% 
                                 rename(qc = value),
                               by=c("name","fn")) %>% 
    dplyr::select(name,fn,everything()) %>% 
    mutate_if(is.numeric,~round(.,1))
  
  
  return(date_comparison)
  
}


summary_check_state <- function(r_data,df2_data,r_cols,df2_cols,name_format="{col}__{fn}"){
  
  separator = str_replace_all(name_format,"(\\{col\\}|\\{fn\\})",replacement = "")
  
  r_date_summary <- r_data %>% 
    dplyr::filter(!is.na(state),is.na(county)) %>% 
    group_by(state) %>% 
    dplyr::summarize(across(one_of(r_cols),
                            .fns = list(nobs = ~sum(!is.na(.)),
                                        nmiss = ~sum(is.na(.)),
                                        mean = ~mean(.,na.rm=TRUE),
                                        sd = ~sd(.,na.rm=TRUE),
                                        min = ~min(.,na.rm=TRUE),
                                        max = ~max(.,na.rm=TRUE)),
                            .names=name_format)) %>% 
    pivot_longer(cols=-one_of("state"),names_to = "variable",values_to="value") %>% 
    separate(variable,into=c("name","fn"),sep=separator)
  
  df2_date_summary <- df2_data %>% 
    dplyr::filter(!is.na(state),is.na(county)) %>% 
    group_by(state) %>% 
    dplyr::summarize(across(one_of(df2_cols),
                            .fns = list(nobs = ~sum(!is.na(.)),
                                        nmiss = ~sum(is.na(.)),
                                        mean = ~mean(.,na.rm=TRUE),
                                        sd = ~sd(.,na.rm=TRUE),
                                        min = ~min(.,na.rm=TRUE),
                                        max = ~max(.,na.rm=TRUE)),
                            .names=name_format)) %>% 
    pivot_longer(cols=-one_of("state"),names_to = "variable",values_to="value") %>% 
    separate(variable,into=c("name","fn"),sep=separator)
  
  date_comparison <- left_join(r_date_summary %>% 
                                 rename(r = value),
                               df2_date_summary %>% 
                                 rename(qc = value),
                               by=c("name","fn","state")) %>% 
    dplyr::select(name,fn,everything()) %>% 
    mutate_if(is.numeric,~round(.,1))
  
  
  return(date_comparison)
  
}

summary_check_county <- function(r_data,df2_data,r_cols,df2_colsname_format="{col}__{fn}"){
  
  separator = str_replace_all(name_format,"(\\{col\\}|\\{fn\\})",replacement = "")
  
  r_date_summary <- r_data %>% 
    dplyr::filter(!is.na(state),!is.na(county)) %>% 
    group_by(state,county) %>% 
    dplyr::summarize(across(one_of(r_cols),
                            .fns = list(nobs = ~sum(!is.na(.)),
                                        nmiss = ~sum(is.na(.)),
                                        mean = ~mean(.,na.rm=TRUE),
                                        sd = ~sd(.,na.rm=TRUE),
                                        min = ~min(.,na.rm=TRUE),
                                        max = ~max(.,na.rm=TRUE)),
                            .names=name_format)) %>% 
    pivot_longer(cols=-one_of(c("state","county")),names_to = "variable",values_to="value") %>% 
    separate(variable,into=c("name","fn"),sep=separator)
  
  df2_date_summary <- df2_data %>% 
    dplyr::filter(!is.na(state),!is.na(county)) %>% 
    group_by(state,county) %>% 
    dplyr::summarize(across(one_of(df2_cols),
                            .fns = list(nobs = ~sum(!is.na(.)),
                                        nmiss = ~sum(is.na(.)),
                                        mean = ~mean(.,na.rm=TRUE),
                                        sd = ~sd(.,na.rm=TRUE),
                                        min = ~min(.,na.rm=TRUE),
                                        max = ~max(.,na.rm=TRUE)),
                            .names=name_format)) %>% 
    pivot_longer(cols=-one_of(c("state","county")),names_to = "variable",values_to="value") %>% 
    separate(variable,into=c("name","fn"),sep=separator)
  
  date_comparison <- left_join(r_date_summary %>% 
                                 rename(r = value),
                               df2_date_summary %>% 
                                 rename(qc = value),
                               by=c("name","fn","state","county")) %>% 
    dplyr::select(name,fn,everything()) %>% 
    mutate_if(is.numeric,~round(.,1))
  
  
  return(date_comparison)
  
}