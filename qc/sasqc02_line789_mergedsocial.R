
source(paste0(path_c19dashboard_repo,"/package/qc_functions.R"))

# SAS ----------
sas789_mergedsocial <- read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Data Check/DashboardData_07.07.2020_Claire/sas789_mergedsocial.csv")) 

# R ----------

source(paste0(path_c19dashboard_repo,"/dashboard data/dd08_mergedsocial.R"))

sas_df = sas789_mergedsocial
r_df = mergedsocial

# 1. Number of rows ---------------   

# check_overall <- row_check_total(r_df,sas_df,grouping_vars = "")
check_by_state <- row_check_state(r_df,sas_df,grouping_vars= c("state"))
check_by_county <- row_check_county(r_df,sas_df,grouping_vars= c("state","county"))

# Inference: 
sasqc02_check1 <- check_by_county %>% 
  dplyr::filter(sas == 2)

write.csv(sasqc02_check1,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/sasqc02_check1.csv"))

# 2. Variable names ------------

colnames(r_df)
colnames(sas_df)

colnames(sas_df)[!colnames(sas_df) %in% colnames(r_df)]
colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)]

# mergedsocial in SAS has many variables from Covidtimeseries_pop and 2 variables from urbancodes3.RDS
# urbancodes3.RDS gets merged only in dd13_final_merged_covidtimeseries.R

# 3. Format of date ----------------    

str(sas_df$date)
str(r_df$date)



# 4. Summaries of all variables (N, N missing, Mean, SD, Min, Max) ----------------    

summary_overall <- summary_check_total(r_df,
                                       sas_df %>% 
                                         distinct(date,state,county,.keep_all = TRUE),
                                       r_cols=colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)],
                                       sas_cols=colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)]
)

summary_state <- summary_check_state(r_df,
                                     sas_df %>% 
                                       distinct(date,state,county,.keep_all = TRUE),
                                     r_cols=colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)],
                                     sas_cols=colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)]
)

summary_county <- summary_check_county(r_df,
                                       sas_df %>% 
                                         distinct(date,state,county,.keep_all = TRUE),
                                       r_cols=colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)],
                                       sas_cols=colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)]
)

sasqc02_check4 <- summary_state %>% 
  dplyr::filter(r!=sas) %>% 
  bind_rows(summary_county %>% dplyr::filter(r!=sas)) %>% 
  dplyr::select(state,county,fn,name,sas,r) %>% 
  mutate(pct_difference = round(((sas - r)*100)/sas,2))

write.csv(sasqc02_check4,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/sasqc02_check4.csv"))

# 5. Equivalence of random subset ----------

View(sas_df %>% dplyr::filter(state==19,county==1)) 
View(r_df %>% dplyr::filter(state==19,county==1)) 
# 1. starts from 2021-03-08 with dailydeaths = 30
# 2. 2021-03-10 has cases = 820 in R while it has cases = 818 in SAS --> definitely an error in SAS since I checked with NYT data

View(sas_df %>% dplyr::filter(state==42,county==53) %>% dplyr::select(one_of(colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)]))) 
View(r_df %>% dplyr::filter(state==42,county==53)%>% dplyr::select(one_of(colnames(sas_df)[colnames(sas_df) %in% colnames(r_df)]))) 

# 1. Number of cases and deaths are fine
# 2. Initial values for caserate and mean7daycases, deathrate and mean7daydeaths are spilling over from previous







