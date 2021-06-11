
source(paste0(path_c19dashboard_repo,"/package/qc_functions.R"))

# SAS ----------
line210_final_hosptest_ts <- read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Data Check/Claire_dataUpdate2/line210_final_hosptest_ts.csv")) 

qc_df <- line210_final_hosptest_ts

# R ----------

source(paste0(path_c19dashboard_repo,"/dashboard data/dd10_series hosptest and state_level.R"))

r_df <- final_hosptest_ts

# 0. Check date range -----------
summary(qc_df$date)
summary(r_df$date)

# 1. Number of rows ---------------   

check_overall <- row_check_total(r_df,qc_df)
check_by_state <- row_check_state(r_df,qc_df)
check_by_county <- row_check_county(r_df,qc_df)

# Inference: 
rqc01_check1 <- check_by_state %>% 
  dplyr::filter(qc == 2) %>% 
  group_by(state) %>% 
  tally()

write.csv(rqc01_check1,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/rqc01_check1.csv"))

# 2. Variable names ------------

colnames(r_df)
colnames(qc_df)

# There are differences between the two. Especially with how "_fig" is added as suffix. Not correcting this as of now.

# 3. Format of date ----------------    

str(qc_df$date)
str(r_df$date)



# 4. Summaries of all variables (N, N missing, Mean, SD, Min, Max) ----------------    

summary_overall <- summary_check_total(r_df,
                                       qc_df %>% 
                                         distinct(date,state,county,.keep_all = TRUE),
                                       r_cols=c("positive","negative",
                                                "recovered","hospTot",
                                                "hospDaily","totaltests",
                                                "percentPositive"),
                                       df2_cols=c("positive","negative",
                                                  "recovered","hospTot",
                                                  "hospDaily","totaltests",
                                                  "percentPositive")
)

summary_state <- summary_check_state(r_df,
                                     qc_df %>% 
                                       distinct(date,state,county,.keep_all = TRUE),
                                     r_cols=c("positive","negative",
                                              "recovered","hospTot",
                                              "hospDaily","totaltests",
                                              "percentPositive"),
                                     df2_cols=c("positive","negative",
                                                "recovered","hospTot",
                                                "hospDaily","totaltests",
                                                "percentPositive")
)


rqc01_check4 <- summary_state %>% 
  dplyr::filter(r!=qc) %>% 
  dplyr::select(state,fn,name,qc,r) %>% 
  mutate(pct_difference = round(((qc - r)*100)/qc,2))


write.csv(rqc01_check4,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/rqc01_check4.csv"))

# 5. Equivalence of random subset ----------

View(qc_df %>% dplyr::filter(state==19)) 
View(r_df %>% dplyr::filter(state==19)) 


View(qc_df %>% dplyr::filter(state==42) %>% dplyr::select(date,one_of(c("positive","negative",
                                                                                               "recovered","hospTot",
                                                                                               "hospDaily","totaltests",
                                                                                               "percentPositive")))) 
View(r_df %>% dplyr::filter(state==42)%>% dplyr::select(date,one_of(c("positive","negative",
                                                                                 "recovered","hospTot",
                                                                                 "hospDaily","totaltests",
                                                                                 "percentPositive")))) 







