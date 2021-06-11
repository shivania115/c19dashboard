
source(paste0(path_c19dashboard_repo,"/package/qc_functions.R"))

# SAS ----------
line236_final_hosptest_ts6 <- read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Data Check/Claire_dataUpdate2/line236_final_hosptest_ts6.csv")) 

qc_df <- line236_final_hosptest_ts6

# R ----------

source(paste0(path_c19dashboard_repo,"/dashboard data/dd11_merging with covidtimeseries.R"))

r_df <- final_hosptest_ts4 %>% 
  dplyr::filter((!is.na(state) & is.na(county)),date <= "2021-05-24")

# 0. Check date range -----------
summary(qc_df$date)
summary(r_df$date)

# 1. Number of rows ---------------   

check_overall <- row_check_total(r_df,qc_df)
check_by_state <- row_check_state(r_df,qc_df)
check_by_county <- row_check_county(r_df,qc_df)

# Inference: There are differences
rqc03_check1 <- check_by_state %>% 
  dplyr::filter(qc == 2|is.na(qc)) %>% 
  group_by(date,qc) %>% 
  tally()

write.csv(rqc03_check1,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/rqc03_check1.csv"))

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
                                       r_cols=c("positive","percentPositive",
                                                "negative","recovered","hospTot","hospDaily",
                                                "totaltests"),
                                       df2_cols=c(
                                         "positive","percentPositive",
                                         "negative","recovered","hospTot","hospDaily",
                                         "totaltests")
)

summary_state <- summary_check_state(r_df,
                                     qc_df %>% 
                                       distinct(date,state,county,.keep_all = TRUE),
                                     r_cols=c("positive","percentPositive",
                                              "negative","recovered","hospTot","hospDaily",
                                              "totaltests"),
                                     df2_cols=c("positive","percentPositive",
                                                "negative","recovered","hospTot","hospDaily",
                                                "totaltests")
)


rqc03_check4 <- bind_rows(summary_overall, summary_state) %>% 
  dplyr::filter(r!=qc) %>% 
  dplyr::select(state,fn,name,qc,r) %>% 
  mutate(pct_difference = round(((qc - r)*100)/qc,2))


write.csv(rqc03_check4,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/rqc03_check4.csv"))

# 5. Equivalence of random subset ----------

View(qc_df %>% dplyr::filter(state==19)) 
View(r_df %>% dplyr::filter(state==19)) 


View(qc_df %>% dplyr::filter(state==42) %>% dplyr::select(date,one_of(c("positive","percentPositive",
                                                                        "negative","recovered","hospTot","hospDaily",
                                                                        "totaltests")))) 
View(r_df %>% dplyr::filter(state==42)%>% dplyr::select(date,one_of(c("positive","percentPositive",
                                                                      "negative","recovered","hospTot","hospDaily",
                                                                      "totaltests")))) 







