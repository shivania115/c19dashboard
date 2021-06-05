
source(paste0(path_c19dashboard_repo,"/package/qc_functions.R"))

# SAS ----------
sas500_Covidtimeseries_pop <- read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Data Check/DashboardData_07.07.2020_Claire/sas500_Covidtimeseries_pop.csv")) 

sas_df <- sas500_Covidtimeseries_pop %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  dplyr::filter(!is.na(date))



# R ----------

source(paste0(path_c19dashboard_repo,"/dashboard data/dd01_nytimes.R"))
source(paste0(path_c19dashboard_repo,"/dashboard data/dd03_covidtimeseries00.R"))

r_df <- covidtimeseries_pop %>% 
  dplyr::filter(date <= "2021-06-01")

# 0. Check date range -----------
summary(sas_df$date)
summary(r_df$date)

# 1. Number of rows ---------------   

check_overall <- row_check_total(r_df,sas_df)
check_by_state <- row_check_state(r_df,sas_df)
check_by_county <- row_check_county(r_df,sas_df)

# Inference: 
sasqc01_check1 <- check_by_county %>% 
       dplyr::filter(sas == 2) %>% 
       group_by(state,county) %>% 
       tally()

write.csv(sasqc01_check1,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/sasqc01_check1.csv"))

# 2. Variable names ------------

colnames(r_df)
colnames(sas_df)

# There are differences between the two. Especially with how "_fig" is added as suffix. Not correcting this as of now.

# 3. Format of date ----------------    

str(sas_df$date)
str(r_df$date)



# 4. Summaries of all variables (N, N missing, Mean, SD, Min, Max) ----------------    

summary_overall <- summary_check_total(r_df,
                                       sas_df %>% 
                                         distinct(date,state,county,.keep_all = TRUE),
                                       r_cols=c("cases","deaths",
                                                "dailycases","dailydeaths",
                                                "mean7daycases","mean7daydeaths",
                                                "covidmortality","caserate",
                                                "covidmortality7day","caserate7day"),
                                       sas_cols=c("cases","deaths",
                                                  "dailycases","dailydeaths",
                                                  "mean7daycases","mean7daydeaths",
                                                "covidmortality","caserate",
                                                "covidmortality7day","caserate7day")
                                       )

summary_state <- summary_check_state(r_df,
                                       sas_df %>% 
                                         distinct(date,state,county,.keep_all = TRUE),
                                       r_cols=c("cases","deaths",
                                                "dailycases","dailydeaths",
                                                "mean7daycases","mean7daydeaths",
                                                "covidmortality","caserate",
                                                "covidmortality7day","caserate7day"),
                                       sas_cols=c("cases","deaths",
                                                  "dailycases","dailydeaths",
                                                  "mean7daycases","mean7daydeaths",
                                                  "covidmortality","caserate",
                                                  "covidmortality7day","caserate7day")
)

summary_county <- summary_check_county(r_df,
                                       sas_df %>% 
                                         distinct(date,state,county,.keep_all = TRUE),
                                       r_cols=c("cases","deaths",
                                                "dailycases","dailydeaths",
                                                "mean7daycases","mean7daydeaths",
                                                "covidmortality","caserate",
                                                "covidmortality7day","caserate7day"),
                                       sas_cols=c("cases","deaths",
                                                  "dailycases","dailydeaths",
                                                  "mean7daycases","mean7daydeaths",
                                                  "covidmortality","caserate",
                                                  "covidmortality7day","caserate7day")
)

sasqc01_check4 <- summary_state %>% 
  dplyr::filter(r!=sas) %>% 
  bind_rows(summary_county %>% dplyr::filter(r!=sas)) %>% 
  dplyr::select(state,county,fn,name,sas,r) %>% 
  mutate(pct_difference = round(((sas - r)*100)/sas,2))


write.csv(sasqc01_check4,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/dashboard data/sasqc01_check4.csv"))

# 5. Equivalence of random subset ----------

View(sas_df %>% dplyr::filter(state==19,county==1)) 
View(r_df %>% dplyr::filter(state==19,county==1)) 
# 1. starts from 2021-03-08 with dailydeaths = 30
# 2. 2021-03-10 has cases = 820 in R while it has cases = 818 in SAS --> definitely an error in SAS since I checked with NYT data

View(sas_df %>% dplyr::filter(state==42,county==53) %>% dplyr::select(date,countyname,one_of(c("cases","deaths",
                                                                                    "dailycases","dailydeaths",
                                                                                    "mean7daycases","mean7daydeaths",
                                                                                    "covidmortality","caserate",
                                                                                    "covidmortality7day","caserate7day")))) 
View(r_df %>% dplyr::filter(state==42,county==53)%>% dplyr::select(date,one_of(c("cases","deaths",
                                                                                 "dailycases","dailydeaths",
                                                                                 "mean7daycases","mean7daydeaths",
                                                                                 "covidmortality","caserate",
                                                                                 "covidmortality7day","caserate7day")))) 

# 1. Number of cases and deaths are fine
# 2. Initial values for caserate and mean7daycases, deathrate and mean7daydeaths are spilling over from previous







