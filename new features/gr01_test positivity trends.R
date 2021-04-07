
# Daily average covid test positivity
# Variables:
# - Viral (RT-PCR) lab test positivity rate - last 7 days 
# - RT-PCR tests per 100k - last 7 days 

# Geography:
# - States: GA ()
# - Counties: Douglas County, GA ()
# - National: YES

raw_files <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Community Profile Reports"))
file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
raw_files <- raw_files[!is.na(file_dates)]
latest_file <- raw_files[which.max(file_dates)]
folder_name = paste0("/",as.Date(max(file_dates),origin = "1970-01-01"),"/")


# Number of counties with test positivity data -----------

counties_df <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"counties_df_clean.RDS"))

# View(counties_df %>% 
#        dplyr::select(V01:V02,date_of_file,V33) %>% 
#        dplyr::filter(!is.na(V33)) %>% 
#        group_by(V01,V02) %>% 
#        summarize(date_min = min(date_of_file),
#                  date_max = max(date_of_file)) %>% 
#        ungroup() %>% group_by(date_min) %>% tally())

counties_df %>%
  rename(viral_positivity_rate_7day = V33,
         rtpcr_per100k_7day = V35) %>%
  dplyr::select(state,county,date_of_file,viral_positivity_rate_7day,rtpcr_per100k_7day) %>% 
  mutate(date = date_of_file) %>% 
  group_by(state,county) %>% 
  dplyr::summarize(n_available = sum(!is.na(viral_positivity_rate_7day))) %>% 
  ggplot(data=.,aes(x=n_available)) +
  geom_histogram()


# Example Data ------------
gr01_df <- bind_rows(
  readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"national_df_clean.RDS")) %>%
    dplyr::filter(file_name == latest_file) %>% 
    mutate(state = NA,
           national = 1,
           county = NA) %>%
    rename(viral_positivity_rate_7day = N08,
           rtpcr_per100k_7day = N10) %>%
    dplyr::select(national,state,county,N01,viral_positivity_rate_7day,rtpcr_per100k_7day,date_of_file) %>% 
    rename(date = N01),
  readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"states_df_clean.RDS")) %>%
    # dplyr::filter(S02 == "GA") %>%
    rename(viral_positivity_rate_7day = S21,
           rtpcr_per100k_7day = S23) %>%
    dplyr::select(state,county,date_of_file,viral_positivity_rate_7day,rtpcr_per100k_7day) %>% 
    mutate(date = date_of_file),
  readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports",folder_name,"counties_df_clean.RDS")) %>%
    # dplyr::filter(V02 == 13097) %>%
    rename(viral_positivity_rate_7day = V33,
           rtpcr_per100k_7day = V35) %>%
    dplyr::select(state,county,date_of_file,viral_positivity_rate_7day,rtpcr_per100k_7day) %>% 
    mutate(date = date_of_file)
  
)

# Example Figure -----------
gr01_df %>% 
  arrange(date) %>% 
  dplyr::filter(national == 1 |(state == 13 & is.na(county))| (state==13 & county == 97)) %>% 
  dplyr::filter(date >= "2020-12-01") %>% 
  mutate(region = case_when(national == 1 ~ 1,
                            state == 13 & is.na(county) ~ 2,
                            state == 13 & !is.na(county) ~ 3)) %>% 
  mutate(region = factor(region,labels=c("USA","Georgia","Douglas County, GA"))) %>% 
  ggplot(data = .,aes(x=as.Date(date),y=viral_positivity_rate_7day*100,col=region)) +
  geom_path()+
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "top") +
  scale_color_discrete("") + 
  # https://stackoverflow.com/questions/11748384/formatting-dates-on-x-axis-in-ggplot2
  scale_x_date(labels = date_format("%b %d %Y"))

write.csv(gr01_df,paste0(path_c19dashboard_shared_folder,"/Dashboard Features/Test positivity trends/gr01_test positivity trends.csv"),row.names = FALSE)

# ts_example <- read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Upload/covidtimeseries.csv"))




