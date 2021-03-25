
# Daily average covid test positivity
# Variables:
# - Viral (RT-PCR) lab test positivity rate - last 7 days 
# - RT-PCR tests per 100k - last 7 days 

# Geography:
# - States: GA ()
# - Counties: Douglas County, GA ()
# - National: YES


file_dates <- sapply(raw_files,function(x) str_extract(x,"[0-9]+") %>% lubridate::ymd(.))
raw_files <- raw_files[!is.na(file_dates)]
latest_file <- raw_files[which.max(file_dates)]

gr01_df <- bind_rows(
  readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/national_df_clean.RDS")) %>%
    dplyr::filter(file_name == latest_file) %>% 
    mutate(state = NA,
           national = 1,
           county = NA) %>%
    rename(viral_positivity_rate_7day = N08,
           rtpcr_per100k_7day = N10) %>%
    dplyr::select(national,state,county,N01,viral_positivity_rate_7day,rtpcr_per100k_7day) %>% 
    rename(date = N01),
  readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/states_df_clean.RDS")) %>%
    dplyr::filter(S02 == "GA") %>%
    mutate(state = 13,
           county = NA) %>%
    rename(viral_positivity_rate_7day = S21,
           rtpcr_per100k_7day = S23) %>%
    dplyr::select(state,county,date_of_file,viral_positivity_rate_7day,rtpcr_per100k_7day) %>% 
    mutate(date = date_of_file),
  readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/counties_df_clean.RDS")) %>%
    dplyr::filter(V02 == 13097) %>%
    mutate(state = substr(V02,1,2) %>% as.numeric(),
           county = substr(V02,3,5) %>% as.numeric()) %>%
    rename(viral_positivity_rate_7day = V33,
           rtpcr_per100k_7day = V35) %>%
    dplyr::select(state,county,date_of_file,viral_positivity_rate_7day,rtpcr_per100k_7day) %>% 
    mutate(date = date_of_file)
  
)

# Figure -----------
gr01_df %>% 
  arrange(date) %>% 
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


