
# Plot weekly covid deaths by race for states of GA and MI
# Note last date of death recorded

qfhf_uhaa_cleaned <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Excess Deaths/Weekly_counts_of_deaths_by_jurisdiction_and_race_and_Hispanic_origin_cleaned.RDS"))

gr02_df <- qfhf_uhaa_cleaned %>% 
  dplyr::filter(week_ending_date >= "2020-01-01",state_abbreviation %in% c("GA","MI")) %>% 
  dplyr::filter(outcome == "COVID-19",type == "Unweighted")


# Example Figure -----------
gr02_plot <- gr02_df %>% 
  arrange(week_ending_date) %>% 
  ggplot(data=.,aes(x=as.Date(week_ending_date),y=number_of_deaths,col=state_abbreviation)) +
  geom_line() +
  facet_wrap(~race_ethnicity) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_discrete("") +
  xlab("Week Ending Date") +
  ylab("Number of deaths (unweighted)") +
  scale_x_date(labels = date_format("%b %d %Y"))

write.csv(gr02_df,paste0(path_c19dashboard_shared_folder,"/Dashboard Features/Excess Deaths by Race/gr02_state observed covid deaths by race.csv"),row.names = FALSE)

ggsave(gr02_plot,
       filename = paste0(path_c19dashboard_shared_folder,"/Dashboard Features/Excess Deaths by Race/gr02_state observed covid deaths by race.png"),
       width = 15,height=8,units="in")

