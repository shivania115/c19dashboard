
# Pooja's box sync folder path
box = "/Users/poojanaik/Box Sync/COVID19_data_shared"

box2 = "/Users/poojanaik/Box Sync/Dashboard_temp"
# I recommend setting a local directory even when there's 
# going to be no file called from your local directory if you're linked to the box.
local = "/Users/poojanaik/Downloads"



###########  CASE RATE  ############

setwd(box2)
raw <- read.csv("./Yubin/nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  select(countyname,percent14dayDailyCases) %>%
  arrange(desc(percent14dayDailyCases)) %>% slice(1:10) %>% 
  mutate(rank = seq(1:10)) %>% 
  select(-percent14dayDailyCases)


setwd(box2)
data <- read.csv("./Yubin/covidtimeseries.csv") %>% 
  left_join(raw) %>% 
  filter(!rank=="") %>%
  select(date,countyname,rank,caserate7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "caserate7day") %>%
  dplyr::rename(measure = caserate7day)




############  DEATH RATE  ###########

setwd(box2)
death <- read.csv("./Yubin/nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  select(countyname,percent14dayDailyDeaths) %>%
  arrange(desc(percent14dayDailyDeaths)) %>% slice(1:5) %>% 
  mutate(rank = seq(1:5)) %>% 
  select(-percent14dayDailyDeaths)


setwd(box2)
deathraw <- read.csv("./Yubin/covidtimeseries.csv") %>% 
  left_join(death) %>% 
  filter(!rank=="") %>%
  select(date,countyname,rank,covidmortality7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "covidmortality7day") %>%
  dplyr::rename(measure = covidmortality7day)



###########  MERGE DATA #########

merge <- full_join(data,deathraw) %>% select(date,countyname,variable,measure,rank)
setwd(box2)
write.csv(merge,"./Yubin/top10trendlines.csv",row.names=FALSE,na="")




