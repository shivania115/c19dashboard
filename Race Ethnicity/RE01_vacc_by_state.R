
## Raw code - needs more work

racedata <- read.csv("https://raw.githubusercontent.com/KFFData/COVID-19-Data/kff_master/Race%20Ethnicity%20COVID-19%20Data/Vaccines/20210405_Distribution%20of%20Vaccinations%2C%20Cases%2C%20Deaths%20and%20Total%20Population%20by%20RaceEthnicity.csv") %>%
  slice(1:51)

racedata2 <- racedata %>% 
  gather("indicator","percent",2:17) %>% 
  select(-Footnotes) %>%
  mutate(race = sub(".Percent.of.*", "", indicator),
         metric= sub(".*.Percent.of.", "", indicator)) %>% select(-indicator) %>%
  spread(metric,percent) 

names(racedata2) <- tolower(names(racedata2))

finaldata <- racedata2 %>%
  dplyr::rename(statename=state,totPop=total.population) 

