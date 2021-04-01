

box1 = "/Users/poojanaik/Box Sync/COVID19_data_shared" 

ccvi_states <- openxlsx::read.xlsx("https://covid-static-assets.s3.amazonaws.com/US-CCVI/ccvi-US.xlsx",sheet=1) %>% 
  dplyr::rename(statename=stateName,state=FIPS) %>%
  mutate(statename=str_to_title(statename), countyname="", countycode=NA)

statecode <- ccvi_states %>% select(statename,state)

ccvi_county <- openxlsx::read.xlsx("https://covid-static-assets.s3.amazonaws.com/US-CCVI/ccvi-US.xlsx",sheet=2) %>%
  dplyr::rename(statename=stateName,countyname=countyName,countycode=FIPS) %>%
  mutate(statename=str_to_title(statename)) %>% join(statecode)

CVI <- full_join(ccvi_county,ccvi_states) %>% select(-countyname)


CVI$statename[!is.na(CVI$countycode)] <- ""

setwd(box1)
nationalraw <- read.csv("./DataUpload/Yubin/nationalraw.csv") 

CVImerged <- join(nationalraw,CVI)

setwd(box1)
write.csv(CVImerged,"./DataUpload/Yubin/nationalraw.csv",row.names=FALSE,na="")


# keep the global environment clean
rm(ccvi_states,statecode,ccvi_county,CVI,nationalraw)
