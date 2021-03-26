# Source: Claire_dataUpdate2.R
library(haven)
library(rvest)
library(httr)

# ALASKA -----------

alaska <- read.csv("https://opendata.arcgis.com/datasets/797cbc3e398241a2b11e76fc06dd2b8b_0.csv") %>%
  select(Hospitalized_Cases__Cumulative_,Date_Reported) %>% 
  dplyr::rename(hospTot = Hospitalized_Cases__Cumulative_,
                date = Date_Reported) %>%
  mutate(date = as.Date(date), statename = "Alaska")

# CALIFORNIA -----------

California <- read.csv("https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv") %>%
  select(county,todays_date,hospitalized_covid_patients,all_hospital_beds,icu_available_beds,icu_covid_confirmed_patients) %>%
  dplyr::rename(hospTot= hospitalized_covid_patients,
                allBeds=all_hospital_beds,
                icuBeds=icu_available_beds,
                icuAdmDaily=icu_covid_confirmed_patients,
                date = todays_date) %>%
  mutate(countyname = paste(county,"County, CA", sep=" "),
         date = as.Date(date),
         state = 6) %>%
  select(-county) %>%
  arrange(desc(date),countyname) %>% 
  group_by(date) %>% 
  summarize_at(vars(hospTot,allBeds,icuBeds,icuAdmDaily),~sum(.,na.rm=TRUE)) %>% 
  mutate(statename = "California")


# MICHIGAN -----------

michigan_url <- "https://www.michigan.gov/coronavirus/0,9753,7-406-98159-523641--,00.html"

mi_tables <- read_html(michigan_url) %>% 
  html_nodes(.,"table") %>% 
  html_table(.) %>% 
  map(.,.f=function(x) janitor::clean_names(x))


mi_captions <- read_html(michigan_url) %>% 
  html_nodes(.,"caption") %>%
  map(.,function(x) case_when(length(html_nodes(x,"caption"))==0 ~ html_text(x),
                              TRUE ~ html_node(x,"caption") %>% html_text(.))) %>% 
  map(.,function(x)str_replace_all(x,"(\\r|\\n|\\t|\\*)","")[[1]]) %>% 
  unlist() %>% 
  data.frame(caption = .) %>% 
  mutate(date = str_extract(caption,"\\s([(0-9|/)])+") %>% lubridate::mdy(.),
         text = str_replace(caption,"\\s([(0-9|/)])+",""))

# michiganhosp = table 6 --> not used further
# michiganbeds = table 1 --> row Total; col allBeds, inpatientBedOccupancy, icuBeds, icuBedsOccupancy,ventilators,ventilatorsInUse
# michiganhospt = table 2 --> col Total; row adultCOVIDHosp,icuAdmDaily,pedsCOVIDHosp

michigan <- bind_cols(
  mi_tables[[1]] %>% 
    mutate(statename = "Michigan") %>% 
    dplyr::filter(x == "Total") %>% 
    rename(allBeds = hospital_beds,
           inpatientBedOccupancy = hospital_inpatient_bed_occupancy,
           icuBeds = icu_beds,
           icuBedsOccupancy = icu_bed_occupancy,
           ventilators = total_ventilators,
           ventilatorsInUse = mechanical_ventilators_in_use) %>% 
    dplyr::select(statename,allBeds, inpatientBedOccupancy, icuBeds, icuBedsOccupancy,ventilators,ventilatorsInUse) %>% 
    mutate_at(vars(-statename),function(x) str_replace(x,",","") %>% as.numeric(.)),
  
  mi_tables[[2]] %>%
    dplyr::select(hcc_region,total) %>%
    mutate(hcc_region = str_replace_all(hcc_region,"(\\r|\\n|\\t|\\*)","")) %>%
    
    pivot_wider(names_from = hcc_region,values_from=total) %>%
    
    
    rename(adultCOVIDHosp = 'Adult Confirmed-PositiveCOVID',
           pedsCOVIDHosp = 'Hospitalized PedConfirmed-Positive',
           icuAdmDaily = 'ICU Adult Confirmed-Positive COVID')
) %>% 
  dplyr::mutate(date = mi_captions[1,]$date)


# DISTRICT OF COLUMBIA ------------------
columbiadatezip = format(Sys.Date()-1, '%m-%d-%Y')
dc_url = paste("https://coronavirus.dc.gov/sites/default/files/dc/sites/coronavirus/page_content/attachments/DC-COVID-19-Data-for-",columbiadate,".xlsx",sep="")

dc_hosp <- openxlsx::read.xlsx(dc_url,sheet="Overal Stats") %>% 
  rename(domain = "X1",
         indicator = "X2") %>% 
  dplyr::filter(!is.na(domain)) %>% 
  dplyr::filter(indicator %in% c("Total COVID-19 Patients in DC Hospitals",
                                 "Total COVID-19 Patients in ICU",
                                 "ICU Beds Available")) %>% 
  mutate_at(vars(-one_of(c("domain","indicator"))),~as.numeric(.)) %>% 
  pivot_longer(cols=matches("[0-9]+"),names_to="date",values_to="value") %>% 
  # https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r/62334132
  mutate(date = as.Date(as.numeric(date),origin="1899-12-30")) %>% 
  mutate(var = case_when(indicator == "Total COVID-19 Patients in DC Hospitals" ~ "hospTot",
                         indicator == "Total COVID-19 Patients in ICU" ~ "icuAdm",
                         indicator == "ICU Beds Available" ~ "icuBeds",
                         TRUE ~ NA_character_)) %>% 
  dplyr::select(-indicator,-domain) %>% 
  pivot_wider(names_from="var",values_from="value")

dc_tests <- openxlsx::read.xlsx(dc_url,sheet="Overal Stats") %>% 
  rename(indicator = "X1") %>% 
  dplyr::select(-X2) %>% 
  dplyr::filter(indicator %in% c("Total Submitted for Testing",
                                 "Total Positives")) %>% 
  mutate_at(vars(-one_of("indicator")),~as.numeric(.)) %>% 
  pivot_longer(cols=matches("[0-9]+"),names_to="date",values_to="value") %>% 
  # https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r/62334132
  mutate(date = as.Date(as.numeric(date),origin="1899-12-30")) %>% 
  mutate(var = case_when(indicator == "Total Submitted for Testing" ~ "testDaily",
                         indicator == "Total Positives" ~ "positive",
                         TRUE ~ NA_character_)) %>% 
  dplyr::select(-indicator) %>% 
  pivot_wider(names_from="var",values_from="value")

columbia <- left_join(dc_hosp,dc_tests,by="date")




# TEXAS ----------------
tx_url <- "https://dshs.texas.gov/coronavirus/TexasCOVID-19HospitalizationsOverTimebyTSA.xlsx"

tx_hosp = openxlsx::read.xlsx(tx_url,sheet="COVID-19 Hospitalizations",startRow=3) %>% 
  dplyr::filter(TSA.ID == "Total") %>% 
  mutate(var = "hospTot") %>% 
  mutate_at(vars(-var,-TSA.ID,-TSA.AREA),~as.numeric(.))
tx_available_beds = openxlsx::read.xlsx(tx_url,sheet="COVID-19 Hospitalizations",startRow=3)%>% 
  dplyr::filter(TSA.ID == "Total")  %>% 
  mutate(var = "hospCum")%>% 
  mutate_at(vars(-var,-TSA.ID,-TSA.AREA),~as.numeric(.))
tx_hospitalizations = openxlsx::read.xlsx(tx_url,sheet="COVID Hospitalizations (%)",startRow=3)%>% 
  dplyr::filter(TSA.ID == "Total") %>% 
  mutate(var = "icuAdm")%>% 
  mutate_at(vars(-var,-TSA.ID,-TSA.AREA),~as.numeric(.))
tx_beds = openxlsx::read.xlsx(tx_url,sheet="COVID-19 ICU",startRow=3)%>% 
  dplyr::filter(TSA.ID == "Total") %>% 
  mutate(var = "icuBeds")%>% 
  mutate_at(vars(-var,-TSA.ID,-TSA.AREA),~as.numeric(.))

texas <- bind_rows(tx_hosp,tx_available_beds,tx_hospitalizations,tx_beds) %>% 
  dplyr::select(-TSA.ID,-TSA.AREA) %>% 
  pivot_longer(cols=-one_of("var"),names_to="date",values_to="value") %>% 
  pivot_wider(names_from="var",values_from="value") %>% 
  mutate(date = case_when(date == "44051" ~ as.character(as.Date(44051,origin="1899-12-30")),
                          TRUE ~ date)) %>% 
  mutate(date = lubridate::ymd(date))


