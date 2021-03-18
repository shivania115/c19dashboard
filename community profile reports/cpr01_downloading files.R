library(tidyverse)
library(haven)
library(rvest)
library(httr)
url = "https://beta.healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9"
# # cpr_url = GET("https://beta.healthdata.gov/api/views/gqxm-d9w9.json?method=opening")
# 
cpr_url <- read_html(url) %>%
  # html_nodes(css="attachment")
  html_nodes(xpath='//script[@type="text/javascript"]') %>% 
  .[7]
# https://beta.healthdata.gov/api/views/gqxm-d9w9/files/58495215-54c1-4513-8daf-b9cdc42f8a71?download=true&filename=Community_Profile_Report_20210315_Public.xlsx
a <- cpr_url %>% 
  html_text()

b = str_split(a[[1]],'","') %>% 
  data.frame( .) 

names(b) <- c("var")

b <- b %>% 
  dplyr::filter(regexpr("^href",var)>0) %>% 
  mutate(var = str_replace(var,'href":"',"")) %>% 
  mutate(file_name = str_match(var, "filename\\s*(.*?)\\s*(\\.xlsx|.pdf)")) %>% 
  dplyr::filter(str_detect(var,".xlsx"))

# Download files ----------

for (f in 1:nrow(b)){
# for (f in 1:3){
  file_name = str_replace(b$file_name[f],pattern = "filename=","")
  
  download.file(paste0("https://beta.healthdata.gov/",b$var[f]),
                destfile = paste0(path_cpr_folder,"/raw/",file_name),mode = "wb")
  
  
}


    