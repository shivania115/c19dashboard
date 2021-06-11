
path_emory_covid19_repo <- "https://raw.githubusercontent.com/yubin-park/emory-covid19/"


file_list <- c(
  "VaccineTimeseries.json","allstates.json","contristates.json",
  "data.json","date.json","fips2county.json","nationalBar.json",
  "nationalBarChart.json","nationalDemogdata.json","nationalDemogdate.json",
  "racedataAll.json","stateVaccineData.json","timeseriesAll.json",
  "topTenCases.json","topTenMortality.json","topten.json",
  "vaccRaceState.json","vaccRaceStatedate.json","vaccineData.json",
  "vaccineDisparity.json",
  "vaccine_site_geocoded.json","vaccinedate.json",
  "vaccinesite_0301_cleaned.json"
)

colnames_file_list <- map_dfr(file_list,function(f){
  
  df = rjson::fromJSON(file=paste0(path_emory_covid19_repo,"master/public/data/",f),simplify = TRUE);
  vec_depth_df = vec_depth(df);

  depth <- list();
  d = 1;
  print(f);
  while((d+2) <= vec_depth_df){
    depth[d] = length(attr(df,"names"))
    df = df[[1]]
    d = d + 1
  }
  
  colnames_f = as.data.frame(df) %>% colnames(.) %>% as.character(.);
  
  return(data.frame(file = f,
                    depth = vec_depth_df,
                    depth_list = paste0(depth,collapse=";"),
                    colnames = colnames_f)
         )
})

map(file_list,function(f){
  colnames_file_list %>% 
    dplyr::filter(file == f) %>% 
    xlsx::write.xlsx(.,paste0(path_c19dashboard_shared_folder,"/Data/Data Check/emory-covid19/colnames of json files from emory-covid19 repository.xlsx"),
                     sheetName = f,row.names = FALSE,append = TRUE);
  print(f)})

  
  



