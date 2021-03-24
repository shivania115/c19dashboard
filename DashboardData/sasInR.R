
if(!require(base)){
  install.packages("base")
  library(base)
}

if(!require(haven)){
  install.packages("haven")
  library(haven)
}
#change the directory to the path containing SAS
setwd("c:\\Program Files\\SASHome 9.4\\SASFoundation\\9.4\\")

#input the system command to run the sas program
return.code <- shell("sas.exe -SYSIN c:\\path\\myprogram.sas")

# Import the CSV dataset generated from SAS
myData <- read.csv("c:\\temps\\exported_csv.csv")

#then executes R script
source("filepath.R")