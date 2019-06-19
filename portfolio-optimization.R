rm(list=ls())

diretorioquote = "D:/portfolio-optimization/Quotes";
setwd(diretorioquote);

f = NULL
files = 
  c("ABEV3.SA.csv","B3SA3.SA.csv","BBAS3.SA.csv")

for (i in 1:length(files)) {
  csv = read.csv(files[i])
  csv = csv[,c("Date","Close")]
  names(csv) = c("Date",files[i])
  if (i == 1) f = csv
  else f = merge(f,csv)
}
