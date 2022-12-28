setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

folder1 = "../experimentResult/RQ1/"
folder2 = "../experimentResult/RQ1-seed10/"

scenario = 'DelayedCVIdeal'
fold = '5Fold'
eva = 'BasicClfPerEva'
postfix = "detail"
folds = 0:4
interval = 1000

pat = paste(scenario,fold,eva,postfix,sep='_')

indicator = 'Recall for class 1 (percent)'

files1 = list.files(path = folder1,pattern = pat)
files2 = list.files(path = folder2,pattern = pat)

first_flag = TRUE

for(file in files2){
  df1 = read.csv(file.path(folder1,file),check.names = FALSE)
  df2 = read.csv(file.path(folder2,file),check.names = FALSE)
  for(fold in folds){
    value1 = as.numeric(df1[df1$fold==fold,indicator])
    value2 = as.numeric(df2[df2$fold==fold,indicator])
    counter = 1
    while(counter*interval <= length(value1) && counter*interval <= length(value2)){
      v1 = value1[interval*counter]
      v2 = value2[interval*counter]
      counter = counter+1
      if(first_flag){
        first_flag = FALSE
        total_res = data.frame(matrix(nrow = 0,ncol = 6))
        colnames(total_res)=c('dataset','fold','#instances','indicator',folder1,folder2)
      }
      total_res[nrow(total_res)+1,] = c(file,fold,interval*counter,indicator,v1,v2)
    }
  }
}
