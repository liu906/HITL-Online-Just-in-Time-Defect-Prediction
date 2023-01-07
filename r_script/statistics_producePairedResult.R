# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

producePairedResult <- function(folder1,folder2){
  # pat = paste(scenario,fold,eva,postfix,sep='_')
  # files1 = list.files(path = folder1,pattern = pat)
  # files2 = list.files(path = folder2,pattern = pat)
  
  
  pat = paste(scenario,fold,eva,postfix,sep='_')
  files1 <- list.files(path = folder1)
  files1 <- files1[endsWith(files1,paste(pat,'.csv',sep=''))]
  files2 <- list.files(path = folder2)
  files2 <- files2[endsWith(files2,paste(pat,'.csv',sep=''))]
  
  first_flag = TRUE
  
  for(file in files2){
    df1 = read.csv(file.path(folder1,file),check.names = FALSE)
    df2 = read.csv(file.path(folder2,file),check.names = FALSE)
    
    # if(nrow(df1) / nrow(df2)<0.99 || nrow(df1) / nrow(df2)>1.1){
    #   cat(folder1,file.path(folder2,file),'\n')  
    #   cat(nrow(df1) / nrow(df2),'\n')
    # }
    
    for(indicator in indicators){
      for(fold in folds){

        value1 = as.numeric(df1[as.numeric(df1$fold)==fold,indicator])
        value2 = as.numeric(df2[as.numeric(df2$fold)==fold,indicator])
        counter = 1
        while(counter*interval <= length(value1) && counter*interval <= length(value2)){
          v1 = value1[interval*counter]
          v2 = value2[interval*counter]
          
          if(first_flag){
            first_flag = FALSE
            total_res = data.frame(matrix(nrow = 0,ncol = 7))
            colnames(total_res)=c('scenario','dataset','fold','#instances','indicator',folder1,folder2)
          }
          total_res[nrow(total_res)+1,] = c(scenario,file,fold,interval*counter,indicator,v1,v2)
          counter = counter+1
        }
      }
    }

  }


  res_file = paste('paired_performance','_',folder1,'_',folder2,'_',pat,'.csv',sep = '')
  write.csv(total_res,file = file.path('RQ3',res_file))

}



output_files <- function(){
  folder1 = "RQ1-seed2"
  folder2 = "RQ1-seed3"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed4"
  folder2 = "RQ1-seed5"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed6"
  folder2 = "RQ1-seed7"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed8"
  folder2 = "RQ1-seed9"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed10"
  folder2 = "RQ1-seed11"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed2"
  folder2 = "RQ1-seed2-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed3"
  folder2 = "RQ1-seed3-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed4"
  folder2 = "RQ1-seed4-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed5"
  folder2 = "RQ1-seed5-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed6"
  folder2 = "RQ1-seed6-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed7"
  folder2 = "RQ1-seed7-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed8"
  folder2 = "RQ1-seed8-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed9"
  folder2 = "RQ1-seed9-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed10"
  folder2 = "RQ1-seed10-noise0.1"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed11"
  folder2 = "RQ1-seed11-noise0.1"
  producePairedResult(folder1,folder2)
  
  
  folder1 = "RQ1-seed2"
  folder2 = "RQ1-seed2-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed3"
  folder2 = "RQ1-seed3-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed4"
  folder2 = "RQ1-seed4-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed5"
  folder2 = "RQ1-seed5-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed6"
  folder2 = "RQ1-seed6-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed7"
  folder2 = "RQ1-seed7-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed8"
  folder2 = "RQ1-seed8-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed9"
  folder2 = "RQ1-seed9-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed10"
  folder2 = "RQ1-seed10-noise0.05"
  producePairedResult(folder1,folder2)
  
  folder1 = "RQ1-seed11"
  folder2 = "RQ1-seed11-noise0.05"
  producePairedResult(folder1,folder2)
}

setwd('../experimentResult')
getwd()
# scenarios = c('DelayedCVIdeal','DelayedCVExtension','DelayedCVPosNegWindow(7-90)')
scenarios = c('DelayedCVExtension','DelayedCVPosNegWindow(7-90)')
fold = '5Fold'
eva = 'FF0.99'
postfix = "detail"
folds = 0:4
interval = 1000
indicators = c('Recall for class 1 (percent)',
               'Kappa Recall Temporal Statistic 1 (percent)',
               'Gmean for recall  (percent)',
               'Kappa Gmean Temporal Statistic  (percent)')

for(scenario in scenarios){
  output_files()
}
