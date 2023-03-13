
library("stringr")  

producePairedResult <- function(folder1,folder2){
  
  files1 <- list.files(path = folder1,pattern = 'detail.csv$')
  files2 <- list.files(path = folder2,pattern = 'detail.csv$')
  files1 <- str_sort(files1)
  files2 <- str_sort(files2)
  
  first_flag = TRUE
  for(idx in 1:length(files2)){
    df1 = read.csv(file.path(folder1,files1[idx]),check.names = FALSE)
    df2 = read.csv(file.path(folder2,files2[idx]),check.names = FALSE)
    df1$`Kappa FPR Temporal Statistic 1 (percent)`
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
          file <- strsplit(files1[idx],'_')[[1]][2]
          total_res[nrow(total_res)+1,] = c(scenario,file,fold,interval*counter,indicator,v1,v2)
          counter = counter+1
        }
      }
    }
    
  }
  
  pat = paste(scenario,fold,eva,postfix,sep='_')
  res_file = paste('paired_performance','_',folder1,'_',folder2,'_',pat,'.csv',sep = '')
  write.csv(total_res,file = file.path('pairedResult/',res_file))
}



Type1_error <- function(maxPair=50){
  folders <- c("seed1-noise0","seed2-noise0","seed3-noise0","seed4-noise0","seed5-noise0","seed6-noise0","seed7-noise0","seed8-noise0","seed9-noise0","seed10-noise0","seed11-noise0")
  counter <- 1

  for(i in 1:length(folders)){
    for(j in (i+1):length(folders)){
      if(counter > maxPair){
        break
      }
      folder1 <- folders[i]
      folder2 <- folders[j]
      cat(folder1,folder2,'\n')
      producePairedResult(folder1,folder2)
      counter <- counter + 1
    }
  }
  
}


Type2_error <- function(maxPair=50){
  folders0 <- c("seed1-noise0","seed2-noise0","seed3-noise0","seed4-noise0","seed5-noise0","seed6-noise0","seed7-noise0","seed8-noise0","seed9-noise0","seed10-noise0","seed11-noise0")
  folders005 <- c("seed1-noise0.05", "seed2-noise0.05", "seed3-noise0.05", "seed4-noise0.05", "seed5-noise0.05", "seed6-noise0.05", "seed7-noise0.05", "seed8-noise0.05", "seed9-noise0.05", "seed10-noise0.05", "seed11-noise0.05")
  folders01 <- c("seed1-noise0.1","seed2-noise0.1","seed3-noise0.1","seed4-noise0.1","seed5-noise0.1","seed6-noise0.1","seed7-noise0.1","seed8-noise0.1","seed9-noise0.1","seed10-noise0.1","seed11-noise0.1")
    
  counter <- 1
  
  for(i in 1:length(folders0)){
    
      if(counter > maxPair){
        break
      }
      folder1 <- folders0[i]
      folder2 <- folders005[i]
      folder3 <- folders01[i]
      
      cat(folder1,folder2,folder3,'\n')
      producePairedResult(folder1,folder2)
      producePairedResult(folder1,folder3)
      counter <- counter + 1
  }
  
 
  
}





setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
setwd('./result/differentNoise/HATCL/')

# scenarios = c('DelayedCVIdeal','DelayedCVExtension','DelayedCVPosNegWindow(7-90)')
scenarios = c('DelayedCVPosNegWindow(7-90)')
fold = '5Fold'
eva = 'FF0.99'
postfix = "detail"
folds = 0:4
interval = 1000
indicators = c('Recall for class 1 (percent)',
               'Kappa Recall Temporal Statistic 1 (percent)',
               'Gmean for recall  (percent)',
               'Kappa Gmean Temporal Statistic  (percent)',
               'FPR for class 1 (percent)',
               'Kappa FPR Temporal Statistic 1 (percent)')



for(scenario in scenarios){
  Type1_error()
  Type2_error()
}
