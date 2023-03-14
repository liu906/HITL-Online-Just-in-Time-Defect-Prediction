

library("parallel")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('mcnemar.R')


producePairedResult <- function(folder1,folder2,interval){
  
  files1 <- list.files(path = folder1,pattern = 'detail.csv$')
  files2 <- list.files(path = folder2,pattern = 'detail.csv$')
  files1 <- str_sort(files1)
  files2 <- str_sort(files2)
  
  first_flag = TRUE
  for(idx in 1:length(files2)){
    df1 = read.csv(file.path(folder1,files1[idx]),check.names = FALSE)
    df2 = read.csv(file.path(folder2,files2[idx]),check.names = FALSE)
    
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
  dir.create("pairedResult",showWarnings = F)
  write.csv(total_res,file = file.path('pairedResult/',res_file))
}



produceMcNemarResult <- function(folder1,folder2,interval=1000){
  pat = paste(scenario,fold,eva,postfix,sep='_')
  
  files1 <- list.files(path = folder1,pattern = 'detail.csv$')
  files2 <- list.files(path = folder2,pattern = 'detail.csv$')
  files1 <- str_sort(files1)
  files2 <- str_sort(files2)
  
  res_file = paste('mcnemar','_',folder1,'_',folder2,'_',pat,'.csv',sep = '')
  res_file_path = file.path('pairedResult',res_file)
  dir.create('pairedResult',showWarnings = F)
  write(paste('scenario','dataset','fold','#instances','folder1','folder2','mcnemar',sep = ','), file=res_file_path, append=F)
  
  
  
  for(idx in 1:length(files1)){
    df1 = read.csv(file.path(folder1,files1[idx]),check.names = FALSE)
    df2 = read.csv(file.path(folder2,files2[idx]),check.names = FALSE)
    
    for(fold in folds){
      
      value1 = df1[as.numeric(df1$fold)==fold,]
      value2 = df2[as.numeric(df2$fold)==fold,]
      
      list_m <- McNemar(value1,value2,interval)
      counter = 1
      while(counter <= length(list_m)){
        sample_value = list_m[counter]
        file <- strsplit(files1[idx],'_')[[1]][2]
        write(paste(scenario,file,fold,interval*counter,folder1,folder2,sample_value,sep = ','), file=res_file_path, append=T)
        counter = counter+1
      }
    }
  }
}


parallel_run <- function(example_element){
  library("stringr")  
  folder1 <- example_element$folder1
  folder2 <- example_element$folder2
  mcnemar_test <- example_element$mcnemar_test
  cat(folder1,folder2,'\n')
  if(mcnemar_test){
    produceMcNemarResult(folder1,folder2,interval)
  }else{
    producePairedResult(folder1,folder2,interval)
  }
}


parallel_run2 <- function(example_element){
  library("stringr")  
  folder1 <- example_element$folder1
  folder2 <- example_element$folder2
  folder3 <- example_element$folder2
  mcnemar_test <- example_element$mcnemar_test
  
  cat(folder1,folder2,folder3,'\n')
  
  if(mcnemar_test){
    produceMcNemarResult(folder1,folder2,interval)
    produceMcNemarResult(folder1,folder3,interval)

  }else{
    producePairedResult(folder1,folder2,interval)
    producePairedResult(folder1,folder3,interval)
  }
}

Type1_error <- function(df_folders,maxPair=50,mcnemar_test=F){
  counter <- 1
  folders0 <- df_folders$folders0
  folders005 <- df_folders$folders005
  folders01 <- df_folders$folders01
  detectCores()
  cl <- makeCluster(12)
  
  clusterExport(cl,c("produceMcNemarResult",
                     "producePairedResult",
                     "scenario",
                     "fold","eva",
                     "postfix",
                     "folds",
                     "McNemar",
                     "interval"),envir=environment())
  example_list <- list()
  for(i in 1:length(folders0)){
    for(j in (i+1):length(folders0)){
      if(counter > maxPair){
        break
      }
      example_list[[length(example_list)+1]] <- list(folder1=folders0[i], folder2 = folders0[j],mcnemar_test=mcnemar_test)
      counter <- counter + 1
    }
  }
  parLapply(cl,example_list,parallel_run)
  stopCluster(cl)
}


Type2_error <- function(df_folders,maxPair=50,mcnemar_test=F){
  folders0 <- df_folders$folders0
  folders005 <- df_folders$folders005
  folders01 <- df_folders$folders01
  
  counter <- 1
  
  detectCores()
  cl <- makeCluster(12)
  clusterExport(cl,c("produceMcNemarResult",
                     "producePairedResult",
                     "scenario",
                     "fold","eva",
                     "postfix",
                     "folds",
                     "McNemar",
                     "interval"),envir=environment())
  example_list <- list()
  for(i in 1:length(folders0)){
    
      if(counter > maxPair){
        break
      }
      example_list[[length(example_list)+1]] <- list(folder1=folders0[i], 
                                                     folder2 = folders005[i],
                                                     folder3 = folders01[i],
                                                     mcnemar_test=mcnemar_test)
      counter <- counter + 1
  
  }
  parLapply(cl,example_list,parallel_run)
  stopCluster(cl)
  
  
}


# scenarios = c('DelayedCVIdeal','DelayedCVExtension','DelayedCVPosNegWindow(7-90)')
scenarios = c('DelayedCVPosNegWindow(7-90)')
fold = '5Fold'
eva = 'FF0.99'
postfix = "detail"
folds = 0:4
indicators = c('Recall for class 1 (percent)',
               'Kappa Recall Temporal Statistic 1 (percent)',
               'Gmean for recall  (percent)',
               'Kappa Gmean Temporal Statistic  (percent)',
               'FPR for class 1 (percent)',
               'Kappa FPR Temporal Statistic 1 (percent)')



batchFolder <- function(seeds){
  folders0 <- paste('seed',seeds,'-noise0',sep='')
  folders005 <- paste('seed',seeds,'-noise0.05',sep='')
  folders01 <- paste('seed',seeds,'-noise0.1',sep='')
  return(data.frame(folders0,folders005,folders01))
}


# for(scenario in scenarios){
#   seeds <- 1:5
#   interval = 1000
#   df_folders <- batchFolder(seeds)
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#   setwd('./result/differentNoise/HATCL-simple/')
#   Type1_error(df_folders,maxPair=10,mcnemar_test = T)
# 
# }

for(scenario in scenarios){
  seeds <- 1:10
  interval = 1000
  df_folders <- batchFolder(seeds)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd('./result/differentNoise/HATCL-simple/')
  Type2_error(df_folders,maxPair=10,mcnemar_test = T)
}

 
# for(scenario in scenarios){
#   seeds <- 1:50
#   interval <- 1
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#   setwd('./result/differentNoise/HATCL50/')
#   Type1_error(df_folders,mcnemar_test = F)
#   Type2_error(df_folders,mcnemar_test = F)
# }
