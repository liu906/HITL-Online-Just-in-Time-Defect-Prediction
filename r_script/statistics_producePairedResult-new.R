library("parallel")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('mcnemar.R')
library('dplyr')

producePairedResult <- function(folder1,folder2,interval){
  cat(folder1,folder2,'\n')
  posfix <- paste(fold,eva,postfix,sep='_')
  pat = paste(scenario,fold,eva,postfix,sep='_')
  
  files1 <- list.files(path = folder1,pattern = paste(posfix,'.csv$',sep=''))
  files2 <- list.files(path = folder2,pattern = paste(posfix,'.csv$',sep=''))
  
  files1 <- str_sort(files1)
  files2 <- str_sort(files2)
  
  first_flag = TRUE
  for(idx in 1:length(files2)){
    
    df1 <- read.table(file.path(folder1,files1[idx]),sep = ',',check.names = F,colClasses = x,header = T)
    df2 <- read.table(file.path(folder2,files2[idx]),sep = ',',check.names = F,colClasses = x,header = T)
    
    file <- strsplit(files1[idx],'_')[[1]][2]
    cat('file ',file,'\n')
    system.time(
    for(indicator in indicators){
      #cat('indicator ',indicator,'\n')
      for(f in folds){
        #cat('fold ',fold,'\n') 
        value1 = as.numeric(df1[as.numeric(df1$f)==f,indicator])
        value2 = as.numeric(df2[as.numeric(df2$f)==f,indicator])
        min_len <- min(length(value1),length(value2))
        # TODO: change the definition of interval
        #idx_sep <- seq(interval,length(value1),interval)
        idx_sep <- seq(interval,min_len,interval)
        #idx_sep <- seq(1,length(value1),length(value1)/interval)
        counter = 1
        v1 <- value1[idx_sep]
        v2 <- value2[idx_sep]
        sub_df <- data.frame(scenario=scenario,dataset=file,fold=f,`#instances`=idx_sep,indicator=indicator,folder1=v1,folder2=v2,check.names=F)
        colnames(sub_df)[6] = folder1
        colnames(sub_df)[7] = folder2
        if(first_flag){
          first_flag <- F
          total_res <- sub_df
        }else{
          total_res <- rbind(total_res,sub_df)
        }

      }
    }
    )
  }
  
  
  res_file = paste('paired_performance','_',folder1,'_',folder2,'_',pat,'.csv',sep = '')
  dir.create("pairedResult",showWarnings = F)
  write.csv(total_res,file = file.path('pairedResult/',res_file))
}



produceMcNemarResult <- function(folder1,folder2,interval){
  pat = paste(scenario,fold,eva,postfix,sep='_')
  
  posfix <- paste(fold,eva,postfix,sep='_')
  files1 <- list.files(path = folder1,pattern = paste(posfix,'.csv$',sep=''))
  files2 <- list.files(path = folder2,pattern = paste(posfix,'.csv$',sep=''))
  files1 <- str_sort(files1)
  files2 <- str_sort(files2)
  
  res_file = paste('mcnemar','_',folder1,'_',folder2,'_',pat,'.csv',sep = '')
  res_file_path = file.path('pairedResult',res_file)
  dir.create('pairedResult',showWarnings = F)
  write(paste('scenario','dataset','fold','#instances','folder1','folder2','mcnemar',sep = ','), file=res_file_path, append=F)
  
  
  first_flag = T
  for(idx in 1:length(files1)){
    cat(idx,'\n')
    df1 <- read.table(file.path(folder1,files1[idx]),sep = ',',check.names = F,colClasses = y,header = T)
    df2 <- read.table(file.path(folder2,files2[idx]),sep = ',',check.names = F,colClasses = y,header = T)
    
    file <- strsplit(files1[idx],'_')[[1]][2]
    for(fold in folds){
      
      value1 = df1[as.numeric(df1$fold)==fold,]
      value2 = df2[as.numeric(df2$fold)==fold,]
      # TODO change interval
      list_m <- McNemar(value1,value2,interval)
      # done
      
      sub_df <- data.frame(scenario=scenario,dataset=file,fold=fold,
                   #`#instances`=(floor(length(list_m)/interval))*(1:length(list_m)),
                   `#instances`=1:length(list_m),
                   `folder1`=folder1,
                   `folder2`=folder2,
                   mcnemar=list_m,
                   check.names = F)
      if(first_flag){
        first_flag = F
        res_df <- sub_df
      }else{
        
        res_df <- rbind(res_df,sub_df)
      }
    
    }
  }
  
  write.table(res_df, file=res_file_path, sep = ",", col.names = !file.exists(file=res_file_path), append = T)
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
  folder3 <- example_element$folder3
  folder4 <- example_element$folder4
  folder5 <- example_element$folder5
  mcnemar_test <- example_element$mcnemar_test
  
  cat(folder1,folder2,folder3,'\n')
  
  if(mcnemar_test){
   produceMcNemarResult(folder1,folder2,interval)
    produceMcNemarResult(folder1,folder3,interval)
    #produceMcNemarResult(folder1,folder4,interval)
    #produceMcNemarResult(folder1,folder5,interval)

  }else{
    producePairedResult(folder1,folder2,interval)
    producePairedResult(folder1,folder3,interval)
    #producePairedResult(folder1,folder4,interval)
    #producePairedResult(folder1,folder5,interval)
  }
}

Type1_error <- function(df_folders,maxPair=50,mcnemar_test=F){
  counter <- 1
  folders0 <- df_folders$folders0
  folders005 <- df_folders$folders005
  folders01 <- df_folders$folders01
  detectCores()
  cl <- makeCluster(14,outfile="D:/work/real-world-evaluation/log3.txt")
  
  
  clusterExport(cl,c("produceMcNemarResult",
                     "producePairedResult",
                     "scenario",
                     "fold","eva",
                     "postfix",
                     "folds",
                     "McNemar",
                     "interval",
                     "indicators",
                     "x",
                     "y"),envir=environment())
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
  folders02 <- df_folders$folders02
  folders03 <- df_folders$folders03
  
  counter <- 1
  
  detectCores()
  cl <- makeCluster(14,outfile="D:/work/real-world-evaluation/log2.txt")
  clusterExport(cl,c("produceMcNemarResult",
                     "producePairedResult",
                     "scenario",
                     "fold","eva",
                     "postfix",
                     "folds",
                     "McNemar",
                     "interval",
                     "indicators",
                     "x",
                     "y"),envir=environment())
  example_list <- list()
  for(i in 1:length(folders0)){
    
      if(counter > maxPair){
        break
      }
      example_list[[length(example_list)+1]] <- list(folder1=folders0[i], 
                                                     folder2 = folders005[i],
                                                     folder3 = folders01[i],
                                                     folder4 = folders02[i],
                                                     folder5 = folders03[i],
                                                     mcnemar_test=mcnemar_test)
      counter <- counter + 1
  
  }
  
  parLapply(cl,example_list,parallel_run2)
  stopCluster(cl)
  
  
}


# scenarios = c('DelayedCVIdeal','DelayedCVExtension','DelayedCVPosNegWindow(7-90)')
scenarios = c('DelayedCVIdeal')
 

eva = 'FF0.99'
postfix = "detail"
 
if(T){
  fold = '10Fold'
  folds = 0:9
}else{
  fold = '30Fold'
  folds = 0:29
}
indicators = c('Recall for class 1 (percent)',
               'Kappa Recall Temporal Statistic 1 (percent)',
               'Gmean for recall  (percent)',
               'Kappa Gmean Temporal Statistic  (percent)',
               'FPR for class 1 (percent)',
               'Kappa FPR Temporal Statistic 1 (percent)')

x <- c()
for(i in 1:46){
  if(i %in% c(3,33,35,36,38,42,45)){
    x[length(x)+1] <- "numeric"
  }else{
    x[length(x)+1] <- "NULL"
  }
}


y <- c()
for(i in 1:14){
  if(i %in% c(3,7,8,9,10)){
    y[length(y)+1] <- "numeric"
  }else{
    y[length(y)+1] <- "NULL"
  }
}

batchFolder <- function(seeds){
  folders0 <- paste('seed',seeds,'-noise0',sep='')
  folders005 <- paste('seed',seeds,'-noise0.05',sep='')
  folders01 <- paste('seed',seeds,'-noise0.1',sep='')
  folders02 <- paste('seed',seeds,'-noise0.2',sep='')
  folders03 <- paste('seed',seeds,'-noise0.3',sep='')
  return(data.frame(folders0,folders005,folders01,folders02,folders03))
}

if(F){
  simple_path <- './result/differentNoise/LB-ideal-s/'
  all_path <- './result/differentNoise/LB-ideal/'
}else{
  simple_path <- './result/differentNoise/LB-ideal-s/'
  all_path <- './result/differentNoise/meta.LeveragingBag-10runs/'
}

if(T){
  for(scenario in scenarios){
    seeds <- 1:5
    # TODO: change the definition of interval
    # interval is expected number of check points new
    interval = 10
    df_folders <- batchFolder(seeds)
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    setwd(simple_path)
    Type1_error(df_folders,maxPair=2,mcnemar_test = T)
    
  }
}

if(T){
  for(scenario in scenarios){
    seeds <- 1:10
    #interval = 1000
    interval = 10
    df_folders <- batchFolder(seeds)
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    setwd(simple_path)
    Type2_error(df_folders,maxPair=2,mcnemar_test = T)
  }
}

if(F){
  for(scenario in scenarios){
    seeds <- 1:50
    interval <- 1
    df_folders <- batchFolder(seeds)
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
    setwd(all_path)
    getwd()
    Type1_error(df_folders,maxPair=50,mcnemar_test = F)
    Type2_error(df_folders,maxPair=50,mcnemar_test = F)
  }
} 




##########check data##############
if(F){
  folders <- list.files(pattern = 'noise')
  for(folder in folders){
    files <- list.files(folder,pattern = 'detail')
    for(file in files){
      df <- read.csv(file.path(folder,file),check.names = F)
      answer <- which(df$`learning evaluation instances on certain fold`=='learning evaluation instances on certain fold')
      if(length(answer)>0){
        df <- df[1:(answer-1),]
        write.csv(df,file = file.path(folder,file),quote = F,row.names = F)
      }
    }
  }
  
}


