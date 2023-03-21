# two stage SK-ESD test

library(ScottKnottESD)
library(dplyr)
indicators <- c('Recall for class 1 (percent)',
                             'Kappa Recall Temporal Statistic 1 (percent)',
                             'Gmean for recall  (percent)',
                             'Kappa Gmean Temporal Statistic  (percent)',
                             'FPR for class 1 (percent)',
                             'Kappa FPR Temporal Statistic 1 (percent)')

# indicator <- indicators[1]
setwd('D:/work/real-world-evaluation/')
root_path <- 'r_script/result/differentLearner-30Fold/'
files <- list.files(root_path,pattern = 'dumpFile.csv$')

projects <- c()
clfs <- c()
scenarios <- c()
seeds <- c()
validations <- c()
folds <- c()
fadingfactors <- c()

for (file in files) {
  cat(file,'\n')
  if(length(strsplit(file,'_')[[1]])==9){
    project <- strsplit(file,'_')[[1]][2]
    clf <- strsplit(file,'_')[[1]][3]
    scenario <- strsplit(file,'_')[[1]][4]
    seed <- strsplit(file,'_')[[1]][5]
    validation <- strsplit(file,'_')[[1]][6]
    fold <- strsplit(file,'_')[[1]][7]
    fadingfactor <- strsplit(file,'_')[[1]][8]
  }else{
    project <- strsplit(file,'_')[[1]][2]
    clf <- strsplit(file,'_')[[1]][3]
    scenario <- strsplit(file,'_')[[1]][4]
    pos <- strsplit(file,'_')[[1]][5]
    neg <- strsplit(file,'_')[[1]][6]
    seed <- strsplit(file,'_')[[1]][7]
    validation <- strsplit(file,'_')[[1]][8]
    fold <- strsplit(file,'_')[[1]][9]
    fadingfactor <- strsplit(file,'_')[[1]][10]
  }
  projects <- append(projects,project)
  clfs <- append(clfs,clf)
  scenarios <- append(scenarios,scenario)
  seeds <- append(seeds,seed)
  validations <- append(validations,validation)
  folds <- append(folds,fold)
  fadingfactors <- append(fadingfactors,fadingfactor)
}

projects <- unique(projects)
clfs <- unique(clfs)
scenarios <- unique(scenarios)
seeds <- unique(seeds)
validations <- unique(validations)
folds <- unique(folds)
fadingfactors <- unique(fadingfactors)

scenario <- scenarios[1]

for (scenario in scenarios) {
  for(indicator in indicators){
    sk_res_first_flag <- T
    for (project in projects) {
    
  
    first_flag <- T  
    for (clf in clfs) {
      clf_ <- paste('_',clf,'_',sep='')  
      file <- list.files(root_path,pattern = glob2rx(paste('',project,clf_,scenario,'detail.csv',sep='*')))
        cat(file,'\n')
        df <- read.csv(file.path(root_path,file),check.names = F)
        
        df <- df[c('learning evaluation instances on certain fold',indicator)]
        
        df.checkpoint <- df %>% group_by(`learning evaluation instances on certain fold`) %>% 
          summarise(across(everything(), mean) ) %>% as.data.frame()
        if(first_flag){
          first_flag <- F
          res <- data.frame(clf = df.checkpoint[,indicator])
          colnames(res) <- clf
        }else{
          res[,clf] <- df.checkpoint[,indicator]
        }
    }
    
    sk <- sk_esd(res, version='np')
    
    
    if(sk_res_first_flag){
      sk_res_first_flag <- F
      sk_res <- as.data.frame(sk$groups)
      colnames(sk_res) <- project
    }else{
      temp <- as.data.frame(sk$groups)
      sk_res[row.names(temp),project] <- temp$`sk$groups`
    }
    }
  }
}






