# two stage SK-ESD test

library(ScottKnottESD)
library(dplyr)
indicators <- c(
  'Recall for class 1 (percent)',
  'Kappa Recall Temporal Statistic 1 (percent)',
  'Gmean for recall  (percent)',
  'Kappa Gmean Temporal Statistic  (percent)',
  'FPR for class 1 (percent)',
  'Kappa FPR Temporal Statistic 1 (percent)'
)
dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years/key_timestamp/top5000Commits/'
# indicator <- indicators[1]
setwd('D:/work/real-world-evaluation/')
root_path <- 'r_script/result/differentLearner-30Fold/'
root_path <- 'r_script/result/diffLearner/'
files <- list.files(root_path, pattern = 'dumpFile.csv$')

projects <- c()
clfs <- c()
scenarios <- c()
seeds <- c()
validations <- c()
folds <- c()
fadingfactors <- c()

for (file in files) {
  cat(file, '\n')
  if (length(strsplit(file, '_')[[1]]) == 9) {
    project <- strsplit(file, '_')[[1]][2]
    clf <- strsplit(file, '_')[[1]][3]
    scenario <- strsplit(file, '_')[[1]][4]
    seed <- strsplit(file, '_')[[1]][5]
    validation <- strsplit(file, '_')[[1]][6]
    fold <- strsplit(file, '_')[[1]][7]
    fadingfactor <- strsplit(file, '_')[[1]][8]
  } else{
    project <- strsplit(file, '_')[[1]][2]
    clf <- strsplit(file, '_')[[1]][3]
    scenario <- strsplit(file, '_')[[1]][4]
    pos <- strsplit(file, '_')[[1]][5]
    neg <- strsplit(file, '_')[[1]][6]
    seed <- strsplit(file, '_')[[1]][7]
    validation <- strsplit(file, '_')[[1]][8]
    fold <- strsplit(file, '_')[[1]][9]
    fadingfactor <- strsplit(file, '_')[[1]][10]
  }
  projects <- append(projects, project)
  clfs <- append(clfs, clf)
  scenarios <- append(scenarios, scenario)
  seeds <- append(seeds, seed)
  validations <- append(validations, validation)
  folds <- append(folds, fold)
  fadingfactors <- append(fadingfactors, fadingfactor)
}

projects <- unique(projects)
clfs <- unique(clfs)
scenarios <- unique(scenarios)
seeds <- unique(seeds)
validations <- unique(validations)
folds <- unique(folds)
fadingfactors <- unique(fadingfactors)

commit <- '1000'

for (scenario in scenarios) {
  first_indicator <- T
  for (indicator in indicators) {
    #indicator <- indicators[5]
    sk_res_first_flag <- T
    for (project in projects) {
      #project <- projects[2]
      first_flag <- T
      for (clf in clfs) {
        # if(clf=='functions.MajorityClass' || clf=='functions.NoChange'){
        #   next
        # }
        clf_ <- paste('_', clf, '_', sep = '')
        file <-
          list.files(root_path, pattern = glob2rx(
            paste('', project, clf_, scenario, 'detail.csv', sep = '*')
          ))
        # cat(file, '\n')
        df <- read.csv(file.path(root_path, file), check.names = F)
        temp <- glob2rx(project)
        temp <- substring(temp, 1, nchar(temp)-1)
        temp <- substring(temp, 2, nchar(temp))
        key_timestamp_file <- list.files(dataset_path,pattern = temp)
        key_timestamp_df <- read.csv(file.path(dataset_path,key_timestamp_file))
        percentages <- key_timestamp_df$percentages
        idx_folds <- unique(df$fold)
        df.idxs <- c()
        
        for(idx_fold in idx_folds){
          sub_df <- df[df$fold==idx_fold,]
          for (ts in key_timestamp_df$timestamp) {
            df.idxs <- append(df.idxs,which.min(abs(sub_df$`current timestamp` - ts)))
          }
        }
        
        df <- df[df.idxs,]
        
        df$commits <- sort(rep(percentages,length(idx_folds)))
        df <- df[df$commits==commit,]
        df.checkpoint <-
          df[c('learning evaluation instances on certain fold',
               indicator)]
        
        

        
        # df.checkpoint <-
        #   df %>% group_by(`learning evaluation instances on certain fold`) %>%
        #   summarise(across(everything(), mean)) %>% as.data.frame()
        if(grepl('FPR', indicator, fixed = TRUE)){
          df.checkpoint[,indicator] = -df.checkpoint[,indicator]
        }
        
        
        
        
        if (first_flag) {
          first_flag <- F
          res <- data.frame(clf = df.checkpoint[, indicator])
          colnames(res) <- clf
        } else{
          df.checkpoint <- df.checkpoint[1:nrow(res),]
          res[, clf] <- df.checkpoint[, indicator]
        }
      }
      
      
      
      sk <- sk_esd(res, version = 'np')
      if (sk_res_first_flag) {
        sk_res_first_flag <- F
        sk_res <- as.data.frame(sk$groups)
        colnames(sk_res) <- project
      } else{
        temp <- as.data.frame(sk$groups)
        # cat(row.names(temp),'\n')
        sk_res[row.names(temp), project] <- temp$`sk$groups`
      }
    }
    
    two_level_sk <- sk_esd(t(-sk_res),version = 'np')
    plot(two_level_sk)
    two_level_sk_df <- as.data.frame(two_level_sk$groups)
    cat(row.names(two_level_sk_df),'\n')
    colnames(two_level_sk_df) <- indicator
    if(first_indicator){
      first_indicator <- F
      total_two_level_sk <- two_level_sk_df
    }else{
      total_two_level_sk[row.names(two_level_sk_df), indicator] <- two_level_sk_df[row.names(two_level_sk_df),indicator]
    }
  }
  sk_root <- 'r_script/learnerComparison/'
  dir.create(sk_root,showWarnings = F)
  write.csv(total_two_level_sk,file.path(sk_root,paste(scenario,folds,fadingfactors,commit,'.csv',sep='_')))
 }
