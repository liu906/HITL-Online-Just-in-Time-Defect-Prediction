setwd('/media/lxt/TOSHIBA EXT/moa/')
list.dirs('experimentResult/', recursive = F)
settings <- c(
  # 'CSMOTE', CSMOTE changes #instance of result, so it is droped
  'HoeffdingAdaptiveTree',
  'OnlineAdaBoost',
  'OnlineAdaC2',
  'OnlineRUSBoost',
  'OnlineSMOTEBagging',
  'OnlineUnderOverBagging',
  'RebalanceStream'
)

subfolder <- 'RQ1-seed2'
files <-
  list.files("experimentResult/RebalanceStream/RQ1-seed2/", pattern = 'dumpFile')

indicators <- c(
  '[avg] Recall for class 1 (percent)',
  '[avg] Kappa Recall Temporal Statistic 1 (percent)',
  '[avg] Gmean for recall  (percent)',
  '[avg] Kappa Gmean Temporal Statistic  (percent)'
)

for (file in files) {
  first_setting <- T
  for (setting in settings) {
    first_flag <- T
    for (indicator in indicators) {
      cat(indicator, '\n')
      
      file_name <-
      file.path('experimentResult', setting, subfolder, file)
      df <- read.csv(file = file_name, check.names = F)
      colnames(df)
      ind_col <- df[, indicator]
      
      if (first_flag) {
        first_flag <- F
        res <- as.data.frame(df[, "learning evaluation instances"])
        colnames(res)[1] <- "learning evaluation instances"
        res[, 'setting'] <- setting
        res[, indicator] <- as.data.frame(ind_col)
        
      } else{
        res[, indicator] <- as.data.frame(ind_col)
      }
      
    }
    if (first_setting) {
      total_res <- res
      first_setting <- F
    } else{
      total_res <- rbind(total_res, res)
    }
  }
  
  write.csv(
    total_res,
    file.path('r_script/result/imbalanceStrategyComparison/', file),
    row.names = F,
    quote = F
  )
  
}

files <- list.files('r_script/result/imbalanceStrategyComparison/',full.names = T)

for(file in files){
  df <- read.csv(file,check.names = F)
  write.csv(df,file,row.names = F,quote = F)
}


