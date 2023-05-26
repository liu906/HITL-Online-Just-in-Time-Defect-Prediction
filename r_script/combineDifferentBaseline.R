library(dplyr)
res_root <- "D:/work/real-world-evaluation/r_script/result/diffLearner-100/"    
files <- list.files(res_root,pattern = 'csv$')

projects <- c()
clfs <- c()
for(file in files){
  project <- strsplit(file,'_')[[1]][2]
  clf <- strsplit(file, '_')[[1]][3]
  projects <- append(projects,project)
  clfs <- append(clfs,clf)
}
projects <- unique(projects)
clfs <- unique(clfs)

#scenarios <- c("PosNegWindow_7_90.csv","Extension_90_90.csv","Ideal.csv")
scenarios <- c("PosNegWindow_7_90")
# indicators = c('[avg] Recall for class 1 (percent)',
#                '[avg] Kappa Recall Temporal Statistic 1 (percent)',
#                '[avg] Kappa Recall M Statistic 1 (percent)',
#                '[avg] Gmean for recall  (percent)',
#                '[avg] Kappa Gmean Temporal Statistic  (percent)',
#                '[avg] Kappa Gmean M Statistic  (percent)',
#                '[avg] FPR for class 1 (percent)',
#                '[avg] Kappa FPR Temporal Statistic 1 (percent)',
#                '[avg] Kappa FPR M Statistic 1 (percent)')

indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] FPR for class 1 (percent)')
percentages <- c(0.1,0.5,1)

#resamples <- c('OnlineUnderOverBagging','OnlineRUSBoost')

for (scenario in scenarios) {
  first_flag <- T
  for(project in projects){
    for(clf in clfs){
      temp <- glob2rx(project)
      temp <- substring(temp, 2, nchar(temp)-1)
      file <- list.files(res_root,pattern=paste(temp,'.*_',clf,'_.*',scenario,'.*','dumpFile.csv$',sep = ''))
      df <- read.csv(file = file.path(res_root,file),check.names = F)
      
      df.idxs <- ceiling(nrow(df)*percentages)
      first_ind <- T
      for (indicator in indicators) {
        sub_df <- df[df.idxs,indicator]
        
        sub_df <- cbind(indicator=indicator,value=as.data.frame(sub_df))
        sub_df <- cbind(scenario=scenario,sub_df)
        sub_df <- cbind(project=project,sub_df)
        sub_df <- cbind(classifier=clf,sub_df)
        sub_df <- cbind(`%ts`=percentages,sub_df)
        if(first_ind){
          first_ind <- F
          total_sub_df <- sub_df
        }else{
          total_sub_df <- rbind(sub_df,total_sub_df)
        }
      }
      
      
      if(first_flag){
        first_flag <- F
        total_res <- total_sub_df
      }else{
        total_res <- rbind(total_res,total_sub_df)
      }
    }
    
  }
  data_path <- 'D:/work/real-world-evaluation/r_script/baselineComparison/'
  dir.create(data_path,showWarnings = F)
  
  # 根据group_col进行分组，计算median_col的中位数
  write.csv(total_res,file.path(data_path,'summaryForPlot.csv'),row.names = F)
}

for (scenario in scenarios) {
  first_flag <- T
  for(project in projects){
    for(clf in clfs){
      temp <- glob2rx(project)
      temp <- substring(temp, 2, nchar(temp)-1)
      file <- list.files(res_root,pattern=paste(temp,'.*_',clf,'_.*',scenario,'.*','dumpFile.csv$',sep = ''))
      df <- read.csv(file = file.path(res_root,file),check.names = F)
      
      df.idxs <- ceiling(nrow(df)*percentages)
      first_ind <- T
      
        sub_df <- df[df.idxs,indicators]
        sub_df <- cbind(scenario=scenario,sub_df)
        sub_df <- cbind(project=project,sub_df)
        sub_df <- cbind(classifier=clf,sub_df)
        sub_df <- cbind(`%ts`=percentages,sub_df)
        
      
      
      
      if(first_flag){
        first_flag <- F
        total_res <- sub_df
      }else{
        total_res <- rbind(total_res,sub_df)
      }
    }
    
  }
  data_path <- 'D:/work/real-world-evaluation/r_script/baselineComparison/'
  dir.create(data_path,showWarnings = F)
  
  result <- total_res %>%
    group_by(`%ts`,classifier) %>%
    summarize(across(all_of(indicators), median, .names = "{.col}_median"))
  
  write.csv(as.data.frame(result),file.path(data_path,'Median_summaryForTable.csv'),row.names = F)
}

