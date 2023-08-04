library(dplyr)
# res_root <- "D:/work/real-world-evaluation/r_script/result/differentLearner-30Fold/"
res_root <- "D:/work/real-world-evaluation/r_script/result/diffLearner/"    
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
scenarios <- c("PosNegWindow_7_15")
indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Kappa Recall Temporal Statistic 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] Kappa Gmean Temporal Statistic  (percent)',
               '[avg] FPR for class 1 (percent)',
               '[avg] Kappa FPR Temporal Statistic 1 (percent)')
# indicators = c('[avg] Recall for class 1 (percent)',
#                '[avg] Gmean for recall  (percent)',
#                '[avg] FPR for class 1 (percent)')

# percentages <- c(0.1,0.5,1)
# percentages <- c('1000','2000','3000','4000','5000')

#resamples <- c('OnlineUnderOverBagging','OnlineRUSBoost')

dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years/key_timestamp/top5000Commits/'


for (scenario in scenarios) {
  first_flag <- T
  for(project in projects){
    for(clf in clfs){
      temp <- glob2rx(project)
      temp <- substring(temp, 2, nchar(temp)-1)
      file <- list.files(res_root,pattern=paste(temp,'.*_',clf,'_.*',scenario,'.*','dumpFile.csv$',sep = ''))
      df <- read.csv(file = file.path(res_root,file),check.names = F)
      read.csv(file = file.path(res_root,'0_brackets_trees.AdaHoeffdingOptionTree_EvaluatePrequentialDelayedCVPosNegWindow_7_15_seed1_Bootstrap-Validation_10Fold_FF0.99_dumpFile.csv'))
      
      
      key_timestamp_file <- list.files(dataset_path,pattern = temp)
      key_timestamp_df <- read.csv(file.path(dataset_path,key_timestamp_file))
      percentages <- key_timestamp_df$percentages
      
      df.idxs <- c()
      for (ts in key_timestamp_df$timestamp) {
        df.idxs <- append(df.idxs,which.min(abs(df$`current timestamp` - ts)))
      }
       
      
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
  data_path <- 'D:/work/real-world-evaluation/r_script/learnerComparison/'
  dir.create(data_path,showWarnings = F)
  
  
 
  
  total_res$`%ts`
  # 根据group_col进行分组，计算median_col的中位数
  result <- total_res %>%
    group_by(`%ts`,classifier) %>%
    summarize(across(all_of(indicators), median, .names = "{.col}_median"))
  
  write.csv(as.data.frame(result),file.path(data_path,'Median_summaryForTable.csv'),row.names = F)
  write.csv(total_res,file.path(data_path,'summaryForPlot.csv'),row.names = F)
}
