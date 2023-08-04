

dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years/key_timestamp/'
percentages <- seq(0.1,1,0.1)
# indicators = c('[avg] Recall for class 1 (percent)',
#                '[avg] Kappa Recall Temporal Statistic 1 (percent)',
#                '[avg] Gmean for recall  (percent)',
#                '[avg] Kappa Gmean Temporal Statistic  (percent)',
#                '[avg] FPR for class 1 (percent)',
#                '[avg] Kappa FPR Temporal Statistic 1 (percent)')

indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] FPR for class 1 (percent)')


res_root <- "D:/work/real-world-evaluation/r_script/result/differentEvaluationProcudure/trees.HoeffdingTree/seed1-noise0/dumpFile/"    

files <- list.files(res_root,pattern = 'csv$')

projects <- c()
for(file in files){
  project <- strsplit(file,'_')[[1]][1]
  projects <- append(projects,project)
}
projects <- unique(projects)
first_flag <- T
for(project in projects){
  temp <- glob2rx(project)
  temp <- substring(temp, 1, nchar(temp)-1)
  project.files <- list.files(res_root,pattern = temp)
  temp <- substring(temp, 2, nchar(temp))
  key_timestamp_file <- list.files(dataset_path,pattern = temp)
  key_timestamp_df <- read.csv(file.path(dataset_path,key_timestamp_file))
  project.ideal.file <- project.files[grep('Ideal',project.files)]
  project.estimate.files <- project.files[!grepl('Ideal',project.files)]
  
  df.ideal <- read.csv(file.path(res_root,project.ideal.file),check.names = F)
  df.ideal.idxs <- as.integer(nrow(df.ideal)*percentages)  
  sub_ideal <- df.ideal[df.ideal.idxs,indicators]
  sub_ideal <- cbind(scenario=project.ideal.file, sub_ideal)
  for(project.estimate.file in project.estimate.files){
    
    df.estimate <- read.csv(file.path(res_root,project.estimate.file),check.names = F)
    df.estimate.idxs <- as.integer(nrow(df.estimate)*percentages) 
    sub_estimate <- df.estimate[df.estimate.idxs,indicators]
    sub_estimate <- cbind(scenario=project.estimate.file, sub_estimate)
    sub_ideal <- rbind(sub_ideal,sub_estimate)
  }
  sub_ideal <- cbind(percentage=percentages, sub_ideal)
  if(first_flag){
    first_flag <- F
    total_res <- sub_ideal
  }else{
    total_res <- rbind(total_res,sub_ideal)
  }
}


write.csv(total_res,"D:/work/real-world-evaluation/r_script/RQ3/metric_value_summary.csv",row.names = F)
