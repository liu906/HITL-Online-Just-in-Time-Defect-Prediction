dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years/key_timestamp/top5000Commits/'

# res_root <- "D:/work/real-world-evaluation/r_script/result/dR/"    
res_root <- "D:/work/real-world-evaluation/r_script/result/diffResampling/"    
files <- list.files(res_root,pattern = 'csv$')

projects <- c()
for(file in files){
  project <- strsplit(file,'_')[[1]][2]
  projects <- append(projects,project)
}
projects <- unique(projects)

# postfixs <- c('-s_1_-i_2','-s_1_-i_1','-s_10_-i_1','-s_10_-i_2')
postfixs <- c('-s_1_-i_2','-s_1_-i_1')
#scenarios <- c("PosNegWindow_7_90.csv","Extension_90_90.csv","Ideal.csv")
scenarios <- c("PosNegWindow_7_15")
indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Kappa Recall Temporal Statistic 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] Kappa Gmean Temporal Statistic  (percent)',
               '[avg] FPR for class 1 (percent)',
               '[avg] Kappa FPR Temporal Statistic 1 (percent)')
indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] FPR for class 1 (percent)')
# percentages <- c(0.1,0.5,1)

resamples <- c('OnlineUnderOverBagging','OnlineRUSBoost')

for (scenario in scenarios) {
  first_flag <- T
  for(project in projects){
    temp <- glob2rx(project)
    temp <- substring(temp, 2, nchar(temp)-1)
    
    file.noresamples <- list.files(res_root,pattern=paste(temp,'.*',scenario,'.*','_10Fold_FF0.99_dumpFile.csv$',sep = ''))
    file.noresample <-file.noresamples[!grepl('imbalance',file.noresamples)] 
    df_noresample <- read.csv(file.path(res_root,file.noresample),check.names = F)
    
    
    key_timestamp_file <- list.files(dataset_path,pattern = temp)
    key_timestamp_df <- read.csv(file.path(dataset_path,key_timestamp_file))
    ts <- key_timestamp_df$timestamp
    df_noresample.idxs <- c()
    for (ts in key_timestamp_df$timestamp) {
      df_noresample.idxs <- append(df_noresample.idxs,which.min(abs(df_noresample$`current timestamp` - ts)))
    }
    
    # df_noresample.idxs <- ceiling(nrow(df_noresample)*percentages)
    
    df_noresample.sub <- df_noresample[df_noresample.idxs,indicators]
    df_noresample.sub <- cbind(strategy='no-resample',df_noresample.sub)
    df_noresample.sub <- cbind(set='no-resample',df_noresample.sub)
    df_noresample.sub <- cbind(percentages,df_noresample.sub)
    
    for(resample in resamples){
      for (postfix in postfixs) {
  
        file <- list.files(res_root,pattern=paste(temp,'.*',resample,'.*',postfix,'.*',scenario,'.*','_10Fold_FF0.99_dumpFile.csv$',sep = ''))
        
        df <- read.csv(file.path(res_root,file),check.names = F)
        df.idxs <- c()
        for (ts in key_timestamp_df$timestamp) {
          df.idxs <- append(df.idxs,which.min(abs(df$`current timestamp` - ts)))
        }
        # df.idxs <- ceiling(nrow(df)*percentages)
        
        df.sub <- df[df.idxs,indicators]
        df.sub <- cbind(strategy=resample,df.sub)
        
        
        df.sub <- cbind(set=paste(strsplit(postfix,'_')[[1]],collapse = ' '),df.sub)
        df.sub <- cbind(percentages,df.sub)
        df_noresample.sub <- rbind(df_noresample.sub,df.sub)
      }
    }
    
    df_noresample.sub <-cbind(project=project,df_noresample.sub)
  if(first_flag){
    first_flag <- F
    total_res <- df_noresample.sub
  }else{
    total_res <- rbind(total_res,df_noresample.sub)
  }
  }
  data_path <- 'D:/work/real-world-evaluation/r_script/ldifferentResample'
  dir.create(data_path,showWarnings = F)
  write.csv(total_res,file.path(data_path,paste(scenario,'_HoeffdingTree.csv',sep='')),row.names = F,quote = F)
}
