
res_root <- "D:/work/real-world-evaluation/r_script/result/dR/"    
files <- list.files(res_root,pattern = 'csv$')

projects <- c()
for(file in files){
  project <- strsplit(file,'_')[[1]][2]
  projects <- append(projects,project)
}
projects <- unique(projects)

postfixs <- c('-s_1_-i_2','-s_1_-i_1','-s_10_-i_1','-s_10_-i_2')
#scenarios <- c("PosNegWindow_7_90.csv","Extension_90_90.csv","Ideal.csv")
scenarios <- c("PosNegWindow_7_90")
indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Kappa Recall Temporal Statistic 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] Kappa Gmean Temporal Statistic  (percent)',
               '[avg] FPR for class 1 (percent)',
               '[avg] Kappa FPR Temporal Statistic 1 (percent)')
indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] FPR for class 1 (percent)')
percentages <- c(0.1,0.5,1)

resamples <- c('OnlineUnderOverBagging','OnlineRUSBoost')

for (scenario in scenarios) {
  first_flag <- T
  for(project in projects){
    file.noresamples <- list.files(res_root,pattern=paste(temp,'.*',scenario,'.*','dumpFile.csv$',sep = ''))
    file.noresample <-file.noresamples[!grepl('imbalance',file.noresamples)] 
    df_noresample <- read.csv(file.path(res_root,file.noresample),check.names = F)
    df_noresample.idxs <- ceiling(nrow(df_noresample)*percentages)
    
    df_noresample.sub <- df_noresample[df_noresample.idxs,indicators]
    df_noresample.sub <- cbind(strategy='no-resample',df_noresample.sub)
    df_noresample.sub <- cbind(set='',df_noresample.sub)
    df_noresample.sub <- cbind(percentages,df_noresample.sub)
    
    for(resample in resamples){
      for (postfix in postfixs) {
        temp <- glob2rx(project)
        temp <- substring(temp, 2, nchar(temp)-1)
        file <- list.files(res_root,pattern=paste(temp,'.*',resample,'.*',postfix,'.*',scenario,'.*','dumpFile.csv$',sep = ''))
        
        df <- read.csv(file.path(res_root,file),check.names = F)
        
        df.idxs <- ceiling(nrow(df)*percentages)
        
        df.sub <- df[df.idxs,indicators]
        df.sub <- cbind(strategy=resample,df.sub)
        df.sub <- cbind(set=postfix,df.sub)
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
  data_path <- 'D:/work/real-world-evaluation/r_script/differentResample'
  dir.create(data_path,showWarnings = F)
  write.csv(total_res,file.path(data_path,paste(scenario,'_HoeffdingTree.csv',sep='')),row.names = F)
}
