#compute total la for clean commits and total la for buggy commits every month
setwd('/media/lxt/TOSHIBA EXT/moa/')
data_path <- './commit_guru_dataset/cut2years/'
files <- list.files(data_path,pattern = 'csv')
file <- files[1]
for(file in files){
  df <- read.csv(file.path(data_path,file))
  max_ts <- max(df$author_date_unix_timestamp)
  min_ts <- min(df$author_date_unix_timestamp)
  one_month <- 30*24*60*60
  
  current_ts <- min_ts 
  first_flag <- T
  counter <- 1
  while(current_ts < max_ts){
    sub_df <- df[df$author_date_unix_timestamp >= current_ts & df$author_date_unix_timestamp < current_ts  + one_month ,]
    current_ts <- current_ts  + one_month
    
    total_commit <- nrow(sub_df)
    
    total_la <- sum(sub_df$la)
    total_la_clean <- sum(sub_df[sub_df$contains_bug==0,'la'])
    total_la_buggy <- sum(sub_df[sub_df$contains_bug!=0,'la'])
    
    total_ld <- sum(sub_df$ld)
    total_ld_clean <- sum(sub_df[sub_df$contains_bug==0,'ld'])
    total_ld_buggy <- sum(sub_df[sub_df$contains_bug!=0,'ld'])
    
    total_lt <- sum(sub_df$lt)
    total_lt_clean <- sum(sub_df[sub_df$contains_bug==0,'lt'])
    total_lt_buggy <- sum(sub_df[sub_df$contains_bug!=0,'lt'])
    if(first_flag){
      first_flag <- F
      total_res <- c(counter,total_commit,total_la,total_la_clean,total_la_buggy,
                     total_ld,total_ld_clean,total_ld_buggy
                     ,total_lt,total_lt_clean,total_lt_buggy)
      total_res <- as.data.frame(t(total_res))
      colnames(total_res) <- c('No.month','total_commit','total_la','total_la_clean','total_la_buggy',
      'total_ld','total_ld_clean','total_ld_buggy',
      'total_lt','total_lt_clean','total_lt_buggy')
    }else{
      total_res[nrow(total_res)+1,] <- c(counter,total_commit,total_la,total_la_clean,total_la_buggy,
                     total_ld,total_ld_clean,total_ld_buggy
                     ,total_lt,total_lt_clean,total_lt_buggy)
    }
    counter <- counter + 1
    
  }
  write.csv(total_res,file.path('./r_script/result/datasetPattern/',paste('summaryBy1Month-',file,sep='')),quote = F,row.names = F)
}