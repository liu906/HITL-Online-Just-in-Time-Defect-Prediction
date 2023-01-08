
setwd('/media/lxt/TOSHIBA EXT/moa/commit_guru_dataset/')
files <- list.files(pattern = 'csv')

info <- c('project','total changes','%defect-inducing changes','time period start','time period end','defects/day')
res_df <- data.frame()

colnames(res_df) <- info

first_flag = T
for(file in files){
  project <- substr(file,3,nchar(file)-4)
  df <- read.csv(file)
  total_changes <- nrow(df)
  buggy <- sum(df$contains_bug!=0)
  buggy_ratio <- buggy / total_changes
  time_start_unix <- df$author_date_unix_timestamp[1]
  time_start_date <- as.Date(as.POSIXct(time_start_unix, origin="1970-01-01"))
  time_end_unix <- df$author_date_unix_timestamp[total_changes]
  time_end_date <- as.Date(as.POSIXct(time_end_unix, origin="1970-01-01"))
  days <- length(seq(from=time_start_date, to=time_end_date, by='day')) -1
  defects_per_day <- buggy/days
  if(first_flag){
    first_flag = F
    res_df <- data.frame(list(project,total_changes,buggy_ratio,time_start_date,time_end_date,defects_per_day))
    colnames(res_df) <- info
  }else{
    res_df[nrow(res_df)+1, ] <- list(project,total_changes,buggy_ratio,time_start_date,time_end_date,defects_per_day)
  }
  
}

res_df[nrow(res_df)+1,] <- list("average",mean(res_df$`total changes`),mean(res_df$`%defect-inducing changes`),"","",mean(res_df$`defects/day`))

write.csv(res_df,'../r_script/result/datasetInformation.csv')



