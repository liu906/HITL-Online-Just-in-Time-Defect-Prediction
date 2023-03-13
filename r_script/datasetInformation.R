
# setwd('/media/lxt/TOSHIBA EXT/moa/commit_guru_dataset/cut2years/')
setwd('D:/work/real-world-evaluation/commit_guru_dataset/')

files <- list.files(pattern = 'csv')

info <- c('project','total changes','%defect-inducing changes','time period start','time period end','defects/day','commits/day','bug-fixing/day')


first_flag = T

for(file in files){
  project <- substr(file,3,nchar(file)-4)
  df <- read.csv(file)
  total_bug_fixing <- sum(df$fix>0)
  total_changes <- nrow(df)
  buggy <- sum(df$contains_bug!=0)
  buggy_ratio <- buggy / total_changes
  time_start_unix <- df$author_date_unix_timestamp[1]
  time_start_date <- as.Date(as.POSIXct(time_start_unix, origin="1970-01-01"))
  time_end_unix <- df$author_date_unix_timestamp[total_changes]
  time_end_date <- as.Date(as.POSIXct(time_end_unix, origin="1970-01-01"))
  days <- length(seq(from=time_start_date, to=time_end_date, by='day')) -1
  defects_per_day <- buggy/days
  commits_per_day <- total_changes/days
  bug_fixing_per_day <- total_bug_fixing/days
  
  if(first_flag){
    first_flag = F
    res_df <- data.frame(list(project,total_changes,buggy_ratio,time_start_date,time_end_date,defects_per_day,commits_per_day,bug_fixing_per_day))
    colnames(res_df) <- info
  }else{
    res_df[nrow(res_df)+1, ] <- list(project,total_changes,buggy_ratio,time_start_date,time_end_date,defects_per_day,commits_per_day,bug_fixing_per_day)
  }
  
}

res_df[nrow(res_df)+1,] <- list("average",mean(res_df$`total changes`),mean(res_df$`%defect-inducing changes`),"","",mean(res_df$`defects/day`),mean(res_df$`commits/day`),mean(res_df$`bug-fixing/day`))

# write.csv(res_df,'/media/lxt/TOSHIBA EXT/moa/r_script/result/datasetInformation.csv',row.names = F)
write.csv(res_df,'D:/work/real-world-evaluation/r_script/result/datasetInformation-origin.csv',row.names = F)


library('tibble')

first_flag = T
for(file in files){
  project <- substr(file,3,nchar(file)-4)
  df <- read.csv(file)
  indicators <- c('la','ld','lt')
  for(indicator in indicators){
    la <- summary(df[,indicator])
    if(first_flag){
      first_flag=F
      
      effort_df <- as.data.frame(as.array(la))
      colnames(effort_df) <- c('Var','Freq')
      effort_df <- add_column(effort_df,project=rep(project,6),indicator=rep(indicator,6),.before = 1)
    }else{
      temp <- as.data.frame(as.array(la))
      colnames(temp) <- c('Var','Freq')
      temp <- add_column(temp,project=rep(project,6),indicator=rep(indicator,6),.before = 1)
      effort_df <- rbind(effort_df,temp)
    }
  }
  
  
}

# write.csv(effort_df,'/media/lxt/TOSHIBA EXT/moa/r_script/result/datasetEffort.csv',row.names = F)
write.csv(effort_df,'D:/work/real-world-evaluation/r_script/result/datasetEffort-origin.csv',row.names = F)
