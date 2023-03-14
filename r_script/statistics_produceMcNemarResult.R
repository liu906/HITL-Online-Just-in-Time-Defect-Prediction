
threshold <- 3.8414588

summary_res_file <- 'RQ3/pvalue_summary.csv'
if(!file.exists(summary_res_file)){
  write(paste('scenario','comparison','indicator','statistic','ratio_rejectN0',sep=','),file=summary_res_file,append = F)
}
files <- list.files('RQ3')
files <- files[startsWith(files,'mcnemar')]
first_flag = T
for(file in files){
  sub_df <- read.csv(file.path('RQ3',file),check.names = F)
  if(first_flag){
    df <- sub_df
    first_flag <- F
  }else{
    colnames(df)
    colnames(sub_df)
    df <- rbind(df,sub_df)
  }
}



statistic <- 'mcnemar'
indicator <- ''

for(scenario in unique(df$scenario)){
  sub_df <- df[df$scenario == scenario,]
  sub_df.noise0.1 <- sub_df[endsWith(sub_df$folder2,'noise0.1'),]
  item_df <- sub_df.noise0.1[,'mcnemar'] 
  ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
  one <- paste(scenario,'normal vs. noise0.1',indicator,statistic,ratio_rejectN0,sep = ',')
  write(one, file=summary_res_file, append=T)
  
  sub_df.noise0.05 <- sub_df[endsWith(sub_df$folder2,'noise0.05'),]
  
  item_df <- sub_df.noise0.05[,'mcnemar'] 
  ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
  one <- paste(scenario,'normal vs. noise0.05',indicator,statistic,ratio_rejectN0,sep = ',')
  write(one, file=summary_res_file, append=T)
  
  
  
  sub_df.normal <- sub_df[!endsWith(sub_df$folder2,'noise0.05') & !endsWith(sub_df$folder2,'noise0.1'),]
  
  item_df <- sub_df.normal[,'mcnemar'] 
  ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
  one <- paste(scenario,'normal vs. normal',indicator,statistic,ratio_rejectN0,sep = ',')
  write(one, file=summary_res_file, append=T)
  
}


# library(tibble)
# files <- list.files('RQ3')
# files <- files[startsWith(files,'mcnemar')]
# files <- files[endsWith(files,'DelayedCVIdeal_5Fold_FF0.99_detail.csv')]
# for(file in files){
#   colnames(df)
#   df <- read.csv(file = file.path('RQ3',file),check.names = F)
#   df <- add_column(df, 'scenario' = 'DelayedCVIdeal', .before = 1)
#   write.csv(df,file = file.path('RQ3',file),row.names = F)
# }


