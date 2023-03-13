#compute total la for clean commits and total la for buggy commits every month
setwd('/media/lxt/TOSHIBA EXT/moa/')
source('r_script/FadingFactor.r')

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


#time decayed feature values
for(file in files){
  df <- read.csv(file.path(data_path,file))
  timeDecayed_la <- ff2(arr=df$la,fadingFactor = 0.99)
  timeDecayed_ld <- ff2(arr=df$ld,fadingFactor = 0.99)
  timeDecayed_lt <- ff2(arr=df$lt,fadingFactor = 0.99)
  a <- c(1:length(timeDecayed_la))
  
  # Make a basic graph
  plot( timeDecayed_la~a , type="b" , bty="l" , xlab="instance" , ylab="performance" , col=rgb(0.2,0.4,0.1,0.7) , lwd=1 , pch=17)
  lines(timeDecayed_ld~a , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )
  lines(timeDecayed_lt~a , col=rgb(0.4,0.8,0.5,0.7) , lwd=3 , pch=19 , type="b" )
  
}