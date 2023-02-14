setwd('/media/lxt/TOSHIBA EXT/moa/')
data_path <- './commit_guru_dataset/'
files <- list.files(data_path,pattern = 'csv')
file <- files[1]
for(file in files){
  df <- read.csv(file.path(data_path,file))
  max_ts <- max(df$author_date_unix_timestamp)
  min_ts <- min(df$author_date_unix_timestamp)
  two_years <- 365*24*60*60*2
  sub_df <- df[df$author_date_unix_timestamp < max_ts - two_years,]
  colnames(sub_df)
  write.csv(sub_df, file.path( './commit_guru_dataset/cut2years/',file),row.names = F)
}

#csv to arff and change contain bugs from numeric to {0,1}