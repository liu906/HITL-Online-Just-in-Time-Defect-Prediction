
setwd('/media/lxt/TOSHIBA EXT/moa/')
file_path <- 'r_script/result/cutByCommitIndex/Bootstrap-Validation/trees.HoeffdingTreeClassifLeaves/seed2/'
files <- list.files(file_path,pattern='detail')
for(file in files){
  
  df <- read.csv(file.path(file_path,file),check.names = F)
  if(!('Date' %in% colnames(df))){
    cat('yes\n')
    Date <- as.POSIXct(df$`current timestamp`, origin = "1970-01-01",  tz = "UTC")
    
    new_df <- cbind(Date,df)
    write.csv(new_df,file.path(file_path,file),row.names = F,quote = F)
  }
}


