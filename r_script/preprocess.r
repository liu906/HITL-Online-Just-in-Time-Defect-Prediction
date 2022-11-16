setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

commit_guru_raw_data_preprocess <- function(file_name,delay_time_interval){
  
  df = read.csv(file.path('../raw_data/',file_name))
  df[,c('author_name','author_email',
        'author_date','commit_message','classification','fileschanged',
        'repository_id','issue_id','issue_date','issue_type',
        'glm_probability','rf_probability')] <- NULL
  
  df <- df[df$fix %in% c("TRUE","FALSE"),]
  
  
  # df$classification <- NULL
  # df$linked <- NULL
  # rownames(df)
  df$fix_unix_timestamp <- -1
  
  for(idx in nrow(df):1){
    if(df[idx,'contains_bug']=='TRUE'){
      
      fix_commit_hash = strsplit(df[idx,'fixes'],'\"')[[1]][2]
      df[idx,'fix_unix_timestamp'] = df[df$commit_hash==fix_commit_hash,'author_date_unix_timestamp']
      
      
    }
  }
  
  #todo time window
  
  df[df$fix=="TRUE",'fix'] = 1
  df[df$fix=="FALSE",'fix'] = 0
  df[df$contains_bug=="TRUE",'contains_bug'] = 1
  df[df$contains_bug=="FALSE",'contains_bug'] = 0
  
  df = df[,c('author_date_unix_timestamp','fix_unix_timestamp','fix','ns','nd','nf','entropy','la','ld','lt','ndev','age','nuc','exp','rexp','sexp','contains_bug')]
  df = df[rowSums(df=="")==0, ]
  
  df$author_date_unix_timestamp = as.numeric(df$author_date_unix_timestamp)
  df$fix_unix_timestamp = as.numeric(df$fix_unix_timestamp)
  
  
  if (delay_time_interval>=0){
    df[df[,'fix_unix_timestamp'] - df[,'author_date_unix_timestamp']<=delay_time_interval & df$contains_bug==1,'contains_bug']=1
    
  }
  df$fix_unix_timestamp=NULL
  
  df = df[order(df$author_date_unix_timestamp),]  
  
  
  
  write.csv(df,file.path('../datasets',paste(delay_time_interval,file_name,sep='_')),row.names = FALSE)

}

commit_guru_raw_data_preprocess(file_name = 'test.csv',delay_time_interval = 60*24*90)





