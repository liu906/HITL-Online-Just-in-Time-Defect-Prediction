setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

commit_guru_raw_data_preprocess <- function(file_name){
  
  df = read.csv(file.path('../raw_data/',file_name))
  df[,c('author_name','author_email',
        'author_date','commit_message','classification','fileschanged',
        'repository_id','issue_id','issue_date','issue_type',
        'glm_probability','rf_probability')] <- NULL
  
  df <- df[df$fix %in% c("True","False"),]
  nrow(df)
  df[1:10,]
  colnames(df)
  nrow(df)
  df$classification <- NULL
  df$linked <- NULL
  rownames(df)
  df$fix_unix_timestamp <- -1
  
  for(idx in nrow(df):1){
    if(df[idx,'contains_bug']=='True'){
      
      fix_commit_hash = strsplit(df[idx,'fixes'],'\"')[[1]][2]
      df[idx,'fix_unix_timestamp'] = df[df$commit_hash==fix_commit_hash,'author_date_unix_timestamp']
      
    }
  }
  
  write.csv(df,file.path('../dataset',file_name),row.names = FALSE)

}

commit_guru_raw_data_preprocess(file_name = 'test.csv')
