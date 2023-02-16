setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

commit_guru_raw_data_preprocess <- function(file_name,delay_time_interval,root,target_path){
  
  df = read.csv(file.path(root,file_name))
  df[,c('author_name','author_email',
        'author_date','commit_message','classification','fileschanged',
        'repository_id','issue_id','issue_date','issue_type',
        'glm_probability','rf_probability')] <- NULL
  
  df <- df[df$fix %in% c("True","False"),]
  
  
  # df$classification <- NULL
  # df$linked <- NULL
  # rownames(df)
  df$fix_unix_timestamp <- -1
  df$fix
  
  for(idx in nrow(df):1){
    if(df[idx,'fixes']!=""){
      
      len = length(strsplit(df[idx,'fixes'],'\"')[[1]])
      i = 2
      while(i<len){
        fix_commit_hash = strsplit(df[idx,'fixes'],'\"')[[1]][i]
        df[df$commit_hash==fix_commit_hash,'fix_unix_timestamp'] = df[idx,'author_date_unix_timestamp']  
        i = i+2
      }
      # df[idx,'fix_unix_timestamp'] = df[df$commit_hash==fix_commit_hash,'author_date_unix_timestamp']
    }
  }
  
  #todo time window
  
  df[df$fix=="True",'fix'] = 1
  df[df$fix=="False",'fix'] = 0
  df[df$contains_bug=="True",'contains_bug'] = 1
  df[df$contains_bug=="False",'contains_bug'] = 0
  
  df = df[,c('author_date_unix_timestamp','fix_unix_timestamp','fix','ns','nd','nf','entropy','la','ld','lt','ndev','age','nuc','exp','rexp','sexp','contains_bug')]
  df = df[rowSums(df=="")==0, ]
  
  df$author_date_unix_timestamp = as.numeric(df$author_date_unix_timestamp)
  df$fix_unix_timestamp = as.numeric(df$fix_unix_timestamp)
  df$fix = as.numeric(df$fix)
  df$contains_bug = as.numeric(df$contains_bug)
  
  if (delay_time_interval>0){
    
    df[(df[,'fix_unix_timestamp'] - df[,'author_date_unix_timestamp']<=delay_time_interval) & df$contains_bug==1,'contains_bug']=2
    df[df$contains_bug!=2,'contains_bug'] = 0
   
    df[df$contains_bug==2,'contains_bug'] = 1
    
  }
  # df$fix_unix_timestamp=NULL
  df = df[order(df$author_date_unix_timestamp),]  
  
  write.csv(df,file.path(target_path,paste(delay_time_interval,file_name,sep='_')),row.names = FALSE,quote = F)

}


# commit_guru_raw_data_preprocess(file_name = 'test.csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')

commit_guru_raw_data_preprocess(file_name = 'FFmpeg(master)-1.csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'brackets.csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'camel(camel-2.20.x).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'edx-platform(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'elasticsearch(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'git(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'kubernetes(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'mindspore(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'tensorflow(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'vlc(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')

commit_guru_raw_data_preprocess(file_name = 'cppcheck(main).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'gerrit(stable-2.12).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'gimp(master).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')
commit_guru_raw_data_preprocess(file_name = 'pip(main).csv',delay_time_interval = 0,root='../commit_guru/',target_path='../commit_guru_dataset/')


sum(df$contains_bug)


