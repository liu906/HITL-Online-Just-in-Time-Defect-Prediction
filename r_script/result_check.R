setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

projects <- c(
  'FFmpeg',
  'brackets',
  'camel',
  'edx-platform',
  'elasticsearch',
  'git',
  'kubernetes',
  'mindspore',
  'tensorflow',
  'vlc'
)


folders <- list.files('../experimentResult/')

write(paste('raw_file','res_file','nrow_raw','nrow_res','ratio',sep = ','), file="./result/checkNrow.csv", append=T)
total_res <- data.frame(matrix(nrow = 0,ncol = 5))
colnames(total_res) <- c('raw_file','res_file','nrow_raw','nrow_res','ratio')
for(folder in folders){
  if(startsWith(folder,'RQ1') && endsWith(folder,'0.05')){
    
    files <- list.files(file.path('../experimentResult/',folder),pattern = 'detail.csv')
    for(project in projects){
      sub_files <- files[startsWith(files,project)]
      datasets <- list.files('../commit_guru_dataset/',pattern = 'csv')
      raw_df_name <- datasets[startsWith(datasets,paste('0',project,sep='_'))]
      raw_df <- read.csv(file.path('../commit_guru_dataset/',raw_df_name))
      nrow_raw = nrow(raw_df)
      for(sub_file in sub_files){
        sub_file_name <- file.path('../experimentResult/',folder,sub_file)
        nrow_res = nrow(read.csv(sub_file_name))
        total_res[nrow(total_res)+1,] <- c(raw_df_name,sub_file_name,nrow_raw,nrow_res,nrow_res/nrow_raw)
        write(paste(raw_df_name,sub_file_name,nrow_raw,nrow_res,nrow_res/nrow_raw,sep = ','), file="./result/checkNrow.csv", append=T)
      }
    }
  }
}

# write.csv(total_res,'./result/checkNrow.csv',row.names = FALSE)

