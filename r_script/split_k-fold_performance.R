setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(dplyr)

file_name = 'camel'
result_root = './result/temp/'

file_path = file.path('../moaGUIresult/camelDumpFile_delayedCV_10fold_detail_1.csv')
df = read.csv(file_path, check.names = FALSE)
df$fold = as.factor(df$fold)
 
folds = unique(df$fold)


for(i in 2:ncol(df)-1){
  indicator = colnames(df)[i]
  sub_df = df[,c(colnames(df)[1],indicator,'fold')]
  first_flag = TRUE
  
  for(ifold in folds){
    temp_df = sub_df[sub_df$fold==ifold,]
    colnames(temp_df)[2] = paste(ifold,'fold',sep = '-')
    temp_df$fold = NULL
    if(first_flag){
      total_df = temp_df
      first_flag = FALSE
    }else{
      
      temp_df$`learning evaluation instances on certain fold`=NULL
      total_df =  cbind(total_df,temp_df)
    }
  }
  write.csv(total_df,file = file.path(result_root,paste(file_name,'-',indicator,'.csv',sep = '')),row.names = FALSE)
  
}
