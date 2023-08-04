#6.3.1#################################
#######################################
library(dplyr)
df <- read.csv("D:/work/real-world-evaluation/r_script/RQ3/validity_metric_new.csv",check.names = F)
df <- df[df$indicator=='[avg] Gmean for recall  (percent)',]

first_flag = T
for(idx in 1:nrow(df)){
  file <- df[idx,'estimate_scenario']
  # project <- strsplit(file,'_')[[1]][1]
  scenario <- strsplit(file,'_')[[1]][1]
  SQA_day <- strsplit(file,'_')[[1]][2]
  BFC_day <- strsplit(file,'_')[[1]][3]
  BFC_day <- strsplit(BFC_day,'.csv')[[1]]
  if(scenario=='Ideal.csv'|| SQA_day!=BFC_day){
    next
  }
  df[idx,]
  temp <- c(project=df[idx,'project'],SQA_day=SQA_day,BFC_day=BFC_day,scenario=scenario,validity=df[idx,'1000'])
  if(first_flag){
    first_flag <- F
    res_df <- temp
  
  }else{
    res_df <- rbind(res_df,temp)
  }
}
res_df <- as.data.frame(res_df)
df_auto <- res_df[res_df$scenario=='Extension',]
df_hitl <- res_df[res_df$scenario=='PosNegWindow',]



df$estimate_scenario
df_join <- left_join(df_auto, df_hitl, by = c("project", "SQA_day", "BFC_day"))

df_join <- df_join[order(as.numeric(df_join$SQA_day)), ]

colnames(df_join)[colnames(df_join)=='validity.x'] = 'Auto'
colnames(df_join)[colnames(df_join)=='validity.y'] = 'HITL'

df_join$`scenario.x` <- NULL
df_join$`scenario.y` <- NULL

write.csv(df_join,file="D:/work/real-world-evaluation/r_script/RQ3/validity_HITLvsAuto/HITLvsAuto.csv",quote = F,row.names = F)

projects <- unique(df$project)
for (project in projects) {
  sub_df <- df_join[df_join$project==project,]
  sub_df <- rbind(rep(sub_df$project[1],ncol(sub_df)),sub_df)
  SQA_day <- sub_df$SQA_day
  sub_df$SQA_day <- NULL
  
  sub_df <- cbind(`waiting time (day)`=SQA_day,sub_df)
  write.csv(sub_df,
            file = file.path('D:/work/real-world-evaluation/r_script/RQ3/validity_HITLvsAuto/',paste("HITLvsAuto",project,".csv",sep='_')),
            row.names = F,
            quote = F)
}


#6.3.2#################################
#######################################


percentages <- c('1000','2000','3000','4000','5000')
cate <- c("PosNegWindow_7_15.csv", "PosNegWindow_15_15.csv","Extension_15_15.csv")


first_flag = T
for(idx in 1:nrow(df)){
  file <- df[idx,'estimate_scenario']
  # project <- strsplit(file,'_')[[1]][1]
  scenario <- strsplit(file,'_')[[1]][1]
  SQA_day <- strsplit(file,'_')[[1]][2]
  BFC_day <- strsplit(file,'_')[[1]][3]
  BFC_day <- strsplit(BFC_day,'.csv')[[1]]
  if(!(file %in% cate)){
    next
  }
  
  for(commit in percentages){
    temp <- c(project=df[idx,'project'],file=file,commit=commit,SQA_day=SQA_day,BFC_day=BFC_day,scenario=scenario,validity=df[idx,commit])
    if(first_flag){
      first_flag <- F
      res_df <- temp
      
    }else{
      res_df <- rbind(res_df,temp)
    }
  }
}


res_df <- as.data.frame(res_df)
df_auto <- res_df[res_df$file=='Extension_15_15.csv',]
df_hitl <- res_df[res_df$file=='PosNegWindow_15_15.csv',]
df_hitl_faster <- res_df[res_df$file=='PosNegWindow_7_15.csv',]


projects <- unique(df$project)
for (project in projects) {
  sub_df <- res_df[res_df$project==project,]
  sub_df <- cbind(`#commit`=sub_df$commit,sub_df)
  
  
  sub_df <- rbind(rep(sub_df$project[1],ncol(sub_df)),sub_df)
  write.csv(sub_df,
            file = file.path('D:/work/real-world-evaluation/r_script/RQ3/validity_HITLvsAuto_2/',paste("HITLvsAuto",project,".csv",sep='_')),
            row.names = F,
            quote = F)
}




