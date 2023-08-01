setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
root_path <- './result/differentEvaluationProcudure/meta.OOB/seed1-noise0'
indicators <- c('[avg] Gmean for recall  (percent)', '[avg] Recall for class 0 (percent)','[avg] Recall for class 1 (percent)')
#indicator <- indicators[1]
files <- list.files(root_path,pattern = 'dumpFile')
projects <- c()
for (file in files) {
  project <-strsplit(file,'_')[[1]][2]
  projects <- c(projects,project)
}
#projects <- unique(projects)

df <- data.frame(project = projects, file_path = files)
# 按照group列进行分组，并使用sum函数对每个组的value列进行求和
library(dplyr)

df_grouped <- df %>% group_by(project) 



# 将每个group拆分为一个独立的DataFrame，并存储在一个列表中
group_list <- df_grouped %>% group_split()

# 遍历group_list中的每个DataFrame，对每个group进行操作
for (group_df in group_list) {
  # 在这里执行您想要对每个group进行的操作，例如打印当前group的数据
  group_df <- as.data.frame(group_df) 
  for (indicator in indicators) {
    first_flag <- T
    
    for (idx in 1:nrow(group_df)) {
      project <- group_df[idx,"project"]
      performance_df <- read.csv(file.path(root_path,group_df[idx,"file_path"]),check.names = F)
      scenario = strsplit(group_df[idx,"file_path"],'_')[[1]][4]
      if(scenario!='EvaluatePrequentialDelayedCVIdeal'){
        day1 <- strsplit(group_df[idx,"file_path"],'_')[[1]][5]
        day2 <- strsplit(group_df[idx,"file_path"],'_')[[1]][6]
        scenario <- paste(scenario,day1,day2,sep = '_')
      }
      if(scenario %in% c('EvaluatePrequentialDelayedCVExtension_15_15',
                         'EvaluatePrequentialDelayedCVPosNegWindow_7_15',
                         'EvaluatePrequentialDelayedCVIdeal')){
        sub_df <- data.frame(instance=performance_df[,"learning evaluation instances"],scenario=scenario,indicator=performance_df[,indicator])
        colnames(sub_df)[3] <- indicator
        if(first_flag){
          first_flag <- F
          res <- sub_df
        }else{
          res <- rbind(res,sub_df)
        }
        
      }
      
    }
    
    write.csv(res,file.path('result','RQ1',paste(indicator,project,'.csv',sep='_')),row.names = F)
  }
  
  
}
