
compute_validity_metric <- function(ideal_evaluation, estimated_evaluation){
  #return(100 - abs(ideal_evaluation-estimated_evaluation))
  return(-abs(ideal_evaluation-estimated_evaluation)/ideal_evaluation)
}


percentages <- seq(0.1,1,0.1)
# indicators = c('[avg] Recall for class 1 (percent)',
#                '[avg] Kappa Recall Temporal Statistic 1 (percent)',
#                '[avg] Gmean for recall  (percent)',
#                '[avg] Kappa Gmean Temporal Statistic  (percent)',
#                '[avg] FPR for class 1 (percent)',
#                '[avg] Kappa FPR Temporal Statistic 1 (percent)')

indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] FPR for class 1 (percent)')


res_root <- "D:/work/real-world-evaluation/r_script/result/differentEvaluationProcudure/trees.HoeffdingTree/seed1-noise0/dump/"    
files <- list.files(res_root,pattern = 'csv$')
dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years/key_timestamp/'
projects <- c()
for(file in files){
  project <- strsplit(file,'_')[[1]][1]
  projects <- append(projects,project)
}
projects <- unique(projects)
first_flag <- T
for(project in projects){
  temp <- glob2rx(project)
  temp <- substring(temp, 1, nchar(temp)-1)
  project.files <- list.files(res_root,pattern = temp)
  temp <- substring(temp, 2, nchar(temp))
  key_timestamp_file <- list.files(dataset_path,pattern = temp)
  key_timestamp_df <- read.csv(file.path(dataset_path,key_timestamp_file))
  project.ideal.file <- project.files[grep('Ideal',project.files)]
  project.estimate.files <- project.files[!grepl('Ideal',project.files)]
  
  for(project.estimate.file in project.estimate.files){
    df.ideal <- read.csv(file.path(res_root,project.ideal.file),check.names = F)
    df.estimate <- read.csv(file.path(res_root,project.estimate.file),check.names = F)
    
    
    
    df.estimate.idxs <- as.integer(nrow(df.estimate)*percentages) 
    df.estimate$`learning evaluation instances on certain fold`
    df.ideal.idxs <- as.integer(nrow(df.ideal)*percentages)  
    
    # if(detail){
    #   # 按照 col1 列的取值将数据框分割为多个子数据框
    #   df_list <- split(df.ideal,df.ideal$fold)
    #   
    #   # 对每个子数据框进行操作，取出 col2 列
    #   col2_list <- lapply(df_list, function(x) select(x, indicator))
    #   unique(df.ideal$`current timestamp`)
    #   nrow(col2_list$`27`)
    #   # 将所有子数据框的 col2 列合并为一个新的数据框
    #   new_df <- bind_cols(col2_list)
    # }
    
    for(indicator in indicators){
      validity <- compute_validity_metric(df.ideal[df.ideal.idxs,indicator],df.estimate[df.estimate.idxs,indicator])
      pvalue <- wilcox.test(df.ideal[df.ideal.idxs,indicator],df.estimate[df.estimate.idxs,indicator],paired = T)$p.value
      cd <- cohen.d(df.ideal[df.ideal.idxs,indicator],df.estimate[df.estimate.idxs,indicator],paired = T)
      c(pvalue,cd$estimate,cd$magnitude)
      temp <- c(project,project.ideal.file,project.estimate.file,substr(project.estimate.file,nchar(project)+2,nchar(project.estimate.file)),indicator,validity)
      
      if(first_flag){
        total_result <- data.frame(matrix(nrow=0,ncol = length(temp)))
        colnames(total_result) <- c('project','ideal','estimate','estimate_scenario','indicator',percentages)
        total_result[nrow(total_result)+1,] <- temp
        first_flag <- F
      }else{
        total_result[nrow(total_result)+1,] <- temp
      }
    }
  }
}

sel <- c("PosNegWindow_3_90.csv", "PosNegWindow_15_90.csv", "PosNegWindow_30_90.csv", "PosNegWindow_7_90.csv", "Extension_90_90.csv")
total_result <- total_result[total_result$estimate_scenario %in% sel,]

write.csv(total_result,"D:/work/real-world-evaluation/r_script/RQ3/validity_metric.csv",row.names = F)

library(dplyr)
library(ScottKnottESD)
first_per <- T
for(percentage in percentages){
  first_flag <- T
  for(indicator in unique(total_result$indicator)){
    sub_total_result <- total_result[total_result$indicator==indicator,]
    # 按照 col1 列的取值将数据框分割为多个子数据框
    df_list <- split(sub_total_result,sub_total_result$estimate_scenario )
    # 对每个子数据框进行操作，取出 col2 列
    
    col2_list <- lapply(df_list, function(x) select(x, as.character(percentage)))
    # 将所有子数据框的 col2 列合并为一个新的数据框
    
    new_df <- bind_cols(col2_list)
    
    colnames(new_df) <- unique(sub_total_result$estimate_scenario)
    df_numeric <- as.data.frame(apply(new_df, 2, as.numeric))
    df_numeric[df_numeric == -Inf] <- 0
    if(grepl('FPR', indicator, fixed = TRUE)){
      df_numeric = -df_numeric
    }
    sk_res <- sk_esd(df_numeric, version = 'np')
    diff <- checkDifference(sk_res$groups, df_numeric)
    effect_size <- diff[diff$j=="PosNegWindow_7_90.csv",]
    sk_res <- as.data.frame(sk_res$groups)
    colnames(sk_res) <- indicator
    sk_res[effect_size$i,paste(indicator,'mag',sep = '-')] <- effect_size$mag
    if(first_flag){
      first_flag <- F
      total_sk_res <- sk_res
    }else{
      total_sk_res[rownames(sk_res),indicator] <- sk_res[,indicator]
      total_sk_res[rownames(sk_res),paste(indicator,'mag',sep = '-')] <- sk_res[,paste(indicator,'mag',sep = '-')]
    }
  }
  total_sk_res$perc <- percentage
  if(first_per){
    first_per <- F
    res <- total_sk_res
    
  }else{
    res <- rbind(res,total_sk_res)
  }
}

res 

write.csv(res,"D:/work/real-world-evaluation/r_script/RQ3/validity_metric_skESD.csv",row.names = T)
