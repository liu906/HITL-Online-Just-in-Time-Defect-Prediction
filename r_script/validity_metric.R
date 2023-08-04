
compute_validity_metric <- function(ideal_evaluation, estimated_evaluation){
  return(100 - abs(ideal_evaluation-estimated_evaluation))
  # return(-abs(ideal_evaluation-estimated_evaluation)/ideal_evaluation)
}


# percentages <- seq(0.1,1,0.1)
# indicators = c('[avg] Recall for class 1 (percent)',
#                '[avg] Kappa Recall Temporal Statistic 1 (percent)',
#                '[avg] Gmean for recall  (percent)',
#                '[avg] Kappa Gmean Temporal Statistic  (percent)',
#                '[avg] FPR for class 1 (percent)',
#                '[avg] Kappa FPR Temporal Statistic 1 (percent)')

indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] FPR for class 1 (percent)')


res_root <- "D:/work/real-world-evaluation/r_script/result/differentEvaluationProcudure/trees.HoeffdingTree/seed1-noise0/dumpFile/"    
files <- list.files(res_root,pattern = 'csv$')
# dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years/key_timestamp/'
dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years/key_timestamp/top5000Commits/'
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
  percentages <- key_timestamp_df$percentages
  project.ideal.file <- project.files[grep('Ideal',project.files)]
  project.estimate.files <- project.files[!grepl('Ideal',project.files)]
  first_estimate <- T
  for(project.estimate.file in project.estimate.files){
    df.ideal <- read.csv(file.path(res_root,project.ideal.file),check.names = F)
    df.estimate <- read.csv(file.path(res_root,project.estimate.file),check.names = F)
    df.estimate.idxs <- c()
    df.ideal.idxs <- c()
    for (ts in key_timestamp_df$timestamp) {
      
      df.ideal.idxs <- append(df.ideal.idxs,which.min(abs(df.ideal$`current timestamp` - ts)))
      df.estimate.idxs <- append(df.estimate.idxs,which.min(abs(as.numeric(df.estimate$`current timestamp`) - ts)))
    }
    
    
    # df.estimate.idxs <- as.integer(nrow(df.estimate)*percentages) 
    # df.ideal.idxs <- as.integer(nrow(df.ideal)*percentages)  
    
    
    
    for(indicator in indicators){
      validity <- compute_validity_metric(as.numeric(df.ideal[df.ideal.idxs,indicator]),
                                          as.numeric(df.estimate[df.estimate.idxs,indicator]))
      ideal_validity <- compute_validity_metric(df.ideal[df.ideal.idxs,indicator],df.ideal[df.ideal.idxs,indicator])
      #pvalue <- wilcox.test(df.ideal[df.ideal.idxs,indicator],df.estimate[df.estimate.idxs,indicator],paired = T)$p.value
      #cd <- cohen.d(df.ideal[df.ideal.idxs,indicator],df.estimate[df.estimate.idxs,indicator],paired = T)
      #c(pvalue,cd$estimate,cd$magnitude)
      temp <- c(project,project.ideal.file,project.estimate.file,substr(project.estimate.file,nchar(project)+2,nchar(project.estimate.file)),indicator,validity)
      
      
      if(first_flag){
        total_result <- data.frame(matrix(nrow=0,ncol = length(temp)))
        colnames(total_result) <- c('project','ideal','estimate','estimate_scenario','indicator',percentages)
        total_result[nrow(total_result)+1,] <- temp
        first_flag <- F
      }else{
        total_result[nrow(total_result)+1,] <- temp
      }
      if(first_estimate){
        total_result[nrow(total_result)+1,] <- c(project,project.ideal.file,project.ideal.file,substr(project.ideal.file,nchar(project)+2,nchar(project.ideal.file)),indicator,ideal_validity)
      }
    }
    first_estimate <- F
  }
}



# project=projects,Ideal='Ideal',indicator = unique(total_result$indicator)

write.csv(total_result,"D:/work/real-world-evaluation/r_script/RQ3/validity_metric_new.csv",row.names = F)
###################################################################################################################
if(T){
   
  total_result <- rbind(total_result[grepl('PosNegWindow_90', total_result$estimate_scenario), ],
                        total_result[grepl('Ideal', total_result$estimate_scenario), ])

  # sel <- c("PosNegWindow_3_90.csv", "PosNegWindow_15_90.csv", "PosNegWindow_30_90.csv", "PosNegWindow_7_90.csv", "Extension_90_90.csv","Ideal.csv")
  # sel <- c("PosNegWindow_3_90.csv", "PosNegWindow_7_90.csv", "PosNegWindow_15_90.csv", 
  #          "PosNegWindow_30_90.csv","PosNegWindow_60_90.csv","PosNegWindow_90_90.csv", "Ideal.csv")
  
  # sel <- c("PosNegWindow_3_90.csv", "PosNegWindow_7_90.csv", "PosNegWindow_30_90.csv",
  #          "PosNegWindow_60_90.csv","PosNegWindow_90_90.csv", "PosNegWindow_15_3.csv","PosNegWindow_15_7.csv", 
  #          "PosNegWindow_15_15.csv", "PosNegWindow_15_30.csv","PosNegWindow_15_60.csv", 
  #          "PosNegWindow_15_90.csv","Ideal.csv")
  # total_result <- total_result[total_result$estima'BFC',te_scenario %in% sel,]
  res_path <- "D:/work/real-world-evaluation/r_script/RQ3/BFC_validity_metric_skESD_new.csv"
}else{
  total_result <- rbind(total_result[grepl('90.csv', total_result$estimate_scenario), ],
                        total_result[grepl('Ideal', total_result$estimate_scenario), ])
  res_path <- "D:/work/real-world-evaluation/r_script/RQ3/SQA_validity_metric_skESD_new.csv"
}
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
    
    
    colnames(new_df) <- names(col2_list)
    df_numeric <- as.data.frame(apply(new_df, 2, as.numeric))
    df_numeric[df_numeric == -Inf] <- 0
    # if(grepl('FPR', indicator, fixed = TRUE)){
    #   df_numeric = -df_numeric
    # }
    sk_res <- sk_esd(df_numeric, version = 'np')
    diff <- checkDifference(sk_res$groups, df_numeric)
    effect_size_i <- diff[diff$i=="Ideal.csv",]
    effect_size_j <- diff[diff$j=="Ideal.csv",]
    sk_res <- as.data.frame(sk_res$groups)
    colnames(sk_res) <- indicator
    sk_res[effect_size_j$i,paste(indicator,'mag',sep = '-')] <- effect_size_j$mag
    sk_res[effect_size_i$j,paste(indicator,'mag',sep = '-')] <- effect_size_i$mag
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



write.csv(res[res$perc %in% percentages,],res_path,row.names = T)
###################################################################################################################################


df <- read.csv("D:/work/real-world-evaluation/r_script/RQ3/validity_metric_new.csv",check.names = F)

col_name <- '1000'
indicator = '[avg] Gmean for recall  (percent)'
res_root <- "D:/work/real-world-evaluation/r_script/RQ3/split_validity/"
dir.create(res_root,recursive = T,showWarnings = F) 

df <- cbind(df[,1:5],df[,col_name])
df <- df[df$estimate_scenario!='Ideal.csv',]
df <- df[df$indicator==indicator,]
projects <- unique(df$project)
for (project in projects) {
  sub_df <- df[df$project==project,]
  for (idx in 1:nrow(sub_df)) {
    SQA_waitingtime <- strsplit(sub_df$estimate_scenario,'_')[[idx]][2]
    BFC_waitingtime <- strsplit(sub_df$estimate_scenario,'_')[[idx]][3]
    BFC_waitingtime <- substr(BFC_waitingtime,1,nchar(BFC_waitingtime)-4)
    sub_df[idx,'SQA wt (day)'] <- as.numeric(SQA_waitingtime)
    sub_df[idx,'BFC wt (day)'] <- as.numeric(BFC_waitingtime)
  }
  sub_df_sorted <- sub_df[order(as.numeric(sub_df$`SQA wt (day)`)), ]
  sub_df_sorted <- sub_df_sorted[as.numeric(sub_df_sorted$`BFC wt (day)`)==15,]
  sub_df_sorted <- cbind(`SQA wt (day)`=sub_df_sorted$`SQA wt (day)`,sub_df_sorted)
  colnames(sub_df_sorted)[colnames(sub_df_sorted)=='df[, col_name]']='validity'
  sub_df_sorted <- rbind(rep(sub_df_sorted$project[1],ncol(sub_df_sorted)),sub_df_sorted )
  
  write.csv(sub_df_sorted,
            file=file.path(res_root,paste('SQA(BFC15)',col_name,project,indicator,'.csv',sep = '_')),
            row.names = F,
            quote = F)
}



for (project in projects) {
  sub_df <- df[df$project==project,]
  for (idx in 1:nrow(sub_df)) {
    SQA_waitingtime <- strsplit(sub_df$estimate_scenario,'_')[[idx]][2]
    BFC_waitingtime <- strsplit(sub_df$estimate_scenario,'_')[[idx]][3]
    BFC_waitingtime <- substr(BFC_waitingtime,1,nchar(BFC_waitingtime)-4)
    sub_df[idx,'SQA wt (day)'] <- as.numeric(SQA_waitingtime)
    sub_df[idx,'BFC wt (day)'] <- as.numeric(BFC_waitingtime)
  }
  sub_df_sorted <- sub_df[order(as.numeric(sub_df$`BFC wt (day)`)), ]
  sub_df_sorted <- sub_df_sorted[as.numeric(sub_df_sorted$`SQA wt (day)`)==15,]
  sub_df_sorted <- cbind(`BFC wt (day)`=sub_df_sorted$`BFC wt (day)`,sub_df_sorted)
  colnames(sub_df_sorted)[colnames(sub_df_sorted)=='df[, col_name]']='validity'
  
  
  sub_df_sorted <- rbind(rep(sub_df_sorted$project[1],ncol(sub_df_sorted)),sub_df_sorted )
  
  write.csv(sub_df_sorted,
            file=file.path(res_root,paste('BFC(SQA=15)',col_name,project,indicator,'.csv',sep = '_')),
            row.names = F,
            quote = F)
}












