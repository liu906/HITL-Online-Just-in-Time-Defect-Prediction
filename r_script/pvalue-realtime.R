setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('mcnemar.R')
setwd('D:/work/real-world-evaluation/r_script/result/HITLvsNoHITL-detail')
library(ggplot2)

performance_realtime <- function(file1,file2,res_name,project){
  
  indicator <- '[avg] Gmean for recall  (percent)'
  
  df1 <- read.csv(file1,check.names = F)
  df2 <- read.csv(file2,check.names = F)
  # mcnemar_res <- McNemar( df1[df1$fold==0,c('fold','TP','FP','TN','FN')],df2[df2$fold==1,c('fold','TP','FP','TN','FN')],1,F)
  # mcnemar_res > 3.8414588
  
  ts1 <- unique(df1$`current timestamp`)
  ts2 <- unique(df2$`current timestamp`)
  
  ts_list <- intersect(ts1,ts2)
  stop=1000
  first_flag = T
  for (ts in ts_list) {
    if(stop<0){
      break
    }
    stop <- stop - 1
    sub1 <- df1[df1$`current timestamp`==ts,indicator]
    sub2 <- df2[df1$`current timestamp`==ts,indicator]
    pvalue <- wilcox.test(sub1,sub2,paired=F)$p.value
    if(!is.na(pvalue)&&pvalue<0.05){
      if(mean(sub1)>mean(sub2)){
        answer = 'non-HITL is better'
      }else{
        answer = 'HITL is better'
      }
    }else{
      answer = 'no significant difference'
    }
    
    if(first_flag){
      first_flag <- F
      res <- data.frame(ts=ts,pvalue=pvalue,answer=answer)
    }else{
      res <- rbind(res,data.frame(ts=ts,pvalue=pvalue,answer=answer))
    }
  }
  # install.packages("extrafont")
  
  # 创建你的数据框
  data <- res
  
  # 为每个answer值分配颜色
  color_mapping <- c("non-HITL is better" = "#F35E5A", 
                     "HITL is better" = "#0000DD",
                     "no significant difference" = "#B0B0B0")
  
  # 创建一个计数列用于堆叠
  data$counter <- 1:nrow(data)
  
  # 使用ggplot绘制堆叠条形图（条形码）
  my_plot <- ggplot(data, aes(x = counter, fill = answer,y=0.1)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = project,
         x = "#commit",
         y = "") +
    scale_fill_manual(values = color_mapping) +
    theme_minimal() +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          text = element_text(family = "Times New Roman",size = 16),
          plot.title = element_text(size = 12)
    )+
    labs(fill = "wilcoxon")
  my_plot
  ggsave(res_name, my_plot, width = 10, height = 1.5, units = "in")
}

pvalue_realtime <- function(file1,file2,res_name,project){
  indicators <- c('Recall for class 1 (percent)',
                  'Gmean for recall  (percent)',
                  'FPR for class 1 (percent)')
  indicator <- 'Gmean for recall  (percent)'
  
  df1 <- read.csv(file1,check.names = F)
  df2 <- read.csv(file2,check.names = F)
  # mcnemar_res <- McNemar( df1[df1$fold==0,c('fold','TP','FP','TN','FN')],df2[df2$fold==1,c('fold','TP','FP','TN','FN')],1,F)
  # mcnemar_res > 3.8414588
  
  ts1 <- unique(df1$`current timestamp`)
  ts2 <- unique(df2$`current timestamp`)
  
  ts_list <- intersect(ts1,ts2)
  stop=1000
  first_flag = T
  for (ts in ts_list) {
    if(stop<0){
      break
    }
    stop <- stop - 1
    sub1 <- df1[df1$`current timestamp`==ts,indicator]
    sub2 <- df2[df1$`current timestamp`==ts,indicator]
    pvalue <- wilcox.test(sub1,sub2,paired=F)$p.value
    if(!is.na(pvalue)&&pvalue<0.05){
      if(mean(sub1)>mean(sub2)){
        answer = 'non-HITL is better'
      }else{
        answer = 'HITL is better'
      }
    }else{
      answer = 'no significant difference'
    }
    
    if(first_flag){
      first_flag <- F
      res <- data.frame(ts=ts,pvalue=pvalue,answer=answer)
    }else{
      res <- rbind(res,data.frame(ts=ts,pvalue=pvalue,answer=answer))
    }
  }
  # install.packages("extrafont")
  
  # 创建你的数据框
  data <- res
  
  # 为每个answer值分配颜色
  color_mapping <- c("non-HITL is better" = "#F35E5A", 
                     "HITL is better" = "#0000DD",
                     "no significant difference" = "#B0B0B0")
  
  # 创建一个计数列用于堆叠
  data$counter <- 1:nrow(data)
  
  # 使用ggplot绘制堆叠条形图（条形码）
  my_plot <- ggplot(data, aes(x = counter, fill = answer,y=0.1)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = project,
         x = "#commit",
         y = "") +
    scale_fill_manual(values = color_mapping) +
    theme_minimal() +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          text = element_text(family = "Times New Roman",size = 16),
          plot.title = element_text(size = 12)
    )+
    labs(fill = "wilcoxon")
  my_plot
  ggsave(res_name, my_plot, width = 10, height = 1.5, units = "in")
}






dir.create('img',showWarnings = F)

file1 <- '0_brackets_trees.HoeffdingTree_EvaluatePrequentialDelayedCVExtension_15_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
file2 <- '0_brackets_trees.HoeffdingTree_EvaluatePrequentialDelayedCVPosNegWindow_7_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
res_name <- 'img/brackets_pvalue_realtime.svg'
pvalue_realtime(file1,file2,res_name,'brackets')

file1 <- '0_git(master)_trees.HoeffdingTree_EvaluatePrequentialDelayedCVExtension_15_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
file2 <- '0_git(master)_trees.HoeffdingTree_EvaluatePrequentialDelayedCVPosNegWindow_7_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
res_name <- 'img/git(master)_pvalue_realtime.svg'
pvalue_realtime(file1,file2,res_name,'git(master)')

file1 <- '0_vlc(master)_trees.HoeffdingTree_EvaluatePrequentialDelayedCVExtension_15_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
file2 <- '0_vlc(master)_trees.HoeffdingTree_EvaluatePrequentialDelayedCVPosNegWindow_7_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
res_name <- 'img/vlc(master)_pvalue_realtime.svg'
pvalue_realtime(file1,file2,res_name,'vlc(master)')
