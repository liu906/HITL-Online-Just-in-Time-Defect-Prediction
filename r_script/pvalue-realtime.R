setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('mcnemar.R')
setwd('D:/work/real-world-evaluation/r_script/result/HITLvsNoHITL-detail')

file1 <- '0_brackets_trees.HoeffdingTree_EvaluatePrequentialDelayedCVExtension_15_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
file2 <- '0_brackets_trees.HoeffdingTree_EvaluatePrequentialDelayedCVPosNegWindow_7_15_seed1_Bootstrap-Validation_10Fold_FF0.99_detail.csv'
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

# 创建你的数据框
data <- data.frame(
  ts = c(1325780207, 1325780844, 1325780988, 1325786994, 1325805311, 1325806364, 1325806490, 1325806657, 1325810062, 1325813825, 1325814717),
  pvalue = c(6.900010e-07, 1.044843e-06, 2.683983e-06, 4.356487e-05, 2.474691e-04, 1.445133e-03, 2.641983e-03, 1.697025e-02, 1.442792e-03, 1.676761e-03, 8.685361e-05),
  answer = c("non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better", "non-HITL is better")
)
data <- res

# 为每个answer值分配颜色
color_mapping <- c("non-HITL is better" = "red", "HITL is better" = "blue", "no significant difference" = "green")

# 创建一个计数列用于堆叠
data$counter <- 1:nrow(data)

# 使用ggplot绘制堆叠条形图（条形码）
ggplot(data, aes(x = counter, fill = answer,y=0.1)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Chart (Barcode) of Answer Over Rows",
       x = "Row Number",
       y = "Count") +
  scale_fill_manual(values = color_mapping) +
  theme_minimal()


