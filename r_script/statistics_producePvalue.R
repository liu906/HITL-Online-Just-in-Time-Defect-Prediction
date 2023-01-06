library(nonpar)


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

setwd(dir = '/media/lxt/TOSHIBA EXT/moa/r_script/')

producePvalue <- function(file_path){
  df <- read.csv(file = file_path,row.names = 1,check.names = F)
  
  folds = unique(df$fold)
  datasets = unique(df$dataset)
  pair_length = length(folds) * length(datasets)
  indicators <- unique(df$indicator)
  sample_intervals <- unique(df$`#instances`)
  for(indicator in indicators){
    for(sample_interval in sample_intervals){
      sub_df <- df[df$indicator == indicator & df$`#instances` == sample_interval, ]
      if(nrow(sub_df) < 30){
        # cat(nrow(sub_df),'\n')
        next
      }else{
        pvalue <- wilcox.test(sub_df[,5],sub_df[,6],paired = T,conf.level = conf_level)$p.value
        
        wline <- paste(sample_interval,indicator,colnames(sub_df)[5],colnames(sub_df)[6],'wilcox',pvalue,sep = ',')
        write(wline, file=res_file_path, append=T)
        pvalue_str <- signtest(sub_df[,5]-sub_df[,6], m=0, conf.level=conf_level, exact=FALSE)
        pvalue <- substr(pvalue_str@PVal,nchar("The p-value is   "),nchar(pvalue_str@PVal))
        wline <- paste(sample_interval,indicator,colnames(sub_df)[5],colnames(sub_df)[6],'signtest',pvalue,sep = ',')
        write(wline, file=res_file_path, append=T)
    
      }
    }
  }
}
  
  
setwd('../experimentResult')
getwd()
res_file_path <- 'RQ3/pvalue.csv'
write(paste('#instances','indicator','col1','col2','statistic','pvalue',sep = ','), file=res_file_path, append=F)
conf_level = 0.95

producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed10_RQ1-seed10-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed10_RQ1-seed10-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed10_RQ1-seed11_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed11_RQ1-seed11-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed11_RQ1-seed11-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed2_RQ1-seed2-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed2_RQ1-seed2-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed2_RQ1-seed3_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed3_RQ1-seed3-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed3_RQ1-seed3-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed4_RQ1-seed4-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed4_RQ1-seed4-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed4_RQ1-seed5_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed5_RQ1-seed5-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed5_RQ1-seed5-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed6_RQ1-seed6-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed6_RQ1-seed6-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed6_RQ1-seed7_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed7_RQ1-seed7-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed7_RQ1-seed7-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed8_RQ1-seed8-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed8_RQ1-seed8-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed8_RQ1-seed9_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed9_RQ1-seed9-noise0.05_DelayedCVIdeal_5Fold_FF0.99_detail.csv')
producePvalue(file_path = 'RQ3/paired_performance_RQ1-seed9_RQ1-seed9-noise0.1_DelayedCVIdeal_5Fold_FF0.99_detail.csv')

df <- read.csv(res_file_path,check.names = F)
indicators <- unique(df$indicator)
summary_res_file <- 'RQ3/pvalue_summary.csv'
if(!file.exists(summary_res_file)){
  write(paste('comparison','indicator','statistic','ratio_rejectN0',sep=','),file=summary_res_file,append = F)
}


for(indicator in indicators){
  sub_df <- df[df$indicator== indicator, ]
  sub_df.noise0.1 <- sub_df[endsWith(sub_df$col2,'noise0.1'),]
  for(statistic in unique(df$statistic)){
    item_df <- sub_df.noise0.1[sub_df.noise0.1$statistic==statistic,'pvalue'] 
    ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
    one <- paste('normal vs. noise0.1',indicator,statistic,ratio_rejectN0,sep = ',')
    write(one, file=summary_res_file, append=T)
  }
  sub_df.noise0.05 <- sub_df[endsWith(sub_df$col2,'noise0.05'),]
  for(statistic in unique(df$statistic)){
    item_df <- sub_df.noise0.05[sub_df.noise0.05$statistic==statistic,'pvalue'] 
    ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
    one <- paste('normal vs. noise0.05',indicator,statistic,ratio_rejectN0,sep = ',')
    write(one, file=summary_res_file, append=T)
  }
  
  
  sub_df.normal <- sub_df[!endsWith(sub_df$col2,'noise0.05') & !endsWith(sub_df$col2,'noise0.1'),]
  for(statistic in unique(df$statistic)){
    item_df <- sub_df.normal[sub_df.normal$statistic==statistic,'pvalue'] 
    ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
    one <- paste('normal vs. normal',indicator,statistic,ratio_rejectN0,sep = ',')
    write(one, file=summary_res_file, append=T)
  }
}
