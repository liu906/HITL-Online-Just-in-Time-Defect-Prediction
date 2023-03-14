library(nonpar)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# setwd(dir = '/media/lxt/TOSHIBA EXT/moa/r_script/')

producePvalue <- function(file_path){
  df <- read.csv(file = file_path,row.names = 1,check.names = F)
 
  scenarios = unique(df$scenario)
  folds = unique(df$fold)
  datasets = unique(df$dataset)
  pair_length = length(folds) * length(datasets)
  indicators <- unique(df$indicator)
  sample_intervals <- unique(df$`#instances`)
  for(scenario in scenarios){
    for(indicator in indicators){
      for(sample_interval in sample_intervals){
        sub_df <- df[df$scenario == scenario & df$indicator == indicator & df$`#instances` == sample_interval, ]
        if(nrow(sub_df) < 30){
          
          next
        }else{
          pvalue <- wilcox.test(sub_df[,6],sub_df[,7],paired = T,conf.level = conf_level)$p.value
          
          wline <- paste(scenario,sample_interval,indicator,colnames(sub_df)[6],colnames(sub_df)[7],'wilcox',pvalue,sep = ',')
          write(wline, file=res_file_path, append=T)
          pvalue_str <- signtest(sub_df[,6]-sub_df[,7], m=0, conf.level=conf_level, exact=FALSE)
          pvalue <- substr(pvalue_str@PVal,nchar("The p-value is   "),nchar(pvalue_str@PVal))
          wline <- paste(scenario,sample_interval,indicator,colnames(sub_df)[6],colnames(sub_df)[7],'signtest',pvalue,sep = ',')
          write(wline, file=res_file_path, append=T)
          
        }
      }
    } 
  }
  
}
  
  
setwd('../experimentResult')
getwd()
res_file_path <- 'RQ3/pvalue.csv'
write(paste('scenario','#instances','indicator','col1','col2','statistic','pvalue',sep = ','), file=res_file_path, append=F)
conf_level = 0.95

output_files <- function(){
  files <- list.files('RQ3')
  files <- files[startsWith(files,'paired_performance')]
  
  for(file in files){
    producePvalue(file_path = file.path('RQ3',file))
  }
}

output_files()

df <- read.csv(res_file_path,check.names = F)
indicators <- unique(df$indicator)
scenarios <- unique(df$scenario)
summary_res_file <- 'RQ3/pvalue_summary.csv'
if(!file.exists(summary_res_file)){
  write(paste('scenario','comparison','indicator','statistic','ratio_rejectN0',sep=','),file=summary_res_file,append = F)
}

for (scenario in scenarios) {
  for(indicator in indicators){
    sub_df <- df[df$scenario== scenario & df$indicator== indicator, ]
    sub_df.noise0.1 <- sub_df[endsWith(sub_df$col2,'noise0.1'),]
    for(statistic in unique(df$statistic)){
      item_df <- sub_df.noise0.1[sub_df.noise0.1$statistic==statistic,'pvalue'] 
      ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
      one <- paste(scenario,'normal vs. noise0.1',indicator,statistic,ratio_rejectN0,sep = ',')
      write(one, file=summary_res_file, append=T)
    }
    sub_df.noise0.05 <- sub_df[endsWith(sub_df$col2,'noise0.05'),]
    for(statistic in unique(df$statistic)){
      item_df <- sub_df.noise0.05[sub_df.noise0.05$statistic==statistic,'pvalue'] 
      ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
      one <- paste(scenario,'normal vs. noise0.05',indicator,statistic,ratio_rejectN0,sep = ',')
      write(one, file=summary_res_file, append=T)
    }
    
    
    sub_df.normal <- sub_df[!endsWith(sub_df$col2,'noise0.05') & !endsWith(sub_df$col2,'noise0.1'),]
    for(statistic in unique(df$statistic)){
      item_df <- sub_df.normal[sub_df.normal$statistic==statistic,'pvalue'] 
      ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
      one <- paste(scenario,'normal vs. normal',indicator,statistic,ratio_rejectN0,sep = ',')
      write(one, file=summary_res_file, append=T)
    }
  }
}

reconstruct <- function(){
  df <- read.csv('RQ3/pvalue_summary.csv')
  new_df <- data.frame(matrix(nrow = 0,ncol = 2+1+2*(length(unique(df$indicator))-1)))
  
  first_flag = T
  for(comparison in unique(df$comparison)){
    for(scenario in unique(df$scenario)){
      mcnemar <- df[df$comparison==comparison & df$scenario == scenario & df$statistic=='mcnemar','ratio_rejectN0']
      wilcox <- df[df$comparison==comparison & df$scenario == scenario & df$statistic=='wilcox','ratio_rejectN0']
      indicator_wilcox <- df[df$comparison==comparison & df$scenario == scenario & df$statistic=='wilcox','indicator']
      indicator_wilcox <- paste(indicator_wilcox,'wilcox',sep = ' ')
      st <- df[df$comparison==comparison & df$scenario == scenario & df$statistic=='signtest','ratio_rejectN0']
      indicator_st <- df[df$comparison==comparison & df$scenario == scenario & df$statistic=='signtest','indicator']
      indicator_st <- paste(indicator_st,'signtest',sep = ' ')
      value <- c(comparison,scenario,mcnemar,wilcox,st)
      coln <- c('comparison','scenario','McNemar',indicator_wilcox,indicator_st)
      if(first_flag){
        first_flag = F
        colnames(new_df) <- coln
        
      }
      new_df[nrow(new_df)+1,] <- value
    }
  }
  write.csv(new_df,file = 'RQ3/folumated_summary_pvalue.csv',row.names = F)

}

