library(nonpar)
library(parallel)
# setwd(dir = '/media/lxt/TOSHIBA EXT/moa/r_script/')
options(scipen=999)
producePvalue <- function(file_path,res_file_path,setting,conf_level){
  df <- read.csv(file = file_path,row.names = 1,check.names = F)
  cols <- c('scenario','dataset','instance','indicator','col1','col2','statistic','pvalue')
  return_df <- data.frame(matrix(nrow = 0,ncol = length(cols)))
  colnames(return_df) <- cols
    
  scenarios = unique(df$scenario)
  folds = unique(df$fold)
  datasets = unique(df$dataset)
  indicators <- unique(df$indicator)
  

  for(scenario in scenarios){
    for(indicator in indicators){
      for(dataset in datasets){
        
        sub_df <- df[df$scenario == scenario & df$indicator == indicator & df$dataset==dataset, ]
        instances <- unique(sub_df$`#instances`)
        if(setting==3){
          for(instance in instances){
            sub_df_ <- sub_df[sub_df$`#instances`==instance,]
            if(nrow(sub_df_)!=length(folds)){
              next
            }
            cat('mode3 #sample ',length(sub_df_[,6]),'\n')
            pvalue <- wilcox.test(sub_df_[,6],sub_df_[,7],paired = T,conf.level = conf_level)$p.value
            return_df[nrow(return_df)+1,] <- c(scenario,dataset,instance,indicator,colnames(sub_df)[6],colnames(sub_df)[7],'wilcox',pvalue)
            pvalue_str <- signtest(sub_df_[,6] - sub_df_[,7], m=0, conf.level=conf_level, exact=FALSE)
            pvalue <- substr(pvalue_str@PVal,nchar("The p-value is   "),nchar(pvalue_str@PVal))
            return_df[nrow(return_df)+1,] <- c(scenario,dataset,instance,indicator,colnames(sub_df)[6],colnames(sub_df)[7],'signtest',pvalue)
            
          }
        }else if(setting==2){
          idx = length(instances)
          while(idx>0){
            sub_df_ <- sub_df[sub_df$`#instances`==instances[idx],]
            if(nrow(sub_df_)==length(folds)){
              break
            }
            idx = idx - 1
          }
          cat('mode2 #sample ',length(sub_df_[,6]),'\n')
          pvalue <- wilcox.test(sub_df_[,6],sub_df_[,7],paired = T,conf.level = conf_level)$p.value
          return_df[nrow(return_df)+1,] <- c(scenario,dataset,instances[idx],indicator,colnames(sub_df)[6],colnames(sub_df)[7],'wilcox',pvalue)
          pvalue_str <- signtest(sub_df_[,6] - sub_df_[,7], m=0, conf.level=conf_level, exact=FALSE)
          pvalue <- substr(pvalue_str@PVal,nchar("The p-value is   "),nchar(pvalue_str@PVal))
          return_df[nrow(return_df)+1,] <- c(scenario,dataset,instances[idx],indicator,colnames(sub_df)[6],colnames(sub_df)[7],'signtest',pvalue)
         
        }else if(setting==1){
          sub_df_ <- sub_df
          instance <- 'all instances by step'
          cat('mode1 #sample ',length(sub_df_[,6]),'\n')
          pvalue <- wilcox.test(sub_df_[,6],sub_df_[,7],paired = T,conf.level = conf_level)$p.value
          return_df[nrow(return_df)+1,] <- c(scenario,dataset,instance,indicator,colnames(sub_df)[6],colnames(sub_df)[7],'wilcox',pvalue)
          pvalue_str <- signtest(sub_df_[,6] - sub_df_[,7], m=0, conf.level=conf_level, exact=FALSE)
          pvalue <- substr(pvalue_str@PVal,nchar("The p-value is   "),nchar(pvalue_str@PVal))
          return_df[nrow(return_df)+1,] <- c(scenario,dataset,instance,indicator,colnames(sub_df)[6],colnames(sub_df)[7],'signtest',pvalue)
          
        }


      }
    }
  }
  return(return_df)

}
  
parallel_run <- function(df){
  
  library(nonpar)
  file_path <- df$file_path
  cat('file_path: ',file_path,'\n')
  
  res_file_path <- df$res_file_path
  setting <- df$setting
  conf_level <- df$conf_level
  return_df <- producePvalue(file_path,res_file_path,setting,conf_level)
  return(return_df)
}





summary_pvalue <- function(res_root,res_file_path,setting){
  
  
  # write(paste('scenario','dataset','instance','indicator','col1','col2','statistic','pvalue',sep = ','), file=res_file_path, append=F)
  conf_level = 0.95
  files <- list.files(res_root,pattern = fold)
  example_list <- list()
  

  for(file in files){
    cat(file,'\n')
    example_list[[length(example_list)+1]] <- list(file_path=file.path(res_root,file), 
                                                   res_file_path = res_file_path,
                                                   setting = setting,
                                                 conf_level = conf_level)}

  cl <- makeCluster(14,outfile='xxx.log')
  clusterExport(cl,c("producePvalue"),envir=environment())
  list_return_df <- parLapply(cl,example_list,parallel_run)
  
  stopCluster(cl)
  ####################################
  for(idx in 1:length(list_return_df)){
    sub_df <- list_return_df[[idx]]
    if(idx==1){
      df = sub_df
    }else{
      df = rbind(df,sub_df)
    }
  }
  
  
  write.csv(df,paste(res_file_path,'_detail.csv',sep = ''),row.names = F)
  indicators <- unique(df$indicator)
  scenarios <- unique(df$scenario)
  
  
  for (scenario in scenarios) {
    for(indicator in indicators){
      sub_df <- df[df$scenario== scenario & df$indicator== indicator, ]
      
      sub_df.noise0.1 <- sub_df[endsWith(sub_df$col2,'noise0.1'),]
      
      for(statistic in unique(df$statistic)){
        item_df <- sub_df.noise0.1[sub_df.noise0.1$statistic==statistic,'pvalue'] 
        ratio_rejectN0 <- sum(as.numeric(item_df) < 1 - conf_level) / length(item_df)
        one <- paste(scenario,'normal vs. noise0.1',indicator,statistic,ratio_rejectN0,sep = ',')
        write(one, file=summary_res_file, append=T)
      }
      sub_df.noise0.05 <- sub_df[endsWith(sub_df$col2,'noise0.05'),]
      for(statistic in unique(df$statistic)){
        item_df <- sub_df.noise0.05[sub_df.noise0.05$statistic==statistic,'pvalue'] 
        ratio_rejectN0 <- sum(as.numeric(item_df) < 1 - conf_level) / length(item_df)
        one <- paste(scenario,'normal vs. noise0.05',indicator,statistic,ratio_rejectN0,sep = ',')
        write(one, file=summary_res_file, append=T)
      }
      
      # sub_df.noise0.2 <- sub_df[endsWith(sub_df$col2,'noise0.2'),]
      # for(statistic in unique(df$statistic)){
      #   item_df <- sub_df.noise0.2[sub_df.noise0.2$statistic==statistic,'pvalue'] 
      #   ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
      #   one <- paste(scenario,'normal vs. noise0.2',indicator,statistic,ratio_rejectN0,sep = ',')
      #   write(one, file=summary_res_file, append=T)
      # }
      # 
      # sub_df.noise0.3 <- sub_df[endsWith(sub_df$col2,'noise0.3'),]
      # for(statistic in unique(df$statistic)){
      #   item_df <- sub_df.noise0.3[sub_df.noise0.3$statistic==statistic,'pvalue'] 
      #   ratio_rejectN0 <- sum(item_df < 1 - conf_level) / length(item_df)
      #   one <- paste(scenario,'normal vs. noise0.3',indicator,statistic,ratio_rejectN0,sep = ',')
      #   write(one, file=summary_res_file, append=T)
      # }
      
      sub_df.normal <- sub_df[!endsWith(sub_df$col2,'noise0.05') & !endsWith(sub_df$col2,'noise0.1'),]
      for(statistic in unique(df$statistic)){
        item_df <- sub_df.normal[sub_df.normal$statistic==statistic,'pvalue'] 
        ratio_rejectN0 <- sum(as.numeric(item_df) < 1 - conf_level) / length(item_df)
        one <- paste(scenario,'normal vs. normal',indicator,statistic,ratio_rejectN0,sep = ',')
        write(one, file=summary_res_file, append=T)
      }
    }
  }
  
}




summary_mcnemar <- function(threshold=3.8414588,root_path='result/differentNoise/LB-ideal-s/pairedResult/'){
  
  files <- list.files(root_path,pattern = fold)
  files <- files[startsWith(files,'mcnemar')]
  first_flag = T
  
  for(file in files){
    sub_df <- read.csv(file.path(root_path,file),check.names = F)
    if(first_flag){
      df <- sub_df
      first_flag <- F
    }else{
      colnames(df)
      colnames(sub_df)
      df <- rbind(df,sub_df)
    }
  }
  
  
  
  statistic <- 'mcnemar'
  indicator <- '' # note McNemar is not related with indicator
  for(scenario in unique(df$scenario)){
    sub_df <- df[df$scenario == scenario,]
    
    
    # sub_df.noise0.3 <- sub_df[endsWith(sub_df$folder2,'noise0.3'),]
    # item_df <- sub_df.noise0.3[,'mcnemar'] 
    # ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
    # one <- paste(scenario,'normal vs. noise0.3',indicator,statistic,ratio_rejectN0,sep = ',')
    # write(one, file=summary_res_file, append=T)
    # 
    # sub_df.noise0.2 <- sub_df[endsWith(sub_df$folder2,'noise0.2'),]
    # item_df <- sub_df.noise0.2[,'mcnemar'] 
    # ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
    # one <- paste(scenario,'normal vs. noise0.2',indicator,statistic,ratio_rejectN0,sep = ',')
    # write(one, file=summary_res_file, append=T)
    
    sub_df.noise0.1 <- sub_df[endsWith(sub_df$folder2,'noise0.1'),]
    item_df <- sub_df.noise0.1[,'mcnemar'] 
    ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
    one <- paste(scenario,'normal vs. noise0.1',indicator,statistic,ratio_rejectN0,sep = ',')
    write(one, file=summary_res_file, append=T)
    
    sub_df.noise0.05 <- sub_df[endsWith(sub_df$folder2,'noise0.05'),]
    
    item_df <- sub_df.noise0.05[,'mcnemar'] 
    ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
    one <- paste(scenario,'normal vs. noise0.05',indicator,statistic,ratio_rejectN0,sep = ',')
    write(one, file=summary_res_file, append=T)
    sub_df.normal <- sub_df[!endsWith(sub_df$folder2,'noise0.05') & !endsWith(sub_df$folder2,'noise0.1'),]
    
    item_df <- sub_df.normal[,'mcnemar'] 
    ratio_rejectN0 <- sum(item_df > threshold) / length(item_df)
    one <- paste(scenario,'normal vs. normal',indicator,statistic,ratio_rejectN0,sep = ',')
    write(one, file=summary_res_file, append=T)
  }
  
}

reconstruct <- function(summary_res_file,final_path){
  df <- read.csv(summary_res_file)
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
  write.csv(new_df,file = final_path,row.names = F)

}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

fold <- '30Fold'

mcnemar_root_path <- 'result/differentNoise/LB-ideal-s/pairedResult/'
pvalue_root_path <- 'result/differentNoise/LB-ideal/pairedResult/'

for (setting in 1:3){
  summary_res_file <- file.path('RQ3',paste(fold,'-pvalue_summary-setting',setting,'.csv',sep = ''))
  write(paste('scenario','comparison','indicator','statistic','ratio_rejectN0',sep=','),
        file=summary_res_file,
        append = F)
  summary_mcnemar(root_path=mcnemar_root_path)
  
  summary_pvalue(res_root=pvalue_root_path,
                 res_file_path= file.path('RQ3',paste(fold,'-pvalue-setting',setting,'.csv',sep='')),
                 setting = setting)
  reconstruct(summary_res_file,
              final_path=file.path('RQ3',paste(fold,'-formulated-pvalue_summary-setting',setting,'.csv',sep = '')))
  
}


# files <- list.files(res_root)
# for(file in files){
#   df <- read.csv(file.path(res_root,file),row.names = 1,check.names = F)
#   temp <- wilcox.test(df[,6],df[,7],paired = F,conf.level = conf_level)$p.value
#   cat(temp,'\n')
# }


