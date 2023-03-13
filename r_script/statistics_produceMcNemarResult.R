# library(nonpar)

# wilcox.test()
# signtest(Data$Likert, m=3, conf.level=0.95, exact=FALSE)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()


# setwd(dir = '/media/lxt/TOSHIBA EXT/moa/r_script/')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

McNemar <- function(df1,df2){
  # df1 <- value1
  # df2 <- value2
  
  list_m <- c()
  
  
  first_flag = TRUE
  n10 = 0
  n01 = 0
  for(i in 1:nrow(df1)){
    if(i>nrow(df2)){
      break
    }
    tp1 = df1[i,'TP']
    fp1 = df1[i,'FP']
    tn1 = df1[i,'TN']
    fn1 = df1[i,'FN']
    
    tp2 = df2[i,'TP']
    fp2 = df2[i,'FP']
    tn2 = df2[i,'TN']
    fn2 = df2[i,'FN']
    
    

    if(first_flag){
      first_flag = FALSE
    }else{
      pre_tp1 = df1[i-1,'TP']
      pre_fp1 = df1[i-1,'FP']
      pre_tn1 = df1[i-1,'TN']
      pre_fn1 = df1[i-1,'FN']
      pre_tp2 = df2[i-1,'TP']
      pre_fp2 = df2[i-1,'FP']
      pre_tn2 = df2[i-1,'TN']
      pre_fn2 = df2[i-1,'FN']
      
      if(tp1+fp1+tn1+fn1-(pre_tp1+pre_fp1+pre_tn1+pre_fn1)>1){
        next
      }
      if(tp2+fp2+tn2+fn2-pre_tp2-pre_fp2-pre_tn2-pre_fn2>1){
        next
      }
      
      if( (tp1>pre_tp1 || tn1>pre_tn1) && (fp2>pre_fp2 || fn2>pre_fn2) ){
        n10 = n10 + 1
      }else if( (tp2>pre_tp2 || tn2>pre_tn2) && (fp1>pre_fp1 || fn1>pre_fn1) ){
        n01 = n01 + 1
      }
      
    }
    
    m = abs(n10-n01) * (n10-n01)^2/(n10+n01)
    if(is.na(m)){
      cat(i,tp1,fp1,tn1,fn1,tp2,fp2,tn2,fn2,m,'\n')
    }
   
    list_m[length(list_m)+1] <- m

  }
 
  return(list_m)
}


produceMcNemarResult <- function(folder1,folder2){
  
 
  pat = paste(scenario,fold,eva,postfix,sep='_')
  files1 <- list.files(path = folder1)
  files1 <- files1[endsWith(files1,paste(pat,'.csv',sep=''))]
  files2 <- list.files(path = folder2)
  files2 <- files2[endsWith(files2,paste(pat,'.csv',sep=''))]
  
  res_file = paste('mcnemar','_',folder1,'_',folder2,'_',pat,'.csv',sep = '')
  res_file_path = file.path('RQ3',res_file)
  write(paste('scenario','dataset','fold','#instances','folder1','folder2','mcnemar',sep = ','), file=res_file_path, append=F)
  
  

  for(file in files2){
    df1 = read.csv(file.path(folder1,file),check.names = FALSE)
    df2 = read.csv(file.path(folder2,file),check.names = FALSE)

    for(fold in folds){
       
        value1 = df1[as.numeric(df1$fold)==fold,]
        value2 = df2[as.numeric(df2$fold)==fold,]
        list_m <- McNemar(value1,value2)
        counter = 1
        
        while(counter*interval <= length(list_m)){
          sample_value = list_m[interval*counter]
          write(paste(scenario,file,fold,interval*counter,folder1,folder2,sample_value,sep = ','), file=res_file_path, append=T)
          counter = counter+1
        }
      }
  }
  
  
  
 
  
}

output_files <- function(){
  folder1 = "RQ1-seed2"
  folder2 = "RQ1-seed3"
  produceMcNemarResult(folder1,folder2)
  
  folders <- c("seed1-noise0","seed2-noise0","seed3-noise0","seed4-noise0","seed5-noise0","seed6-noise0","seed7-noise0","seed8-noise0","seed9-noise0","seed10-noise0","seed11-noise0")
  counter <- 1
  
  for(i in 1:length(folders)){
    for(j in (i+1):length(folders)){
      if(counter > maxPair){
        break
      }
      folder1 <- folders[i]
      folder2 <- folders[j]
      cat(folder1,folder2,'\n')
      producePairedResult(folder1,folder2)
      counter <- counter + 1
    }
  }
  
  
}
 
setwd('../experimentResult')
getwd()

# scenarios = c('DelayedCVIdeal','DelayedCVExstension','DelayedCVPosNegWindow(7-90)')
scenarios = c('DelayedCVExtension','DelayedCVPosNegWindow(7-90)')
fold = '5Fold'
eva = 'FF0.99'
postfix = "detail"
folds = 0:4
interval = 1000


for(scenario in scenarios){
  output_files()
}


threshold <- 3.8414588

summary_res_file <- 'RQ3/pvalue_summary.csv'
if(!file.exists(summary_res_file)){
  write(paste('scenario','comparison','indicator','statistic','ratio_rejectN0',sep=','),file=summary_res_file,append = F)
}
files <- list.files('RQ3')
files <- files[startsWith(files,'mcnemar')]
first_flag = T
for(file in files){
  sub_df <- read.csv(file.path('RQ3',file),check.names = F)
  if(first_flag){
    df <- sub_df
    first_flag <- F
  }else{
    colnames(df)
    colnames(sub_df)
    df <- rbind(df,sub_df)
  }
}

# df <- df[!is.na(df$mcnemar),]

statistic <- 'mcnemar'
indicator <- ''

for(scenario in unique(df$scenario)){
  sub_df <- df[df$scenario == scenario,]
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


# library(tibble)
# files <- list.files('RQ3')
# files <- files[startsWith(files,'mcnemar')]
# files <- files[endsWith(files,'DelayedCVIdeal_5Fold_FF0.99_detail.csv')]
# for(file in files){
#   colnames(df)
#   df <- read.csv(file = file.path('RQ3',file),check.names = F)
#   df <- add_column(df, 'scenario' = 'DelayedCVIdeal', .before = 1)
#   write.csv(df,file = file.path('RQ3',file),row.names = F)
# }


