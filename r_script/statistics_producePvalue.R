library(nonpar)

# wilcox.test()
# signtest(Data$Likert, m=3, conf.level=0.95, exact=FALSE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



McNemar <- function(df1,df2){
  
  list_m <- c()
  
  # if(nrow(df1)!=nrow(df2)){
  #   cat("two dataframe must have same #rows!")
  #   quit(status = 0)
  # }
  
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
    cat(i,tp1,fp1,tn1,fn1,m,'\n')
    list_m[length(list_m)+1] <- m
    

  }
  return(list_m)
}

df <- read.csv('../experimentResult/temp/vlc_DelayedCVIdeal_5Fold_BasicClfPerEva_detail.csv')
df <- read.csv('../experimentResult/temp/camel_DelayedCVPosNegWindow(60-90)_5Fold_FF0.99_detail.csv')


df1 <- df[df$fold==0,]
df2 <- df[df$fold==1,]

list_m <- McNemar(df1,df2)
list_m
