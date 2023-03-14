
McNemar <- function(df1,df2,step=1){
  # df1 <- value1
  # df2 <- value2
  
  list_m <- c()
  
  
  first_flag = TRUE
  n10 = 0
  n01 = 0
  
  min_len = min(nrow(df1),nrow(df2))
  for(i in 1:min_len){
    
    tp1 = df1[i,'TP']
    fp1 = df1[i,'FP']
    tn1 = df1[i,'TN']
    fn1 = df1[i,'FN']
    
    tp2 = df2[i,'TP']
    fp2 = df2[i,'FP']
    tn2 = df2[i,'TN']
    fn2 = df2[i,'FN']
    if(i==1){
      if( (tp1>0 || tn1>0) && (fp2>0 || fn2>0) ){
        n10 = n10 + 1
      }else if( (tp2>0 || tn2>0) && (fp1>0 || fn1>0) ){
        n01 = n01 + 1
      }
      
    }else{
      pre_tp1 = df1[i-1,'TP']
      pre_fp1 = df1[i-1,'FP']
      pre_tn1 = df1[i-1,'TN']
      pre_fn1 = df1[i-1,'FN']
      pre_tp2 = df2[i-1,'TP']
      pre_fp2 = df2[i-1,'FP']
      pre_tn2 = df2[i-1,'TN']
      pre_fn2 = df2[i-1,'FN']
      
      
      if( (tp1>pre_tp1 || tn1>pre_tn1) && (fp2>pre_fp2 || fn2>pre_fn2) ){
        n10 = n10 + 1
      }else if( (tp2>pre_tp2 || tn2>pre_tn2) && (fp1>pre_fp1 || fn1>pre_fn1) ){
        n01 = n01 + 1
      }
      
    }
    m = (n10-n01)^2/(n10+n01)
    if(is.na(m)){
      # cat(i,tp1,fp1,tn1,fn1,tp2,fp2,tn2,fn2,m,'\n')
      m = 0
    }
    
    if(i %% step==0){
      list_m[length(list_m)+1] <- m
    }
  }
  
  return(list_m)
}

