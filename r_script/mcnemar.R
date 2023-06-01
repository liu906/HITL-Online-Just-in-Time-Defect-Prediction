
McNemar <- function(df1,df2,interval,softInterval){
  # df1 <- value1
  # df2 <- value2
  
  temp <- data.frame(fold=0,TP=0,FP=0,TN=0,FN=0)
  
  pre_df1 <- rbind(temp,df1)
  pre_df2 <- rbind(temp,df2)
  pre_df1 <- pre_df1[1:(nrow(pre_df1)-1),]
  pre_df2 <- pre_df2[1:(nrow(pre_df2)-1),]
  
  diff_1 <- df1 - pre_df1
  diff_2 <- df2 - pre_df2
  min_len = min(nrow(diff_1),nrow(diff_2))
  
  total_df <- cbind(diff_1[1:min_len,],diff_2[1:min_len,])
  colnames(total_df) <- c('fold1','tp1','fp1','tn1','fn1','fold2','tp2','fp2','tn2','fn2')
  
  # n10 <- numeric(length = min_len)
  # n01 <- numeric(length = min_len)
  
  item_10 <- (total_df$tp1 > 0 | total_df$tn1 > 0) & (total_df$fp2 > 0 | total_df$fn2 > 0)
  item_01 <- (total_df$tp2 > 0 | total_df$tn2 > 0) & (total_df$fp1 > 0 | total_df$fn1 > 0)
  
  n10 <- cumsum(item_10)
  n01 <- cumsum(item_01)
  
  
  if(softInterval){
    n10 <- n10[seq(interval,length(n10),floor(length(n10)/interval))]
    n01 <- n01[seq(interval,length(n01),floor(length(n01)/interval))]
  }else{
    n10 <- n10[seq(interval,length(n10),interval)]
    n01 <- n01[seq(interval,length(n01),interval)]
  }
  
  # cat(seq(1,length(n10),floor(length(n10)/interval)),
  #     '\n')
  m = (n10-n01)^2/(n10+n01)
  
  return(m)
}

