
res_root <- "D:/work/real-world-evaluation/r_script/result/dR/"    
files <- list.files(res_root,pattern = 'csv$')

projects <- c()
for(file in files){
  project <- strsplit(file,'_')[[1]][2]
  projects <- append(projects,project)
}
projects <- unique(projects)

postfixs <- c('-s_1_-i_2')
#scenarios <- c("PosNegWindow_7_90.csv","Extension_90_90.csv","Ideal.csv")
scenarios <- c("PosNegWindow_7_90")
indicators = c('Kappa Recall Temporal Statistic 1 (percent)',
               'Kappa Gmean Temporal Statistic  (percent)',
               'Kappa FPR Temporal Statistic 1 (percent)')

percentages <- c(0.1,0.5,1)

# resamples <- c('OnlineRUSBoost')
postfixs <- c('-s_1_-i_2','-s_1_-i_1')
resamples <- c('OnlineUnderOverBagging','OnlineRUSBoost')

COMPRAE_TEMPORAL <- F

for (scenario in scenarios) {
  first_flag <- T
  for(project in projects){
    file.noresamples <- list.files(res_root,pattern=paste(temp,'.*',scenario,'.*','detail.csv$',sep = ''))
    file.noresample <-file.noresamples[!grepl('imbalance',file.noresamples)]
    df_noresample <- read.csv(file.path(res_root,file.noresample),check.names = F)
    instances <- unique(df_noresample$`learning evaluation instances on certain fold`)
    key_points_noresample <- instances[ceiling((length(instances)-10)*percentages)]
    df_noresample.sub <- df_noresample[df_noresample$`learning evaluation instances on certain fold` %in%  key_points_noresample,]

    for(resample in resamples){
      for (postfix in postfixs) {
        temp <- glob2rx(project)
        temp <- substring(temp, 2, nchar(temp)-1)
        file <- list.files(res_root,pattern=paste(temp,'.*',resample,'.*',postfix,'.*',scenario,'.*','detail.csv$',sep = ''))

        df <- read.csv(file.path(res_root,file),check.names = F)
        instances <- unique(df$`learning evaluation instances on certain fold`)
        key_points <- instances[ceiling((length(instances)-10)*percentages)]
        df.sub <- df[df$`learning evaluation instances on certain fold` %in%  key_points,]
        
        
        for(i in 1:length(percentages)){
          df1 <- df_noresample.sub[df_noresample.sub$`learning evaluation instances on certain fold`==key_points_noresample[i],indicators]
          df2 <- df.sub[df.sub$`learning evaluation instances on certain fold`==key_points[i],indicators]
          cat(nrow(df1),nrow(df2),'\n')
          df1 <- df1[1:min(nrow(df1),nrow(df2)),]
          df2 <- df2[1:min(nrow(df1),nrow(df2)),]
          
          counter <- 1
          if(COMPRAE_TEMPORAL){
            df2[] <- 0
          }
          p_values <- apply(df1, 2, function(col1) {
            col2 <- df2[, colnames(df2)[counter]]
            pvalue <- wilcox.test(col1, col2,paired=T)$p.value
            counter <<- counter + 1
            return(pvalue)
          })
          counter <- 1
          effect_sizes <- apply(df1, 2, function(col1) {
            col2 <- df2[, colnames(df2)[counter]]
            
            eff <- cliff.delta(col1, col2,paired=T)$magnitude
            counter <<- counter + 1
            return(eff)
          })
          counter <- 1
          
          
          effect_sizes.estimate <- apply(df1, 2, function(col1) {
            col2 <- df2[, colnames(df2)[counter]]
            
            eff <- cliff.delta(col1, col2,paired=T)$estimate
            counter <<- counter + 1
            return(eff)
          })
          
          
          temp_df <- cbind(`%ts`=percentages[i],t(as.data.frame(p_values)))
          temp_df <- cbind(project=project,temp_df)
          temp_df <- cbind(resample=resample,temp_df)
          temp_df <- cbind(postfix=postfix,temp_df)
          
          temp_df2 <- cbind(`%ts`=percentages[i],t(as.data.frame(effect_sizes)))
          temp_df2 <- cbind(project=project,temp_df2)
          temp_df2 <- cbind(resample=resample,temp_df2)
          temp_df2 <- cbind(postfix=postfix,temp_df2)
          
          temp_df3 <- cbind(`%ts`=percentages[i],t(as.data.frame(effect_sizes.estimate)))
          temp_df3 <- cbind(project=project,temp_df3)
          temp_df3 <- cbind(resample=resample,temp_df3)
          temp_df3 <- cbind(postfix=postfix,temp_df3)
          
          if(first_flag){
            first_flag <- F
            total_res <- temp_df
            total_eff <- temp_df2
            total_eff.estimate <- temp_df3
          }else{
            total_res <- rbind(total_res,temp_df)
            total_eff <- rbind(total_eff,temp_df2)
            total_eff.estimate <- rbind(total_eff.estimate,temp_df3)
          }
        }
        
      }
    }

  }
  data_path <- 'D:/work/real-world-evaluation/r_script/differentResample'
  dir.create(data_path,showWarnings = F)
  if(COMPRAE_TEMPORAL){
    # write.csv(total_res,file.path(data_path,paste(scenario,'pvalue_temporal_HoeffdingTree.csv',sep='')),row.names = F)
    # write.csv(total_eff,file.path(data_path,paste(scenario,'cliffd_temporal_HoeffdingTree.csv',sep='')),row.names = F)
    write.csv(total_eff.estimate,file.path(data_path,paste(scenario,'cliffdEstimate_temporal_HoeffdingTree.csv',sep='')),row.names = F)
  }else{
    # write.csv(total_res,file.path(data_path,paste(scenario,'pvalue_hoeffdingTree_HoeffdingTree.csv',sep='')),row.names = F)
    # write.csv(total_eff,file.path(data_path,paste(scenario,'cliffd_hoeffdingTree_HoeffdingTree.csv',sep='')),row.names = F)
    write.csv(total_eff.estimate,file.path(data_path,paste(scenario,'cliffdEstimate_hoeffdingTree_HoeffdingTree.csv',sep='')),row.names = F)
  }
  
}
