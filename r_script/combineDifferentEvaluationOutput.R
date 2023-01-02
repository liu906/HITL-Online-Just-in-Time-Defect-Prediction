setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


library(tibble)

combineOutput <- function(root,res_root,projects,evaluators,posfix,fold,evaluationSettings,flagDeleteInstancesNum){
  for(project in projects){
    cat(project,'\n')
    for(evaluator in evaluators){
      first_flag = TRUE
      for(evaluationSetting in evaluationSettings){
        file_name = paste(project,evaluationSetting,fold,evaluator,posfix,sep = "_")
        df = read.csv(file = file.path(root,file_name),check.names = FALSE)
        if(flagDeleteInstancesNum){
          df$`learning evaluation instances on certain fold` = NULL
        }
        
        df = add_column(df,scenario = evaluationSetting,.after = 1)
        
        
        if(first_flag){
          first_flag = FALSE
          total_df = df
        }else{
          total_df = rbind(total_df,df)
        }
        
      }
      total_df$`learning evaluation instances`
      
      total_df <- total_df[order(total_df[,1]),]
      res_file_name = paste(project,fold,evaluator,posfix,sep = "_")
      write.csv(total_df,row.names = FALSE,file = file.path(res_root,res_file_name))
    }
  }
  
}



root = "../experimentResult/RQ1/"
res_root = "../experimentResult/RQ1/combinationOfDifferentEvaluationSetting/"
projects <- c(
              'FFmpeg',
              # 'brackets',
              # 'camel',
              # 'edx-platform',
              # 'elasticsearch',
              'git',
              'kubernetes',
              # 'mindspore',
              # 'tensorflow',
              # 'vlc'
              )
evaluators <- c("BasicClfPerEva")
posfix <- "detail.csv"
fold <- "5Fold"
evaluationSettings <- c("DelayedCVPosNegWindow(1-90)",
                        "DelayedCVPosNegWindow(3-90)",
                        "DelayedCVPosNegWindow(7-90)",
                        "DelayedCVPosNegWindow(15-90)",
                        "DelayedCVPosNegWindow(30-90)",
                        "DelayedCVPosNegWindow(60-90)",
                        "DelayedCVIdeal",
                        "DelayedCVExtension")
flagDeleteInstancesNum = TRUE
combineOutput(root,res_root,projects,evaluators,posfix,fold,evaluationSettings,flagDeleteInstancesNum)

posfix <- "dumpFile.csv"
flagDeleteInstancesNum = FALSE
combineOutput(root,res_root,projects,evaluators,posfix,fold,evaluationSettings,flagDeleteInstancesNum)
