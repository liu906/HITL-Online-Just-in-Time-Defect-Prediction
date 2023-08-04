# setwd('/media/lxt/TOSHIBA EXT/moa/')
setwd('D:/work/real-world-evaluation/')
source('./r_script/generateExperimentBatch.R')
getwd()
learners = c(
  'trees.HoeffdingTree'
  # 'meta.OOB'
  
)


files <-
  list.files('./commit_guru_dataset/cut2years/',
             pattern = 'arff$',
             full.names = T)


# seeds <- c('1','2','3','4','5','6','7','8','9','10')
seeds <- c('1')
noises <- c('0')

f_sampleFrequency = '30'
q_timeFrequency = '30'

fold <- '10'
validation = 'Bootstrap-Validation'
script_file <- paste('RQ3-hitl.sh',sep='')
for(noise in noises){
  for(seed in seeds){
    
    for (i in 1:length(files)) {
      project <- files[i]
      for (learner in learners) {
        
        res_root <- paste('./r_script/result/differentEvaluationProcudure/',learner,'/','seed',seed,'-noise',noise,sep='')
        dir.create(res_root,recursive = T, showWarnings = F)
        
        if(F){
          #PosNeg
          #PosWinowLengths <- c(1,3,7,15,30,60)
          PosWindowLengths <- c(15,90)
          # NegWinowLengths <- c(15,90)
          NegWindowLengths <- c(1,3,7,15,30,60,90)
          evaluation_method <- 'EvaluatePrequentialDelayedCVPosNegWindow'
          # script_file <- 'command-EvaluatePrequentialDelayedCVPosNegWindow.sh'
          for(P_day in PosWindowLengths){
            P <- P_day * seconds_in_a_day
            for(N_day in NegWindowLengths){
              N <- N_day * seconds_in_a_day
              command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root,validation,noise,fold)
              sink(script_file,append = T)
              cat(command)
              cat("\n")
              sink()
              
            }
          }
          
        }
        if(T){

          NegWindowLengths <- c(1,3,7,15,30,60,90)
          evaluation_method <- 'EvaluatePrequentialDelayedCVPosNegWindow'
          # script_file <- 'command-EvaluatePrequentialDelayedCVPosNegWindow.sh'
          
            for(N_day in NegWindowLengths){
              N <- N_day * seconds_in_a_day
              P <- N #!!!!
              command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root,validation,noise,fold)
              sink(script_file,append = T)
              cat(command)
              cat("\n")
              sink()
            }
          }
          
        
        if(F){
          #Extension
          evaluation_method <- 'EvaluatePrequentialDelayedCVExtension'
          # script_file <- 'command-EvaluatePrequentialDelayedCVExtension.sh'
          NegWindowLengths <- c(1,3,7,15,30,60,90)
          for(N_day in NegWindowLengths){
            N <- N_day * seconds_in_a_day
            command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,N,N,evaluation_method,res_root,validation,noise,fold)
            sink(script_file,append = T)
            cat(command)
            cat("\n")
            sink()
          }
        }
        if(F){
          #Ideal
          evaluation_method <- 'EvaluatePrequentialDelayedCVIdeal'
          # script_file <- 'command-EvaluatePrequentialDelayedCVIdeal.sh'
          command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,-1,-1,evaluation_method,res_root,validation,noise,fold)
          sink(script_file,append = T)
          cat(command)
          cat("\n")
          sink()
        }
      }
    }
  }
  
}
