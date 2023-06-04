setwd('D:/work/real-world-evaluation/')
# setwd('/media/lxt/TOSHIBA EXT/moa/')
source('./r_script/generateExperimentBatch.R')
#install.packages("foreign")
library(foreign)
learners = c(
  #'trees.HoeffdingTree'
  'meta.LeveragingBag'
)

files <-
  list.files('./commit_guru_dataset/cut2years/',
             pattern = 'arff$',
             full.names = T)

noises <- c('0','0.05','0.1')
fold = '30'
# fold = '10'
if(T){
  f_sampleFrequency = '1'
  q_timeFrequency = '1'
  seeds <- 6:10
}else{
  seeds <- as.character(1:50)
  f_sampleFrequency = '100'
  q_timeFrequency = '100'
}


validation = 'Bootstrap-Validation'

#script_file <- paste('HT-noise.sh',sep='')
script_file <- paste('simple2.sh',sep='')
softInvertal <- F
interval <- 30 / 0.632
for(noise in noises){
  for(seed in seeds){
   # script_file <- paste('experiment-differentNoise-seed',seed,'.sh',sep='')
    for (i in 1:length(files)) {
      project <- files[i]
      
      if(softInvertal){
        data <- read.arff(project)
        fre <- ceiling(nrow(data)/interval)
        f_sampleFrequency <- as.character(fre)
        q_timeFrequency <- as.character(fre)
      }
      
      
      for (learner in learners) {
        
        res_root <- paste('./r_script/result/differentNoise/',learner,'/','seed',seed,'-noise',noise,sep='')
        dir.create(res_root,recursive = T, showWarnings = F)
        
        
        if(T){
          #PosNeg
          PosWinowLengths <- c(7)
          NegWinowLengths <- c(90)
          evaluation_method <- 'EvaluatePrequentialDelayedCVPosNegWindow'
          # script_file <- 'command-EvaluatePrequentialDelayedCVPosNegWindow.sh'
          for(P_day in PosWinowLengths){
            P <- P_day * seconds_in_a_day
            for(N_day in NegWinowLengths){
              N <- N_day * seconds_in_a_day
              command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root,validation,noise,fold)
              sink(script_file,append = T)
              cat(command)
              cat("\n")
              sink()
              
            }
          }
        }
        if(F){
          #Extension
          evaluation_method <- 'EvaluatePrequentialDelayedCVExtension'
          # script_file <- 'command-EvaluatePrequentialDelayedCVExtension.sh'
          for(N_day in NegWinowLengths){
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
