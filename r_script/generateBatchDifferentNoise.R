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

seeds <- as.character(1:50)


noises <- c('0','0.05','0.1')

f_sampleFrequency = '1000'
q_timeFrequency = '1000'
# fold = '30'
fold = '10'
# f_sampleFrequency = '1'
# q_timeFrequency = '1'

validation = 'Bootstrap-Validation'
softInvertal <- T
#script_file <- paste('HT-noise.sh',sep='')
script_file <- paste('statisticalTest_softInvertal-10Fold-30.sh',sep='')
interval <- 30 / 0.632
for(noise in noises){
  for(seed in seeds){
   # script_file <- paste('experiment-differentNoise-seed',seed,'.sh',sep='')
    for (i in 1:length(files)) {
      if(softInvertal){
        project <- files[i]
        data <- read.arff(project)
        fre <- ceiling(nrow(data)/interval)
        f_sampleFrequency <- as.character(fre)
        q_timeFrequency <- as.character(fre)
      }
      
      for (learner in learners) {
        
        res_root <- paste('./r_script/result/differentNoise/',learner,'/','seed',seed,'-noise',noise,sep='')
        dir.create(res_root,recursive = T, showWarnings = F)
        
        
        if(F){
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
        if(T){
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
