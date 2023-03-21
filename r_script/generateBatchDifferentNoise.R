setwd('D:/work/real-world-evaluation/')
# setwd('/media/lxt/TOSHIBA EXT/moa/')
source('./r_script/generateExperimentBatch.R')

learners = c(
  'meta.LeveragingBag'
)

files <-
  list.files('./commit_guru_dataset/cut2years/',
             pattern = 'arff$',
             full.names = T)


# seeds <- c('1','2','3','4','5','6','7','8','9','10','11')
seeds <- as.character(1:50)


noises <- c('0.2','0.3')

f_sampleFrequency = '1000'
q_timeFrequency = '1000'
fold = '30'
# f_sampleFrequency = '1'
# q_timeFrequency = '1'

validation = 'Bootstrap-Validation'

script_file <- paste('ideal-big-noise.sh',sep='')
for(noise in noises){
  for(seed in seeds){
   # script_file <- paste('experiment-differentNoise-seed',seed,'.sh',sep='')
    for (i in 1:length(files)) {
      project <- files[i]
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
