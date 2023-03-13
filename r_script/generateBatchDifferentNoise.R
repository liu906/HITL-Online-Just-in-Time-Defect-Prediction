setwd('D:/work/real-world-evaluation/')
# setwd('/media/lxt/TOSHIBA EXT/moa/')
source('./r_script/generateExperimentBatch.R')

learners = c(
  'trees.HoeffdingAdaptiveTreeClassifLeaves'
)

files <-
  list.files('./commit_guru_dataset/cut2years/',
             pattern = 'arff$',
             full.names = T)


seeds <- c('1','2','3','4','5','6','7','8','9','10','11')
noises <- c('0','0.05','0.1')

f_sampleFrequency = '1'
q_timeFrequency = '1'

validation = 'Bootstrap-Validation'

script_file <- paste('experiment-allNoise.sh',sep='')
for(noise in noises){
  for(seed in seeds){
   # script_file <- paste('experiment-differentNoise-seed',seed,'.sh',sep='')
    for (i in 1:length(files)) {
      project <- files[i]
      for (learner in learners) {
        
        res_root <- paste('./r_script/result/differentNoise/',learner,'/','seed',seed,'-noise',noise,sep='')
        dir.create(res_root,recursive = T, showWarnings = F)
        
        #PosNeg
        
        PosWinowLengths <- c(7)
        NegWinowLengths <- c(90)
        evaluation_method <- 'EvaluatePrequentialDelayedCVPosNegWindow'
        # script_file <- 'command-EvaluatePrequentialDelayedCVPosNegWindow.sh'
        for(P_day in PosWinowLengths){
          P <- P_day * seconds_in_a_day
          for(N_day in NegWinowLengths){
            N <- N_day * seconds_in_a_day
            command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root,validation,noise)
            sink(script_file,append = T)
            cat(command)
            cat("\n")
            sink()
            
          }
        }
        
        # #Extension
        # evaluation_method <- 'EvaluatePrequentialDelayedCVExtension'
        # # script_file <- 'command-EvaluatePrequentialDelayedCVExtension.sh'
        # for(N_day in NegWinowLengths){
        #   N <- N_day * seconds_in_a_day
        #   command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,N,N,evaluation_method,res_root,validation,noise)
        #   sink(script_file,append = T)
        #   cat(command)
        #   cat("\n")
        #   sink()
        # }
        # 
        # #Ideal
        # evaluation_method <- 'EvaluatePrequentialDelayedCVIdeal'
        # # script_file <- 'command-EvaluatePrequentialDelayedCVIdeal.sh'
        # command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,-1,-1,evaluation_method,res_root,validation,noise)
        # sink(script_file,append = T)
        # cat(command)
        # cat("\n")
        # sink()
        # 
      }
    }
  }
  
}
