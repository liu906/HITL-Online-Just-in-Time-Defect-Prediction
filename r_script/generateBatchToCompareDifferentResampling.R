# setwd('/media/lxt/TOSHIBA EXT/moa/')
setwd('D:/work/real-world-evaluation/')
source('./r_script/generateExperimentBatch.R')
res_root <- './r_script/result/differentResampling/'
learners = c(
  'trees.HoeffdingTree',
  #'"(meta.imbalanced.OnlineSMOTEBagging" -l trees.HoeffdingTree -s 1 -i "2)"',
  #' #'"(meta.imbalanced.OnlineSMOTEBagging" -l trees.HoeffdingTree -s 10 -i "2)"',
  '"(meta.imbalanced.OnlineUnderOverBagging" -l trees.HoeffdingTree -s 10 -i "2)"',
  '"(meta.imbalanced.OnlineUnderOverBagging" -l trees.HoeffdingTree -s 1 -i "2)"',
  '"(meta.imbalanced.OnlineRUSBoost" -l trees.HoeffdingTree -s 1 -i "2)"',
  '"(meta.imbalanced.OnlineRUSBoost" -l trees.HoeffdingTree -s 10 -i "2)"',
  '"(meta.imbalanced.OnlineUnderOverBagging" -l trees.HoeffdingTree -s 10 -i "1)"',
  '"(meta.imbalanced.OnlineUnderOverBagging" -l trees.HoeffdingTree -s 1 -i "1)"',
  '"(meta.imbalanced.OnlineRUSBoost" -l trees.HoeffdingTree -s 1 -i "1)"',
  '"(meta.imbalanced.OnlineRUSBoost" -l trees.HoeffdingTree -s 10 -i "1)"'
)


files <-
  list.files('./commit_guru_dataset/cut2years/',
             pattern = 'arff$',
             full.names = T)

seed = '1'
f_sampleFrequency = '100'
q_timeFrequency = '100'
fold = '30'

script_file <- 'resample.sh'

for (i in 1:length(files)) {
  project <- files[i]
  for (learner in learners) {
    
    if(T){
      #PosNeg
      PosWinowLengths <- c(7)
      NegWinowLengths <- c(90)
      evaluation_method <- 'EvaluatePrequentialDelayedCVPosNegWindow'
      for(P_day in PosWinowLengths){
        P <- P_day * seconds_in_a_day
        for(N_day in NegWinowLengths){
          N <- N_day * seconds_in_a_day
          command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root,fold=fold)
          sink(script_file,append = T)
          cat(command)
          cat("\n")
          sink()
          
        }
      }
    }
    
    
    if(T){
      #Extension
      evaluation_method <- 'EvaluatePrequentialDelayedCVExtension'
      for(N_day in NegWinowLengths){
        N <- N_day * seconds_in_a_day
        command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,N,N,evaluation_method,res_root,fold=fold)
        sink(script_file,append = T)
        cat(command)
        cat("\n")
        sink()
      }
    }
    
    
    
    if(T){
      #Ideal
      evaluation_method <- 'EvaluatePrequentialDelayedCVIdeal'
      command <- combineCommandSimplePath(learner,seed,f_sampleFrequency,q_timeFrequency,project,-1,-1,evaluation_method,res_root,fold=fold)
      sink(script_file,append = T)
      cat(command)
      cat("\n")
      sink()
    }
  }
}
