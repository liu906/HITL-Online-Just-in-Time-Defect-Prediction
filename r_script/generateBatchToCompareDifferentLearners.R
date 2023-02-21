setwd('/media/lxt/TOSHIBA EXT/moa/')
source('./r_script/generateExperimentBatch.R')
res_root <- './r_script/result/differentLearner/'
learners = c(
  'trees.HoeffdingTree',
  'trees.HoeffdingAdaptiveTree',
  'trees.ARFHoeffdingTree',
  'trees.ASHoeffdingTree',
  'trees.AdaHoeffdingOptionTree',
  'trees.EFDT',
  'trees.HoeffdingAdaptiveTreeClassifLeaves',
  'trees.HoeffdingOptionTree',
  'trees.HoeffdingTreeClassifLeaves',
  'trees.RandomHoeffdingTree'
  # '(meta.OzaBag -l trees.HoeffdingTree)',
  # '(meta.OzaBag -l trees.HoeffdingAdaptiveTree)',
  # '(meta.OzaBagASHT -l ASHoeffdingTree)',
  # '(meta.OzaBagAdwin -l trees.HoeffdingTree)',
  # '(meta.OzaBagAdwin -l trees.HoeffdingAdaptiveTree)',
  # '(meta.OzaBoost -l trees.HoeffdingTree)',
  # '(meta.OzaBoost -l trees.HoeffdingAdaptiveTree)',
  # '(meta.OzaBoostAdwin -l trees.HoeffdingTree)',
  # '(meta.OzaBoostAdwin -l trees.HoeffdingAdaptiveTree)',
  # '(meta.imbalanced.CSMOTE -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  # '(meta.imbalanced.OnlineAdaBoost -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  # '(meta.imbalanced.OnlineRUSBoost -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  # '(meta.imbalanced.OnlineSMOTEBagging -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  # '(meta.imbalanced.OnlineUnderOverBagging -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))'
)




files <-
  list.files('./commit_guru_dataset/cut2years/',
             pattern = 'arff',
             full.names = T)

seed = '2'
f_sampleFrequency = '1'
q_timeFrequency = '1'

script_file <- 'command.sh'

for (i in 1:length(files)) {
  project <- files[i]
  for (learner in learners) {
    #PosNeg
    #PosWinowLengths <- c(1,3,7,15,30,60)
    PosWinowLengths <- c(7)
    NegWinowLengths <- c(15,90)
    evaluation_method <- 'EvaluatePrequentialDelayedCVPosNegWindow'
    for(P_day in PosWinowLengths){
      P <- P_day * seconds_in_a_day
      for(N_day in NegWinowLengths){
        N <- N_day * seconds_in_a_day
        command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root)
        sink(script_file,append = T)
        cat(command)
        cat("\n")
        sink()
        
      }
    }
    
    #Extension
    evaluation_method <- 'EvaluatePrequentialDelayedCVExtension'
    for(N_day in NegWinowLengths){
      N <- N_day * seconds_in_a_day
      command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,N,N,evaluation_method,res_root)
      sink(script_file,append = T)
      cat(command)
      cat("\n")
      sink()
    }
    #Ideal
    evaluation_method <- 'EvaluatePrequentialDelayedCVIdeal'
    command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,-1,-1,evaluation_method,res_root)
    sink(script_file,append = T)
    cat(command)
    cat("\n")
    sink()
    # system(command, wait = F)
  }
}