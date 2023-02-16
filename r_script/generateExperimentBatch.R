
combineCommand <- function(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,dump_path,detail_path,seed,evaluation_method){
  command <-
    paste(
      'java -classpath "classes" moa.DoTask ',evaluation_method ,' -l ',
      learner ,
      ' -s "(ArffFileStream" -f "',
      project ,
      ')" -e "(FadingFactorClassificationPerformanceEvaluator" -a 0.99 -o -p -r "-f)" -k 99 -f ',
      f_sampleFrequency ,
      ' -q ',
      q_timeFrequency ,
      ' -d "',
      dump_path ,
      '" -o ',
      '"',
      detail_path ,
      '"',
      ' -a Bootstrap-Validation  -D 0 -w 5 -A 1 -r ',seed,' -P ', P,' -N ',N,
      sep = ''
    )
  return(command)
}



setwd('/media/lxt/TOSHIBA EXT/moa/')
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
  'trees.RandomHoeffdingTree',
  '(meta.OzaBag -l trees.HoeffdingTree)',
  '(meta.OzaBag -l trees.HoeffdingAdaptiveTree)',
  '(meta.OzaBagASHT -l ASHoeffdingTree)',
  '(meta.OzaBagAdwin -l trees.HoeffdingTree)',
  '(meta.OzaBagAdwin -l trees.HoeffdingAdaptiveTree)',
  '(meta.OzaBoost -l trees.HoeffdingTree)',
  '(meta.OzaBoost -l trees.HoeffdingAdaptiveTree)',
  '(meta.OzaBoostAdwin -l trees.HoeffdingTree)',
  '(meta.OzaBoostAdwin -l trees.HoeffdingAdaptiveTree)',
  '(meta.imbalanced.CSMOTE -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  '(meta.imbalanced.OnlineAdaBoost -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  '(meta.imbalanced.OnlineRUSBoost -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  '(meta.imbalanced.OnlineSMOTEBagging -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))',
  '(meta.imbalanced.OnlineUnderOverBagging -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))'
)




files <-
  list.files('./commit_guru_dataset',
             pattern = 'arff',
             full.names = T)

seed = '2'
f_sampleFrequency = '1'
q_timeFrequency = '1'
seconds_in_a_day <- 24*60*60


evaluation_method <- 'EvaluatePrequentialDelayedCVPosNegWindow'

for (i in 1:length(projects)) {
  project <- files[i]
  name <- substr(basename(project), 1, nchar(basename(project)) - 5)
  for (learner in learners) {
    learner <- '"(meta.OzaBag" -l "trees.HoeffdingTree)"'
    temp <- gsub('"', '', learner)
    temp <- gsub(' ', '_', temp)
    PosWinowLengths <- c(1,3,7,15,30,60)
    NegWinowLengths <- c(15,90)
    
    for(P_day in PosWinowLengths){
      P = P_day * seconds_in_a_day
      for(N_day in NegWinowLengths){
        N = N_day * seconds_in_a_day
        dump_path <-
          paste(
            './r_script/result/differentLearner/',
            name,
            '_',
            temp,
            '_',evaluation_method,'_',P_day,'_',N_day,'_5Fold_FF0.99_dumpFile.csv',
            sep = ''
          )
        detail_path <-
          paste(
            './r_script/result/differentLearner/',
            name,
            '_',
            temp,
            '_',evaluation_method,'_',P_day,'_',N_day,'_5Fold_FF0.99_detail.csv',
            sep = ''
          )
        
        command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,dump_path,detail_path,seed,evaluation_method)
      }
    }
    system(command, wait = F)
    
  }
}
