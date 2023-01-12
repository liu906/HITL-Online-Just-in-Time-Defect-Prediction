

seed = 2
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
  '(meta.imbalanced.OnlineUnderOverBagging -l (meta.AdaptiveRandomForest -x (ADWINChangeDetector -a 0.001) -p (ADWINChangeDetector -a 0.01)))')

f_sampleFrequency = '300'
q_timeFrequency = '300'


files <- list.files('./commit_guru_dataset',pattern = 'arff',full.names = T)
projects <- c('brackets',
              'camel',
              'edx-platform',
              'elasticsearch',
              'FFmpeg',
              'git',
              'kubernetes',
              'mindspore',
              'tensorflow',
              'vlc')
for(i in 1:length(files)){
  
  project <- files[i]
  name <- projects[i]
  for(learner in learners){
    
    dump_path <- paste('./r_script/result/differentLearner/',learner,'_',name,'_DelayedCVIdeal_5Fold_FF0.99_dumpFile.csv',sep='')
    detail_path <- paste('./r_script/result/differentLearner/',learner,'_',name,'_DelayedCVIdeal_5Fold_FF0.99_detail.csv',sep='')
    # command <- paste('java -classpath "classes" moa.DoTask EvaluatePrequentialDelayedCVIdeal -l ',
    #                  learner ,' -s "(ArffFileStream" -f "', project ,')" -e 
    #                "(FadingFactorClassificationPerformanceEvaluator" 
    #                -a 0.99 -o -p -r "-f)" -k 99 -f ', f_sampleFrequency ,' -q ', q_timeFrequency ,' -d "',
    #                  dump_path ,'" -o ','"', detail_path ,'"', ' -a Bootstrap-Validation  -D 0 -w 5 -A 1 -r 2 ',sep='')
    command <- paste('java -classpath "classes" moa.DoTask EvaluatePrequentialDelayedCVIdeal -l ', learner ,' -s "(ArffFileStream" -f "', project ,')" -e "(FadingFactorClassificationPerformanceEvaluator" -a 0.99 -o -p -r "-f)" -k 99 -f ', f_sampleFrequency ,' -q ', q_timeFrequency ,' -d "', dump_path ,'" -o ','"', detail_path ,'"', ' -a Bootstrap-Validation  -D 0 -w 5 -A 1 -r 2 ',sep='')
    system(command,wait = F)
  }
}




