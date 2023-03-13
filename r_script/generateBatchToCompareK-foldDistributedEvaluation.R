# setwd('/media/lxt/TOSHIBA EXT/moa/')


setwd('D:/work/real-world-evaluation/')
source('./r_script/generateExperimentBatch.R')

getwd()
learners = c(
  'trees.HoeffdingAdaptiveTreeClassifLeaves'
)


files <-
  list.files('./commit_guru_dataset/cut2years/',
             pattern = 'arff',
             full.names = T)

list.files('.',pattern = '*.sh')
# seeds <- c('1','2','3','4','5','6','7','8','9','10')
seeds <- c('1')
noises <- c('0')

f_sampleFrequency = '1'
q_timeFrequency = '1'


validations = c('Cross-Validation','Split-Validation','Bootstrap-Validation')
script_file <- paste('experiment-different-Distributed-Split.sh',sep='')
for(noise in noises){
  for(seed in seeds){
    
    for (i in 1:length(files)) {
      project <- files[i]
      for (learner in learners) {
        for (validation in validations) {
          res_root <- paste('./r_script/result/differentDistributedSplit/',learner,'/','seed',seed,'-noise',noise,sep='')
          dir.create(res_root,recursive = T, showWarnings = F)
          
          
          #Ideal
          evaluation_method <- 'EvaluatePrequentialDelayedCVIdeal'
          # script_file <- 'command-EvaluatePrequentialDelayedCVIdeal.sh'
          command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,-1,-1,evaluation_method,res_root,validation,noise)
          sink(script_file,append = T)
          cat(command)
          cat("\n")
          sink()
        }
       
        
      }
    }
  }
  
}
