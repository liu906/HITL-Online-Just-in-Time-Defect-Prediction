setwd('/media/lxt/TOSHIBA EXT/moa/')

# dataset_root <- './commit_guru_dataset/cut2years/'
# res_root <- './commit_guru_dataset/cut2years/cutByCommitIndex/'
# dir.create(res_root, showWarnings = F, recursive = T)
# 
# 
# files <- list.files(dataset_root,pattern = 'csv')
# gap = 10000 - 1
# 
# file <- files[1]
# for (file in files) {
#   df <- read.csv(file.path(dataset_root,file))
#   nrow(df)
#   idx_start <- 1
#   while(idx_start<nrow(df)){
#     sub_df <- df[idx_start:(idx_start+gap),]
#     nrow(sub_df)
#     write.csv(sub_df,file.path(res_root,paste(idx_start,'-',idx_start+gap,'-',file,sep='')),quote = F,row.names = F)
#     sub_df <- df[idx_start:nrow(df),]
#     write.csv(sub_df,file.path(res_root,paste(idx_start,'-','end','-',file,sep='')),quote = F,row.names = F)
#     idx_start <- idx_start + gap + 1
#     cat(nrow(df),file.path(res_root,paste(idx_start,'-',idx_start+gap,'-',file,sep='')),'\n')
#   }
# }
# 



# # csv to arff done by windows batch file
source('./r_script/generateExperimentBatch.R')

# generate batch script to train models on different time stage of project 
files <-
  list.files('./commit_guru_dataset/cut2years/cutByCommitIndex/',
             pattern = 'arff',
             full.names = T)

seed = '2'
f_sampleFrequency = '1'
q_timeFrequency = '1'


# learners = c(
#   'trees.HoeffdingAdaptiveTreeClassifLeaves',
#   'trees.HoeffdingTreeClassifLeaves'
# )

learners = c(
  'trees.HoeffdingAdaptiveTreeClassifLeaves'
)


# validation = 'Split-Validation'
validations <- c('Split-Validation','Cross-Validation','Bootstrap-Validation')

for(validation in validations){
  for (i in 1:length(files)) {
    project <- files[i]
    for (learner in learners) {
      
      
      res_root <- paste('./r_script/result/cutByCommitIndex/',validation,'/posNeg',sep='')
      script_file <- paste('command-posNeg-',validation,'.sh',sep='')
      dir.create(res_root,recursive = T, showWarnings = F)
      PosWinowLengths <- c(7)
      NegWinowLengths <- c(90)
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
      
      
      res_root <- paste('./r_script/result/cutByCommitIndex/',validation,'/Ideal',sep='')
      dir.create(res_root,recursive = T, showWarnings = F)
      script_file <- paste('command-Ideal-',validation,'.sh',sep='')
      # Ideal
      evaluation_method <- 'EvaluatePrequentialDelayedCVIdeal'
      command <- combineCommand(learner,seed,f_sampleFrequency,q_timeFrequency,project,-1,-1,evaluation_method,res_root,validation)
      sink(script_file,append = T)
      cat(command)
      cat("\n")
      sink()
      
    }
  }
}



