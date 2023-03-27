
compute_validity_metric <- function(ideal_evaluation, estimated_evaluation){
  return(100 - abs(ideal_evaluation-estimated_evaluation))
}

scenario_process <- function(filename){
  
}
percentages <- seq(0.25,1,0.25)
indicators = c('[avg] Recall for class 1 (percent)',
               '[avg] Kappa Recall Temporal Statistic 1 (percent)',
               '[avg] Gmean for recall  (percent)',
               '[avg] Kappa Gmean Temporal Statistic  (percent)',
               '[avg] FPR for class 1 (percent)',
               '[avg] Kappa FPR Temporal Statistic 1 (percent)')

res_root <- "D:/work/real-world-evaluation/r_script/result/differentEvaluationProcudure/trees.HoeffdingTree/seed1-noise0/dumpFile"    
files <- list.files(res_root,pattern = 'csv$')
projects <- c()
for(file in files){
  project <- strsplit(file,'_')[[1]][1]
  projects <- append(projects,project)
}
projects <- unique(projects)
first_flag <- T
for(project in projects){
  temp <- glob2rx(project)
  temp <- substring(temp, 1, nchar(temp)-1)
  project.files <- list.files(res_root,pattern = temp)
  project.ideal.file <- project.files[grep('Ideal',project.files)]
  project.estimate.files <- project.files[!grepl('Ideal',project.files)]
  for(project.estimate.file in project.estimate.files){
    df.ideal <- read.csv(file.path(res_root,project.ideal.file),check.names = F)
    df.estimate <- read.csv(file.path(res_root,project.estimate.file),check.names = F)
    df.estimate.idxs <- as.integer(nrow(df.estimate)*percentage) 
    df.ideal.idxs <- as.integer(nrow(df.ideal)*percentage)  
    for(indicator in indicators){
      validity <- compute_validity_metric(df.estimate[df.estimate.idxs,indicator],df.ideal[df.ideal.idxs,indicator])
      temp <- c(project.ideal.file,project.estimate.file,indicator,validity)
      if(first_flag){
        total_result <- data.frame(matrix(nrow=0,ncol = length(temp)))
        colnames(total_result) <- c('ideal','estimate','indicator',percentages)
        total_result[nrow(total_result)+1,] <- temp
        first_flag <- F
      }else{
        total_result[nrow(total_result)+1,] <- temp
      }
    }
  }
}


total_result
