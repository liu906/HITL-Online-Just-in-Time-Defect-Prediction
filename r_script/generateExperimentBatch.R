
seconds_in_a_day <- 24*60*60
# setwd('/media/lxt/TOSHIBA EXT/moa/')
setwd('D:/work/real-world-evaluation/')

combineCommand <- function(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root,validation='Bootstrap-Validation',noise=0){
  
  name <- substr(basename(project), 1, nchar(basename(project)) - 5)
  temp <- gsub('"', '', learner)
  temp <- gsub(' ', '_', temp)
  
  root <- file.path(res_root, temp,paste('seed',seed,sep = ''))
  dir.create(root, showWarnings = F, recursive = T)
  if(P<0 || N<0){
    dump_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_seed',seed,'_',validation,'_5Fold_FF0.99_dumpFile.csv',
        sep = ''
      )
    dump_path <- file.path(root,dump_path)
    
    detail_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_seed',seed,'_',validation,'_5Fold_FF0.99_detail.csv',
        sep = ''
      )
    detail_path <- file.path(root,detail_path)
  }else{
    dump_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_',as.character(P/seconds_in_a_day),'_',as.character(N/seconds_in_a_day),'_seed',seed,'_',validation,'_5Fold_FF0.99_dumpFile.csv',
        sep = ''
      )
    dump_path <- file.path(root,dump_path)
    
    detail_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_',as.character(P/seconds_in_a_day),'_',as.character(N/seconds_in_a_day),'_seed',seed,'_',validation,'_5Fold_FF0.99_detail.csv',
        sep = ''
      )
    detail_path <- file.path(root,detail_path)
  }

  
  if(P<0 || N<0){
    command <-
      paste(
        'java -classpath "classes" moa.DoTask ',evaluation_method ,' -l ',
        learner ,
        ' -s "(ArffFileStream" -f "',
        project ,
        ')" -e "(FadingFactorClassificationPerformanceEvaluator" -a 0.99 ', ' -n ', noise,' -o -p -r "-f)" -k 99 -f ',
        f_sampleFrequency ,
        ' -q ',
        q_timeFrequency ,
        ' -d "',
        dump_path ,
        '" -o ',
        '"',
        detail_path ,
        '"',
        ' -a ', validation ,' -D 0 -w 5 -A 1 -r ',seed,
        sep = ''
      )
  }else{
    command <-
      paste(
        'java -classpath "classes" moa.DoTask ',evaluation_method ,' -l ',
        learner ,
        ' -s "(ArffFileStream" -f "',
        project ,
        ')" -e "(FadingFactorClassificationPerformanceEvaluator" -a 0.99 ',' -n ', noise, ' -o -p -r "-f)" -k 99 -f ',
        f_sampleFrequency ,
        ' -q ',
        q_timeFrequency ,
        ' -d "',
        dump_path ,
        '" -o ',
        '"',
        detail_path ,
        '"',
        ' -a ', validation, ' -D 0 -w 5 -A 1 -r ',seed,' -P ', P,' -N ',N,
        sep = ''
      )
  }

  return(command)
}


combineCommandSimplePath <- function(learner,seed,f_sampleFrequency,q_timeFrequency,project,P,N,evaluation_method,res_root,validation='Bootstrap-Validation',noise=0){
  
  name <- substr(basename(project), 1, nchar(basename(project)) - 5)
  temp <- gsub('"', '', learner)
  temp <- gsub(' ', '_', temp)
  
  root <- res_root
  dir.create(root, showWarnings = F, recursive = T)
  if(P<0 || N<0){
    dump_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_seed',seed,'_',validation,'_5Fold_FF0.99_dumpFile.csv',
        sep = ''
      )
    dump_path <- file.path(root,dump_path)
    
    detail_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_seed',seed,'_',validation,'_5Fold_FF0.99_detail.csv',
        sep = ''
      )
    detail_path <- file.path(root,detail_path)
  }else{
    dump_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_',as.character(P/seconds_in_a_day),'_',as.character(N/seconds_in_a_day),'_seed',seed,'_',validation,'_5Fold_FF0.99_dumpFile.csv',
        sep = ''
      )
    dump_path <- file.path(root,dump_path)
    
    detail_path <-
      paste(
        name,
        '_',
        temp,
        '_',evaluation_method,'_',as.character(P/seconds_in_a_day),'_',as.character(N/seconds_in_a_day),'_seed',seed,'_',validation,'_5Fold_FF0.99_detail.csv',
        sep = ''
      )
    detail_path <- file.path(root,detail_path)
  }
  
  
  if(P<0 || N<0){
    command <-
      paste(
        'java -classpath "classes" moa.DoTask ',evaluation_method ,' -l ',
        learner ,
        ' -s "(ArffFileStream" -f "',
        project ,
        ')" -e "(FadingFactorClassificationPerformanceEvaluator" -a 0.99 ', ' -n ', noise,' -o -p -r "-f)" -k 99 -f ',
        f_sampleFrequency ,
        ' -q ',
        q_timeFrequency ,
        ' -d "',
        dump_path ,
        '" -o ',
        '"',
        detail_path ,
        '"',
        ' -a ', validation ,' -D 0 -w 5 -A 1 -r ',seed,
        sep = ''
      )
  }else{
    command <-
      paste(
        'java -classpath "classes" moa.DoTask ',evaluation_method ,' -l ',
        learner ,
        ' -s "(ArffFileStream" -f "',
        project ,
        ')" -e "(FadingFactorClassificationPerformanceEvaluator" -a 0.99 ',' -n ', noise, ' -o -p -r "-f)" -k 99 -f ',
        f_sampleFrequency ,
        ' -q ',
        q_timeFrequency ,
        ' -d "',
        dump_path ,
        '" -o ',
        '"',
        detail_path ,
        '"',
        ' -a ', validation, ' -D 0 -w 5 -A 1 -r ',seed,' -P ', P,' -N ',N,
        sep = ''
      )
  }
  
  return(command)
}



