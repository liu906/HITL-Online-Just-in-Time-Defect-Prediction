setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


analyzeTPFP <- function(file,write_file,scenario){
  
  df <- read.csv(file)
  if(scenario==""){
    df.ideal.fold0 = df
  }else{
    df.ideal = df[df$scenario==scenario,]
    df.ideal.fold0 = df.ideal[df.ideal$fold==0,]
  }
  
  
  
  # cTP = c(0)
  # cFN = c(0)
  # cTP2 = c(0)
  # cFP = c(0)
  # cFN = c(0)
  
  # current_recall1 = 0
  # current_precision1 = 0
  df.ideal.fold0$tp = 0
  df.ideal.fold0$fp = 0
  df.ideal.fold0$tn = 0
  df.ideal.fold0$fn = 0
  
    
  for(i in 1:nrow(df.ideal.fold0)){
    
    current_instances = df.ideal.fold0$classified.instances[i]
    current_acc = df.ideal.fold0$classifications.correct..percent.[i]/100
    TPplusTN = current_acc * current_instances
    recall = df.ideal.fold0$Recall.for.class.1..percent.[i]
    precision = df.ideal.fold0$Precision.for.class.1..percent.[i]
    recall0 = df.ideal.fold0$Recall.for.class.0..percent.[i]
    if(recall=="?" || precision=="?" ){
    
      next
    }
    recall = as.numeric(recall)/100
    recall0 = as.numeric(recall0)/100
    precision = as.numeric(precision)/100
    
    b = c(TPplusTN,current_instances,0,0)
    a=matrix(c(1,1,0,0,  1,1,1,1,   1-precision,0,-precision,0, 
               0,1-recall0,-recall0,0),ncol=4,nrow = 4,byrow = T)
    tryCatch({
      res = solve(a,b)
    }, warning = function(w){
      df.ideal.fold0$tp[i] = 0
      df.ideal.fold0$tn[i] = 0
      df.ideal.fold0$fp[i] = 0
      df.ideal.fold0$fn[i] = 0
    }, error = function(e){
      df.ideal.fold0$tp[i] = 0
      df.ideal.fold0$tn[i] = 0
      df.ideal.fold0$fp[i] = 0
      df.ideal.fold0$fn[i] = 0
    },finally = {
      tp = res[1] 
      tn = res[2] 
      fp = res[3] 
      fn = res[4] 
      df.ideal.fold0$tp[i] = tp
      df.ideal.fold0$tn[i] = tn
      df.ideal.fold0$fp[i] = fp
      df.ideal.fold0$fn[i] = fn
    })
    

  }

  write.csv(df.ideal.fold0,write_file)
  
}
file = '../moaGUIresult/comparisonResult.csv'
write_file = '../moaGUIresult/comparisonResult_df.singleWindow(90).fold0.csv'
scenario = "singleWindow(90)"
analyzeTPFP(file,write_file,scenario)


file = '../moaGUIresult/comparisonResult.csv'
write_file = '../moaGUIresult/comparisonResult_df.posNegWindow(7-90).csv'
scenario = "posNegWindow(7-90)"
analyzeTPFP(file,write_file,scenario)

file = '../moaGUIresult/comparisonResult.csv'
write_file = '../moaGUIresult/comparisonResult_df.ideal.csv'
scenario = "ideal"
analyzeTPFP(file,write_file,scenario)

file = '../moaGUIresult/comparisonResult.csv'
write_file = '../moaGUIresult/comparisonResult_df.total.csv'
scenario = ""
analyzeTPFP(file,write_file,scenario)
