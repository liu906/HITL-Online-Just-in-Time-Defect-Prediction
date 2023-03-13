
compute_validity_metric <- function(ideal_evaluation, estimated_evaluation){
  return(1 - abs(ideal_evaluation-estimated_evaluation))
}


    