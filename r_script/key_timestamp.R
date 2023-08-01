dataset_path <-'D:/work/real-world-evaluation/commit_guru_dataset/cut2years'
files <- list.files(dataset_path,pattern = 'csv$')
for(file in files){
  
  data <- read.table(file.path(dataset_path,file), header=T, colClasses= c(rep("character", 1), rep("NULL", 16)),sep=',')
  percentages <- seq(0.1,1,0.1)
  res <- data.frame(percentages=percentages,timestamp=data[as.integer(nrow(data)*percentages),])
  dir.create(file.path(dataset_path,'key_timestamp'),showWarnings = F)
  write.csv(res,file.path(dataset_path,'key_timestamp',file),row.names = F,quote = F)
}



for(file in files){
  
  data <- read.table(file.path(dataset_path,file), header=T, colClasses= c(rep("character", 1), rep("NULL", 16)),sep=',')
  nk <- seq(1000,5000,1000)
  res <- data.frame(percentages=nk,timestamp=data[nk,])
  dir.create(file.path(dataset_path,'key_timestamp','top5000Commits'),showWarnings = F)
  write.csv(res,file.path(dataset_path,'key_timestamp','top5000Commits',file),row.names = F,quote = F)
}
