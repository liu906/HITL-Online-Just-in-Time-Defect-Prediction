

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
setwd('../experimentResult/')

folders <- list.files(path = '.',pattern = 'RQ1')
for (folder in folders) {
  files <- list.files(folder,pattern = 'detail')
  for (file in files) {
    df <- read.csv(file.path(folder,file),check.names = FALSE)
    df$`G-mean for recall` = sqrt(as.numeric(df$`Recall for class 0 (percent)`) * as.numeric(df$`Recall for class 1 (percent)`))
    write.csv(df,file = file.path(folder,file),row.names = FALSE)
  }
}
