df <- read.csv('temp.csv')
wilcox.test(df$a,df$b,paired = T)
