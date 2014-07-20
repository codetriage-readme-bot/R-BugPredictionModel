work.dir <- setwd ("E:/CS 6890/proj3")

topic.cor = function(dat, k) {
  res = rep(0, k)
  for (i in 1:k) res[i] = cor.test(dat[,i], dat$bug, method = 'spearman')$estimate
  res
}



work.dir <- setwd ("E:/CS 6890/proj3")
files.dir <- dir()
files = c("topic10log.csv","topic20log.csv","topic50log.csv","topic100log.csv")
for (directory in files.dir){
  if ((directory == "jdt")||(directory == "equinox")||(directory == "lucene")||(directory == "mylyn")||(directory == "pde"))
  {
    file.names <- paste(work.dir,directory,sep="/")
    setwd(file.names)
    print(getwd())
    allCSV = dir(file.names,pattern=".csv",recursive=TRUE)
    for(file in allCSV)
    {
      if (file %in% files)
      {
        print(file) 
        topic.file = read.csv(file = file,header = TRUE, row.names=1, sep=",")
        if (file =="topic10log.csv")
          topic10log = topic.cor(topic.file,10)
        else if (file =="topic20log.csv")
          topic20log = topic.cor(topic.file,20)      
        else if (file =="topic50log.csv")
          topic50log = topic.cor(topic.file,50)
        else if (file =="topic100log.csv")
          topic100log = topic.cor(topic.file,100)       
      }
    }
    if (directory  == "lucene")
      {
      boxplot(topic10log,topic20log,topic50log,topic100log,main = "LUCENE", ylab ="Bug Correlation", xlab ="Topic Metrics",
              col = c("dodgerblue","darkorchid1","gold1","lightcoral"), at = c(1,2,3,4), 
              names = c('10', '20', '50', '100'))
      
      
    }
  }
}


