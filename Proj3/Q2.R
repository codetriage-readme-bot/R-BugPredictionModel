work.dir <- setwd ("E:/CS 6890/proj3")

aic.pca = function(pca, bug, k) {
  res = rep(0, k)
  for (i in 1:k) {
    dat = data.frame(bug = bug, pca[,1:i])
    mod = lm(bug ~ ., data = dat)
    res[i] = AIC(mod)
  }
  res
}

cval = function (dat, model, predicator, rseed = 4307)
{
  set.seed(rseed)
  fun = match.fun(model)
  mae =  rcor = rep(0,30)
  n= nrow(dat)
  for (i in 1:30)
  {
    test = sample.int(n,n*0.2)
    mod = fun ( predicator, data = dat[-test,] )
    pred = predict (mod,dat[test,])
    actual = dat[test,]$bug
    rcor[i] = cor.test(pred,actual,method = 'spearman')$estimate
  }
  avg.rcor = mean(rcor)
  rcor
}

cor.pca = function(pca, bug, k) {
  res = rep(0, k)
  for (i in 1:k) {
    dat = data.frame(bug = bug, pca[,1:i])
    # training and predicting 30%
    cval.res = cval(dat, lm, bug ~ .)
    res[i] = cval.res
  }
  res
}



work.dir <- setwd ("E:/CS 6890/proj3")
files.dir <- dir()
for (directory in files.dir){
  if ((directory == "jdt")||(directory == "equinox")||(directory == "lucene")||(directory == "mylyn")||(directory == "pde"))
  {
    file.names <- paste(work.dir,directory,sep="/")
    setwd(file.names)
    count = 5
    while (count <=100)
    {
      setwd(file.names)
      CSV.name <- paste("topic",count,"log.csv",sep="")
      print (CSV.name)
      topic.file = read.csv(file = CSV.name,header = TRUE, row.names=1, sep=",")
      count_1 = 1+count
      ## Computing the AIC values for the Topic metrics
      jdt.pca = aic.pca(princomp(topic.file[,1:count])$scores,topic.file$bug,count)
      ## Computing the AIC values for the Topic+Base metrics
      jdt.pca.combined = aic.pca(princomp(topic.file[,-count_1])$scores,topic.file$bug,count+3)
      ## Computing the AIC values for the Topic metrics + loc
      jdt.loc = AIC (lm(bug ~ loc, topic.file))
      ## Computing the AIC values for the Topic metrics + bf
      jdt.bf = AIC (lm(bug ~ bf, topic.file))
      ## Computing the AIC values for the Topic metrics + hcm
      jdt.hcm = AIC (lm(bug ~ hcm, topic.file))
      
      ## Computing the AIC values for the Topic metrics
      jdt.pca.cor = cor.pca(princomp(topic.file[,1:count])$scores,topic.file$bug,count)
      ## Computing the AIC values for the Topic+Base metrics
      jdt.pca.combined.cor = cor.pca(princomp(topic.file[,-count_1])$scores,topic.file$bug,count+3)
      
      ## The minimum of all the AIC values computed above
      jdt.pca.min = min (jdt.pca)
      jdt.pca.combined.min = min(jdt.pca.combined)
      jdt.loc.min = min (jdt.loc)
      jdt.bf.min = min (jdt.bf)
      jdt.hcm.min = min (jdt.hcm)
      jdt.pca.cor.max = max(jdt.pca.cor)
      jdt.pca.combined.cor.max = max (jdt.pca.combined.cor)
      
      ## The index of all the minimum AIC values computed above
      jdt.pca.which = which.min (jdt.pca)
      jdt.pca.combined.which = which.min(jdt.pca.combined)
      jdt.loc.which = which.min (jdt.loc)
      jdt.bf.which = which.min (jdt.bf)
      jdt.hcm.which = which.min (jdt.hcm)
      jdt.pca.cor.which = which.max(jdt.pca.cor)
      jdt.pca.combined.cor.which = which.max(jdt.pca.combined.cor)
      
      topic.loc.cor = max(cval(topic.file, lm, bug ~ loc))      
      topic.bf.cor = max(cval(topic.file, lm, bug ~ bf))      
      topic.hcm.cor = max(cval(topic.file, lm, bug ~ hcm))
      
      jdt.name  <- cbind (jdt.pca.min,jdt.pca.combined.min, jdt.loc.min, jdt.bf.min, jdt.hcm.min,
                          jdt.pca.cor.max,jdt.pca.combined.cor.max,topic.loc.cor,topic.bf.cor,topic.hcm.cor,
                          jdt.pca.which, jdt.pca.combined.which, jdt.loc.which,jdt.bf.which,jdt.hcm.which,
                          jdt.pca.cor.which,jdt.pca.combined.cor.which,CSV.name,directory)         
      
      setwd (paste(work.dir,"Output",sep="/"))
      write.table(jdt.name,file = "Result.csv",sep = ",",na = "NA",append = TRUE,col.names=FALSE)
      count = count+5
    }    
  }
}
    


