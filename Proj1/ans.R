## Loop for prediction model
cval = function (dat, model, predicator, rseed = 6208)
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
    actual = dat[test,]$bugs
    mae[i] = mean(abs(pred - actual))
    rcor[i] = cor.test(pred,actual,method = 'spearman')$estimate
  }
  list(avg.mae = mean(mae), avg.rcor = mean(rcor), detail.mae = mae, detail.rcor = rcor)
}


### The code for the program
work.dir <- "E:/CS 6890"
setwd(work.dir)
library(rpart)
library(MASS)
count =1
files.dir <- dir()

for (directory in files.dir){
  if ((directory == "eclipse")||(directory == "equinox")||(directory == "lucene")||(directory == "mylyn")||(directory == "pde"))
  {
    file.names <- paste(work.dir,directory,directory,sep="/")
    setwd(file.names)
    directory_files <- dir()
    for(k in directory_files)
    {
      if(k=="bug-metrics.csv" || k == "change-metrics.csv" || k == "single-version-ck-oo.csv")
      {
        constant = 5
        ### K=3 ----> bug-metrics.csv
        ### K=4 ----> change-metrics.csv
        ### K=8 ----> single-version-ck-oo.csv
        setwd(file.names)
        jdt.change.metric <- read.csv(k,header =TRUE,row.names = 1,sep = ";")
        jdt.change.metrics <- jdt.change.metric[ , -which(names(jdt.change.metric) %in% c("majorBugs","criticalBugs","highPriorityBugs","nonTrivialBugs"))]
        jdt.change.cor=cor(jdt.change.metrics,method = "spearman")
        jdt.change.cor.bugs= jdt.change.cor ["bugs",]
        
        if (k=="bug-metrics.csv"){
          jdt.change.cor.bugs.matrix = sort (jdt.change.cor.bugs,decreasing = T)[2:3]
          constant = 2
        }
        else 
        {
          jdt.change.cor.bugs.matrix = sort (jdt.change.cor.bugs,decreasing = T)[2:6]
          constant = 5
        }
        names.change.metrix <- names(jdt.change.cor.bugs.matrix)
        jdt.change.metrics.five <- jdt.change.metrics[ , names.change.metrix ]       
        jdt.change.cor.bugs.matrix.cor = cor (jdt.change.metrics.five,method = "spearman")
        print(jdt.change.cor.bugs.matrix) 
        
        ### dataframes that are used to calculate prediction performance while combining matrics (For 3rd question)
        if (k=="bug-metrics.csv")
          jdt.metrices.frame <- jdt.change.metrics[ ,c(names.change.metrix[1],names.change.metrix[2])]
        else if ( k == "change-metrics.csv" )
          jdt.metrices.frame1 <- jdt.change.metrics[ ,c(names.change.metrix[1],names.change.metrix[2])]        
        else if (k == "single-version-ck-oo.csv")
        {
          jdt.metrices.frame2 <- jdt.change.metrics[ ,c(names.change.metrix[1],names.change.metrix[2],"bugs")]
          jdt.metrix.frame <- cbind (jdt.metrices.frame,jdt.metrices.frame1,jdt.metrices.frame2)
        }        
        setwd(paste(work.dir,"Results",sep ="/" ))
        
        ### Prediction performance of each model is estimated.(CHange regression model name and file name to get results for that model)
        for (p in 1:constant){
          var.name <- paste("bugs~", names.change.metrix[p],sep = "")
          cval.jdt.lm.loc <- cval(jdt.change.metrics,lm,var.name)
          cval.jdt.lm.loc_a<-cbind(names.change.metrix[p],cval.jdt.lm.loc$avg.mae,cval.jdt.lm.loc$avg.rcor,directory,k)        
          write.table(cval.jdt.lm.loc_a,file = "Metrices_lm_perf.csv",sep = ",",na = "NA",append = TRUE,col.names=FALSE)           
        }
        
        ### Will write the list of all the top metrics from all datasets to a file.
        jdt.change.cor.bugs.matrix1 = cbind(jdt.change.cor.bugs.matrix,directory,k)
        write.table(jdt.change.cor.bugs.matrix1,file = "Top_metrics.csv",sep = ",",col.names = FALSE,na = "NA",append = TRUE)
        
        ### will write the top metrics in each metrics to seperate files so that i have a track of
        ### top metrics.
        file.name.r = paste ("Top_metrics",count,".csv",sep="")
        jdt.change.metrics.five1 = cbind(jdt.change.metrics.five,directory,k)
        write.table(jdt.change.metrics.five1,file = file.name.r,row.names=FALSE,sep = ",",append = TRUE)
        
        ### Predicator as bugs~A1+A2 or B1+B2 or C1+C2 in each dataset.(CHange regression model name and file name to get results for that model)
        dir.one <- read.csv(file.name.r,header =TRUE,sep = ",")
        dir.one.names <- names(dir.one)
        var.name2 <- paste("bugs~", dir.one.names[1],"+",dir.one.names[2],sep = "")
        cval.jdt.lm.loc2 <- cval(jdt.change.metrics,lm,var.name2)
        cval.jdt.lm.loc3<-cbind(var.name2,cval.jdt.lm.loc2$avg.mae,cval.jdt.lm.loc2$avg.rcor,directory,k)
        write.table(cval.jdt.lm.loc3,file = "Top_2_metrics_lm.csv",sep = ",",na = "NA",append = TRUE,col.names=FALSE)     
        count <- count+1
      }
    }
    
    ### Predicator as bugs~A1+A2+B1+B2+C1+C2 in each dataset.(CHange regression model name and file name to get results for that model)
    frame.name <- names(jdt.metrix.frame)
    var.name3 <- paste("bugs~", frame.name[1],"+",frame.name[2],"+",frame.name[3],"+",frame.name[4],"+",frame.name[5],"+",frame.name[6],sep = "")
    cval.jdt.lm1 <- cval(jdt.metrix.frame,lm,var.name3)
    cval.jdt.lm2 <- cbind(var.name3,cval.jdt.lm1$avg.mae,cval.jdt.lm1$avg.rcor,directory)
    write.table(cval.jdt.lm2,file = "All_metrics_lm.csv",sep = ",",na = "NA",append = TRUE,col.names=FALSE) 
    
    ### Predicator as bugs~A1+B1+C1 in each dataset.(CHange regression model name and file name to get results for that model)
    frame.name <- names(jdt.metrix.frame)
    var.name4 <- paste("bugs~", frame.name[1],"+",frame.name[3],"+",frame.name[5],sep = "")
    cval.jdt.lma1 <- cval(jdt.metrix.frame,lm,var.name4)
    cval.jdt.lma2 <- cbind(var.name4,cval.jdt.lma1$avg.mae,cval.jdt.lma1$avg.rcor,directory)
    write.table(cval.jdt.lma2,file = "All_metrics_top_lm.csv",sep = ",",na = "NA",append = TRUE,col.names=FALSE) 
  }
}


## Read csv files and will find the correlation matrix
setwd("C:/Users/VeNuSs/Documents/CS 6890/Results")
count =1
for (i in 1:15){
  name <- paste("Top_metrics",count,".csv",sep="")
  jdt.change.metrices.wcorr <- read.csv(name,header =TRUE,sep = ",")
  jdt.change.metrics <- jdt.change.metrices.wcorr[ , -which(names(jdt.change.metrices.wcorr) %in% c("directory","k"))]
  jdt.change.metrices.corr = cor (jdt.change.metrics,method = "spearman")
  write.table(jdt.change.metrices.corr,file = "Correlation.csv",append=TRUE,sep = ",")
  count =count+1; 
}
jdt.change.metrices.wcorr <- read.csv("Correlation.csv",header =FALSE,sep = ",")
View(jdt.change.metrices.wcorr)
?write.csv
?read.csv

###########Plotting in R
setwd(paste(work.dir,"Results",sep ="/" ))
metrices.top.lm <- read.csv("All_metrics_lm.csv",header = FALSE,sep = ",",col.names=c("a","b","c","d","e"))
metrices.top.lm1 <- read.csv("All_metrics_glm.csv",header = FALSE,sep = ",",col.names=c("a","b","c","d","e"))
metrices.top.lm2 <- read.csv("All_metrics_rpart.csv",header = FALSE,sep = ",",col.names=c("a","b","c","d","e"))

plot(metrices.top.lm$c,type="o",lwd=2,xaxt="n",col="red",xlab="Dataset",ylab="Mean absolute error",main="Combining Top 2 metrics", ylim=c(0,1.5))
axis(1,at=1:length(metrices.top.lm$e),labels=metrices.top.lm$e)
lines(metrices.top.lm1$c,col="black",type="o",lwd=2)
lines(metrices.top.lm2$c,col="orange",type="o",lwd=2)
legend("topright",legend=c("lm","rpart","glm"),lty=1,lwd=2,pch=21,col=c("red","black","orange"),ncol=2,
       bty="n",cex=0.8,text.col=c("red","black","orange"),inset=0.01)


plot(metrices.top.lm$d,type="o",lwd=2,xaxt="n",col="red",xlab="Dataset",ylab="Mean absolute error",main="Combining Top 2 metrics")
axis(1,at=1:length(metrices.top.lm$e),labels=metrices.top.lm$e)
lines(metrices.top.lm1$d,col="black",type="o",lwd=2)
lines(metrices.top.lm2$d,col="orange",type="o",lwd=2)
legend("topright",legend=c("lm","rpart","glm"),lty=1,lwd=2,pch=21,col=c("red","black","orange"),ncol=2,
       bty="n",cex=0.8,text.col=c("red","black","orange"),inset=0.01)

setwd(paste(work.dir,"Results",sep ="/" ))
metrices.top.lm <- read.csv("a1.csv",header = TRUE,sep = ",",col.names=c("a","b","c","d","e","f","g","h"))
plot(metrices.top.lm$b,type="o",lwd=2,xaxt="n",col="red",xlab="Top Metrices",ylab="Mean absolute error",main="Single metrice as predictor",ylim=c(0.4,1.8))
axis(1,at=1:length(metrices.top.lm$b),labels=metrices.top.lm$a)
lines(metrices.top.lm$c,col="black",type="o",lwd=2)
lines(metrices.top.lm$d,col="orange",type="o",lwd=2)
legend("center",legend=c("lm","rpart","glm"),lty=1,lwd=3,pch=21,col=c("red","black","orange"),ncol=2,
       bty="n",cex=0.6,text.col=c("red","black","orange"),inset=0.04)

?legend
?plot



