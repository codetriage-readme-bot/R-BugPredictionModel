work.dir <- setwd ("E:/CS 6890/proj3")


file.names <- paste(work.dir,"Results",sep="/")
setwd(file.names)

getwd()
topic.file = read.csv(file = "Result.csv",header = FALSE, sep=",")

plot(topic.file$V4[1:20],type = "l")
setwd(file.names)

len1 = seq(5,100,by=5)
plot(topic.file$V2[1:20],type="o",xaxt = "n",lwd=2,ylim=c(700,1250),col="blue",xlab = "Number of topics",ylab = "AIC scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V3[1:20],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V4[1:20],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V5[1:20],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V6[1:20],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "AIC scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="EQUINOX Explanation Power")



len1 = seq(5,100,by=5)
plot(topic.file$V2[21:40],type="o",xaxt = "n",lwd=2,ylim=c(2000,2725),col="blue",xlab = "Number of topics",ylab = "AIC scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V3[21:40],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V4[21:40],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V5[21:40],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V6[21:40],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "AIC scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="JDT Explanation Power")


len1 = seq(5,100,by=5)
plot(topic.file$V2[41:60],type="o",xaxt = "n",lwd=2,ylim=c(400,1500),col="blue",xlab = "Number of topics",ylab = "AIC scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V3[41:60],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V4[41:60],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V5[41:60],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V6[41:60],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "AIC scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="LUCENE Explanation Power")

len1 = seq(5,100,by=5)
plot(topic.file$V2[61:80],type="o",xaxt = "n",lwd=2,ylim=c(1600,2500),col="blue",xlab = "Number of topics",ylab = "AIC scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V3[61:80],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V4[61:80],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V5[61:80],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V6[61:80],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "AIC scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="MYLYN Explanation Power")

len1 = seq(5,100,by=5)
plot(topic.file$V2[81:100],type="o",xaxt = "n",lwd=2,ylim=c(2300,4700),col="blue",xlab = "Number of topics",ylab = "AIC scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V3[81:100],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V4[81:100],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V5[81:100],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "AIC scores")
lines(topic.file$V6[81:100],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "AIC scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="PDE Explanation Power")


len1 = seq(5,100,by=5)
plot(topic.file$V7[1:20],type="o",xaxt = "n",lwd=2,ylim=c(0.47,0.9),col="blue",xlab = "Number of topics",ylab = "Correlation Score")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V8[1:20],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "Correlation Score")
lines(topic.file$V9[1:20],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V10[1:20],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V11[1:20],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "Correlation scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="EQUINOX Predictive Power")



len1 = seq(5,100,by=5)
plot(topic.file$V7[21:40],type="o",xaxt = "n",lwd=2,ylim=c(0.35,0.67),col="blue",xlab = "Number of topics",ylab = "Correlation scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V8[21:40],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V9[21:40],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V10[21:40],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V11[21:40],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "Correlation scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="JDT Predictive Power")


len1 = seq(5,100,by=5)
plot(topic.file$V7[41:60],type="o",xaxt = "n",lwd=2,ylim=c(0.2,0.6),col="blue",xlab = "Number of topics",ylab = "Correlation scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V8[41:60],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V9[41:60],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V10[41:60],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V11[41:60],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "Correlation scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="LUCENE Predictive Power")

len1 = seq(5,100,by=5)
plot(topic.file$V7[61:80],type="o",xaxt = "n",lwd=2,ylim=c(0.2,0.55),col="blue",xlab = "Number of topics",ylab = "Correlation scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V8[61:80],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V9[61:80],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V10[61:80],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V11[61:80],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "AIC scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="MYLYN Predictive Power")

len1 = seq(5,100,by=5)
plot(topic.file$V7[81:100],type="o",xaxt = "n",lwd=2,ylim=c(0.2,0.65),col="blue",xlab = "Number of topics",ylab = "Correlation scores")
axis(1,at = 1:length(len1),labels=len1)
lines(topic.file$V8[81:100],type="o",lwd=2,col="red",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V9[81:100],type="o",lwd=2,col="darkslategray4",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V10[81:100],type="o",lwd=2,col="cyan",xlab = "Number of topics",ylab = "Correlation scores")
lines(topic.file$V11[81:100],type="o",lwd=2,col="darkorchid",xlab = "Number of topics",ylab = "Correlation scores")
legend("topright",legend=c("Topic","Topic+Base","loc","bf","hcm"),lty=1,lwd=3,pch=21,col=c("blue","red","darkslategray4","cyan","darkorchid"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","darkslategray4","cyan","darkorchid"),inset=0.06)
title(main="PDE Predictive Power")







