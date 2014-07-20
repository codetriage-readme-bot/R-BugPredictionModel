setwd ("E:\\CS 6890\\proj2")
b1 = scan ("B1.txt")
b2 = scan ("B2.txt")
b3 = scan ("B3.txt")
b4 = scan ("B4.txt")
b5 = scan ("B5.txt")
b6 = scan ("B6.txt")
b7 = scan ("B7.txt")

e1 = scan ("E1.txt")
e2 = scan ("E2.txt")
e3 = scan ("E3.txt")
e4 = scan ("E4.txt")
e5 = scan ("E5.txt")
e6 = scan ("E6.txt")

t1 = scan ("T1.txt")
t2 = scan ("T2.txt")
t3 = scan ("T3.txt")
t4 = scan ("T4.txt")

e2_2.1=scan("E2_2.1.txt")
e2_2=scan("E2_2.txt")
e2_3=scan("E2_3.txt")

e3_3.1=scan("E3_3.1.txt")
e3_2=scan("E3_2.txt")
e3_3=scan("E3_3.txt")

e4_2=scan("E4_2.txt")
e4_3=scan("E4_5.txt")
e4_4=scan("E4_4.txt")
e4_5=scan("E4_5.txt")

#Function to estimate values from other version
estimate = function(bug,k2,l2) {
  sum.e= sum(bug)
  error= rep(1, length(bug))
  temp_sum = rep(1, length(bug))
  for (i in 2:length(bug)){
    mod1 = nls(y ~ n * dweibull(x, shape = k2, scale = l2), data = data.frame(x = 1:i, y = bug[1:i]), start = list(n = sum(bug[1:i])))
    n.e = summary(mod1)$coefficients[1,1]
    error[i]=abs(n.e-sum(bug))/sum(bug)
    temp_sum[i]=n.e
  }
  error
}

#Function to estimate values from other projects
forecast = function(bug, k = 3.85, l = 2.95) {  
  err = aic = rep(0, length(bug))
  vol = bug[1]/dweibull(1, shape = k, scale = l)
  total = sum(bug)
  err[1] = abs(vol - total)/total
  aic[1] = 100
  for (i in 2:length(bug)) {
    if (i <= 5)
      mod = nls(y ~ n * dweibull(x, shape = k, scale = l), data = data.frame(x = 1:i, y = bug[1:i]), start = list(n = sum(bug[1:i])))
    else
      mod = nls(y ~ n * dweibull(x, shape = k, scale = l), data = data.frame(x = 1:i, y = bug[1:i]), start = list(n = sum(bug[1:i]), l = 2.95))		
    vol = summary(mod)$coefficients[1,1]
    err[i] = abs(vol-total)/total
    aic[i] = AIC(mod)
  }
  list(error = err, aic = aic)
} 

### Q 3(A)
plot(estimate(e3_3.1,1.797007,2.6925918),type="o",lwd=2,col="blue",ylim=c(0,0.7),xlab = "Number of Quarters since release rate",ylab = "Relative error of estimate")
lines(estimate(e2_3,4.476521,3.0385232),type="o",lwd=2,col="red",xlab = "Number of Quarters since release rate",ylab = "Number of defects")
lines(estimate(e4_4,9.595919,10.3551379),type="o",lwd=2,col="black",xlab = "Number of Quarters since release rate",ylab = "Number of defects")
legend("topright",legend=c("Eclipse JDT","Eclipse PDE","Eclipse CDT"),lty=1,lwd=3,pch=21,col=c("blue","red","black"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","black"),inset=0.04)

### Q 3(B)
para=read.csv(file="buggrate.csv",header=TRUE)
para.mod.shape = lm(shape ~ cycle+type+cost,data=para[1:31,])
para.mod.scale = lm(scale ~ cycle+type+cost,data=para[1:31,])
predict(para.mod.shape,para[32:34,])
predict(para.mod.scale,para[32:34,])
fc1<-forecast(e3_3.1,3.85,2.95)
fc2<-forecast(e3_2,3.85,2.95)
fc3<-forecast(e3_3,3.85,2.95)
plot(fc1$error,type="o",lwd=2,col="blue",ylim=c(0,0.4),xlab = "Number of Quarters since release rate",ylab = "Relative error of estimate")
title("Eclipse JDT",cex.main = 1.5,font.main= 2, col.main= "black")
lines(fc2$error,type="o",lwd=2,col="red",xlab = "Number of Quarters since release rate",ylab = "Number of defects")
lines(fc3$error,type="o",lwd=2,col="black",xlab = "Number of Quarters since release rate",ylab = "Number of defects")
legend("topright",legend=c("V3.1","V2","V3"),lty=1,lwd=3,pch=21,col=c("blue","red","black"),ncol=2,
       bty="n",cex=0.6,text.col=c("blue","red","black"),inset=0.04)
