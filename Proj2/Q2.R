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

#0.4,6.3
####Change to 1,3 when error
fit.all = function(y) {  
  x = 1:length(y)
  dat = data.frame(x, y)
  res = matrix(nrow = 3, ncol = 2, dimnames = list(c("Weibull", "Gamma", "Rayleigh"), c("R2", "AIC")))  
  sumy = sum(y)
  startl = which.max(y)
  # fit Weibull
  mod1 = nls(y ~ n * dweibull(x, shape = k, scale = l), data = dat, start = list(n = sumy, l = startl, k = 2))
  res1 = summary(mod1)
  res[1,1] = 1 - var(res1$residuals)/var(y)
  res[1,2] = AIC(mod1)
  Weibull=c(total =res1$coefficients[1,1],scale =res1$coefficients[2,1],shape =res1$coefficients[3,1],r2=res[1,1],AIC=res[1,2])
  
  # fit Gamma
  mod2 = nls(y ~ n * dgamma(x, shape = k, scale = l), data = dat, start = list(n = sumy, l = 1, k = 5), trace = T)
  res2 = summary(mod2)
  res[2,1] = 1 - var(res2$residuals)/var(y)
  res[2,2] = AIC(mod2)
  Gamma=c(total = res2$coefficients[1,1],scale =res2$coefficients[2,1],shape =res2$coefficients[3,1],r2=res[2,1],AIC=res[2,2])
  
  # fit Rayleigh
  mod3 = nls(y ~ n * dweibull(x, shape = 2, scale = l), data = dat, start = list(n = sumy, l = startl))
  res3 = summary(mod3)
  res[3,1] = 1 - var(res3$residuals)/var(y)
  res[3,2] = AIC(mod3)
  Rayleigh=c(total =res3$coefficients[1,1],scale =res3$coefficients[2,1],shape=2,r2=res[3,1],AIC=res[3,2])
  
  row.names(res) = c()
  stat = rbind(Weibull,Gamma,Rayleigh)
  stat
}

#### K---shape parameter, lambda----scale parameter
#### AIC less and R sq more
fit1 <- fit.all(e3_3.1)   #JDT
fit2 <- fit.all(e3_2) 
fit3 <- fit.all(e3_3)
fit4 <- fit.all(e2_2.1)
fit5 <- fit.all(e2_2)    #PDE
fit6 <- fit.all(e2_3)
fit7<- fit.all(e4_2)     #0.8,12
fit8<- fit.all(e4_3)     #2.48,4.59,1152.162
fit9<- fit.all(e4_4)      #CDT

fit1
fit2
fit3
fit4
fit5 
fit6
fit7
fit8
fit9


write.table(fit1,file="2ndoutput.csv",append=TRUE,sep=",")
write.table(fit2,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)
write.table(fit3,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)
write.table(fit4,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)
write.table(fit5,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)
write.table(fit6,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)
write.table(fit7,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)
write.table(fit8,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)
write.table(fit9,file="2ndoutput.csv",append=TRUE,sep=",",col.names=FALSE)

