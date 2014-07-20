setwd ("E:\\CS 6890\\proj2")


# sim(X,Y) = 1 - Sum (X(t)-Y(t)/2)
curve.sim = function(x , y) {
  len = max(length(x), length(y))
  sumx = sum(x)
  sumy = sum(y)
  xx = yy = rep(0, len)
  for (i in 1:length(x)) xx[i] = x[i]/sumx
  for (i in 1:length(y)) yy[i] = y[i]/sumy
  1 - sum(abs(xx-yy))/2
}


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
e4_3=scan("E4_3.txt")
e4_4=scan("E4_4.txt")

len=1:length(e2_3)
plot(len,e2_3,type="o",lwd=2,col="red", xlab = "Number of Quarters since release rate",ylab = "Number of defects")
title("Eclipse PDE",cex.main = 1.5,font.main= 2, col.main= "black")
axis(1,at=1:length(e2_3),labels=len)
lines(e2_2.1,col="black",type="o",lwd=2)
lines(e2_2,col="blue",type="o",lwd=2)
legend("topright",legend=c("V3","V2.1","V2"),lty=1,lwd=3,pch=21,col=c("red","black","blue"),ncol=2,
       bty="n",cex=0.6,text.col=c("red","black","blue"),inset=0.04)

plot(e3_2,type="o",lwd=2,col="red", xlab = "Number of Quarters since release rate",ylab = "Number of defects")
title("Eclipse JDT",cex.main = 1.5,font.main= 2, col.main= "black")
axis(1,at=1:length(e3_2),labels=1:length(e3_2))
lines(e3_3.1,col="black",type="o",lwd=2)
lines(e3_3,col="blue",type="o",lwd=2)
legend("topright",legend=c("V2","V3.1","V3"),lty=1,lwd=3,pch=21,col=c("red","black","blue"),ncol=2,
       bty="n",cex=0.6,text.col=c("red","black","blue"),inset=0.04)


plot(e4_2,type="o",lwd=2,col="red",xaxt="n",xlab = "Number of Quarters since release rate",ylab = "Number of defects")
title("Eclipse CDT",cex.main = 1.5,font.main= 2, col.main= "black")
axis(1,at=1:length(e4_2),labels=1:length(e4_2))
lines(e4_3,col="black",type="o",lwd=2)
lines(e4_4,col="blue",type="o",lwd=2)
legend("topright",legend=c("V2","V3","V4"),lty=1,lwd=3,pch=21,col=c("red","black","blue"),ncol=2,
       bty="n",cex=0.6,text.col=c("red","black","blue"),inset=0.04)


plot(b5,type="o",lwd=2,xaxt="n",col="red")
axis(1,at=1:length(b5),labels=b5)
lines(b1,col="black",type="o",lwd=2)
lines(b3,col="orange",type="o",lwd=2)
lines(b4,col="blue",type="o",lwd=2)
lines(b2,col="purple",type="o",lwd=2)
lines(b6,col="green",type="o",lwd=2)
lines(b7,col="yellow",type="o",lwd=2)
legend("topright",legend=c("b5","b1","b3","b4","b2","b6","b7"),lty=1,lwd=3,pch=21,col=c("red","black","orange","blue","purple","green","yellow"),ncol=2,
       bty="n",cex=0.6,text.col=c("red","black","orange","blue","purple","green","yellow"),inset=0.04)


plot(e1,type="o",lwd=2,xaxt="n",col="red")
axis(1,at=1:length(e1),labels=e1)
lines(e2,col="black",type="o",lwd=2)
lines(e3,col="orange",type="o",lwd=2)
lines(e4,col="blue",type="o",lwd=2)
lines(e5,col="purple",type="o",lwd=2)
lines(e6,col="green",type="o",lwd=2)
legend("topright",legend=c("e1","e2","e3","e4","e5","e6"),lty=1,lwd=3,pch=21,col=c("red","black","orange","blue","purple","green"),ncol=2,
       bty="n",cex=0.6,text.col=c("red","black","orange","blue","purple","green"),inset=0.04)



plot(t2,type="o",lwd=2,xaxt="n",col="red")
axis(1,at=1:length(t2),labels=t2)
lines(t1,col="black",type="o",lwd=2)
lines(t3,col="orange",type="o",lwd=2)
lines(t4,col="blue",type="o",lwd=2)
legend("topright",legend=c("t2","t1","t3","t4"),lty=1,lwd=3,pch=21,col=c("red","black","orange","blue"),ncol=2,
       bty="n",cex=0.6,text.col=c("red","black","orange","blue"),inset=0.04)

curve.sim(e2_3,e2_2.1)
curve.sim(e2_3,e2_2)
curve.sim(e2_2,e2_2.1)

curve.sim(e4_2,e4_3)
curve.sim(e4_3,e4_4)
curve.sim(e4_4,e4_2)

curve.sim(e3_3,e3_3.1)
curve.sim(e3_3,e3_2)       #83.9
curve.sim(e3_2,e3_3.1)

curve.sim (b1,b2)
curve.sim (b1,b3)
curve.sim (b1,b4)
curve.sim (b1,b5)
curve.sim (b1,b6)
curve.sim (b1,b7)
curve.sim (b2,b3)
curve.sim (b2,b4)
curve.sim (b2,b5)
curve.sim (b2,b6)
curve.sim (b2,b7)
curve.sim (b3,b4)
curve.sim (b3,b5)
curve.sim (b3,b6)
curve.sim (b3,b7)
curve.sim (b4,b5)
curve.sim (b4,b6)
curve.sim (b4,b7)
curve.sim (b5,b6)
curve.sim (b5,b7)
curve.sim (b6,b7)

curve.sim (e1,e2)
curve.sim (e1,e3)
curve.sim (e1,e4)
curve.sim (e1,e5)
curve.sim (e1,e6)
curve.sim (e2,e3)
curve.sim (e2,e4)
curve.sim (e2,e5)
curve.sim (e2,e6)
curve.sim (e3,e4)
curve.sim (e3,e5)
curve.sim (e3,e6)
curve.sim (e4,e5)
curve.sim (e4,e6)
curve.sim (e5,e6)

curve.sim (t1,t2)
curve.sim (t1,t3)
curve.sim (t1,t4)
curve.sim (t2,t3)
curve.sim (t2,t4)
curve.sim (t3,t4)

sim_values<- read.csv("barchart_data.csv",header=TRUE)
names = c("lightblue", rep("mistyrose",2), "lavender",rep("lightcyan",7), rep("cornsilk",8),rep("deepskyblue",3))
barplot(sim_values$Curve_Similarity,ylim=c(0.82,1),col=names,cex.names=0.5,
        names.arg=sim_values$Versions,las=2.4)
#,legend("left", legend = c("Eclipse PDE", "Eclipse CDT","Eclipse JDT","OpenBSD","Eclipse","Tomcat"), 
#fill = c("lightblue", "mistyrose","lavender","lightcyan","cornsilk","deepskyblue")),bty="n"
box()
grid()

