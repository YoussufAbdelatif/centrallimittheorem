


plot(0,xlim = c(5,20),ylim=c(0,14),ylab="",yaxt="n")
abline(v=10,col="red",lwd=10)

###################1
mm=c()
for (i in 1:1000){
  x1 = rnorm(3, mean = 10, sd = 5.0)
  x2 = rnorm(1, mean = 10, sd = 5.0)
  x3 = rnorm(1, mean = 10, sd = 5.0)
  mm[i] = 0.2*x1[1]+0.4*x1[2]+0.4*x1[3]
  abline(v=mm[i],col="lightblue")
}
mean(mm)
abline(v=mean(mm),col="pink",lwd=8)
abline(v=sd(mm)^2,col="black",lwd=8)





##########################2


mc=c()
for (i in 1:10000){
  x1 = rnorm(1, mean = 10, sd = 5.0)
  x2 = rnorm(1, mean = 10, sd = 5.0)
  mc[i] = (x1+x2)/2
  abline(v=mc[i],col="lightblue")
}
mean(mc)
abline(v=mean(mc),col="pink",lwd=8)
abline(v=sd(mc)^2,col="green",lwd=8)



#########################3



dev.off()

