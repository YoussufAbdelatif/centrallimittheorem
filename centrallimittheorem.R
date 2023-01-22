par(mfrow=c(2,2))


x = sample(x = seq(0,6,0.1), size = 5, replace = TRUE)
x
y =  sample(x = seq(0,6,0.1), size = 5, replace = TRUE)
y

Summe <- x + y
Summe

hist(Summe,freq = FALSE, col ="white",breaks=20,xlim = c(-1,12))
lines(density(Summe), col = "red",lwd=3)
lines(density(rnorm(50000,mean = 7,sd = 2.45)),col="blue",lwd=3)

###################################################################################


x = sample(x = seq(0,6,0.1), size = 50, replace = TRUE)
x
y =  sample(x = seq(0,6,0.1), size = 50, replace = TRUE)
y

Summe <- x + y
Summe

hist(Summe,freq = FALSE, col ="white",breaks=20,xlim = c(-1,12))
lines(density(Summe), col = "red",lwd=3)
lines(density(rnorm(50000,mean = 7,sd = 2.45)),col="blue",lwd=3)

##################################################################################


x = sample(x = seq(0,6,0.1), size = 500, replace = TRUE)
x
y =  sample(x = seq(0,6,0.1), size = 500, replace = TRUE)
y

Summe <- x + y
Summe

hist(Summe,freq = FALSE, col ="white",breaks=20,xlim = c(-1,12))
lines(density(Summe), col = "red",lwd=3)
lines(density(rnorm(50000,mean = 7,sd = 2.45)),col="blue",lwd=3)

###################################################################################





Summe <- x + y
Summe

hist(Summe,freq = FALSE, col ="white",breaks=20,xlim = c(-1,12))
lines(density(Summe), col = "red",lwd=3)
lines(density(rnorm(50000,mean = 7,sd = 2.45)),col="blue",lwd=3)




dev.off()




x <- rnorm(30,mean = 10,sd = 6.04)
mean(x)
sd(x)
o = 10+2.245*6.04/sqrt(30)
u = 10-2.245*6.04/sqrt(30)

x = sample(x = seq(0,6,0.1), size = 50000, replace = TRUE)
x
y =  sample(x = seq(0,6,0.1), size = 50000, replace = TRUE)
i = 1


plot(0,xlim = c(5,14),ylim=c(0,14),ylab="",yaxt="n")

abline(v=o,col="red",lwd=8)
abline(v=u,col="red",lwd=8)

d = c()
for (i in 1:500){
  sample_i = rnorm(30, mean = 10, sd = 6.0)
  mean_i = mean(sample_i)
  abline(v=mean_i,col="blue")
  if (mean_i <= o & mean_i >=u) {
    d[i]=1
  } else {
    d[i]=0
  }
}


(sum(as.numeric(d))/length(d))*100



dev.off()

















Einkommen = rnorm(50, mean = 10, sd = 6.0)
Ausgaben = 5+0.8*Einkommen+rnorm(50, mean = 0, sd = 1.0)
par(mfrow = c(1,2))
plot(Einkommen,Ausgaben,ylab="Ausgaben",xlab="Einnahmen")


cor(Einkommen,Ausgaben)

lm1 =lm(Ausgaben ~ Einkommen)
lm1
abline(lm1,col="red")


plot(lm1$residuals,ylab="Residuen",xlab="Einkommen")
abline(h=0,col="black")
