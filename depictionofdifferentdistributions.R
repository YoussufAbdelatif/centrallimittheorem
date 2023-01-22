set.seed(100)

par(mfrow=c(2,2))

n=10
p=1/5
x=0:10
p=dbinom(x,size=n,prob=p)
plot(x,p,type="h",xlim=c(-1,11),ylim=c(0,0.5),lwd=2,col="blue",ylab="p")
points(x,p,pch=16,cex=2,col="dark red")

ccf = c(0,p[1],p[1]+p[2],p[1]+p[2]+p[3],p[1]+p[2]+p[3]+p[4],p[1]+p[2]+p[3]+p[4]+p[5],
        p[1]+p[2]+p[3]+p[4]+p[5]+p[6],p[1]+p[2]+p[3]+p[4]+p[5]+p[6]+p[7],
        p[1]+p[2]+p[3]+p[4]+p[5]+p[6]+p[7]+p[8],
        p[1]+p[2]+p[3]+p[4]+p[5]+p[6]+p[7]+p[8]+p[9],
        p[1]+p[2]+p[3]+p[4]+p[5]+p[6]+p[7]+p[8]+p[9]+p[10])
plot(x,ccf,type="l",col="blue")

x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2.5, sd = 0.5)
plot(x,y,type="l",col="red",xlim = c(0,5))

cumulative<-pnorm(x, 0, 1)
plot(x, cumulative, col="red",type="l")

