library(ggplot2)

mydata = read.csv("ex5log - ex2.csv", header = TRUE)

head(mydata)




par(mar = rep(2, 4))

plot(mydata$u[mydata$y == 0], mydata$v[mydata$y == 0], xlab="u", ylab="v")

points(mydata$u[mydata$y == 1], mydata$v[mydata$y == 1], col="blue", pch=3)

legend("topright", c("y=0","y=1"), pch=c(1, 3), col=c("black", "blue"), bty="n")




source("RLR.helper.R")


m = length(mydata$u) # samples

x = hi.features(mydata$u, mydata$v,6)

hi.features(1,2,2)
x
n = ncol(x) # features
y = matrix(mydata$y, ncol=1)

y

# lambda = 1
# use the cost function to check is works
th1 = matrix(0,n)
la = 1
jiter = array(0,c(15,1))
for (i in 1:15) {
  jiter[i] = J(x,y,th1,m,la)
  th1 = th1 - solve(H(x,y,th1,m,la)) %*% grad(x,y,th1,m,la) 
}


plot(jiter, xlab="iterations", ylab="cost J")



th0 = matrix(0,n)
la = 0
for (i in 1:15) {
  th0 = th0 - solve(H(x,y,th0,m,la)) %*% grad(x,y,th0,m,la) 
}

#slove():This generic function solves the equation a %*% x = b for x, 
#where b can be either a vector or a matrix.


# lambda = 10
th10 = matrix(0,n)
la = 10
for (i in 1:15) {
  th10 = th10 - solve(H(x,y,th10,m,la)) %*% grad(x,y,th10,m,la) 
}



u = seq(-1, 1.2, len=200);
v = seq(-1, 1.2, len=200);
z0 = matrix(0, length(u), length(v))
z1 = matrix(0, length(u), length(v))
z10 = matrix(0, length(u), length(v))
for (i in 1:length(u)) {
  for (j in 1:length(v)) {
    z0[j,i] =  hi.features(u[i],v[j],6) %*% th0
    z1[j,i] =  hi.features(u[i],v[j],6) %*% th1
    z10[j,i] =  hi.features(u[i],v[j],6) %*% th10
  }
}

# plots
contour(u,v,z0,nlev = 0, xlab="u", ylab="v", nlevels=0, col="black",lty=2)
contour(u,v,z1,nlev = 0, xlab="u", ylab="v", nlevels=0, col="red",lty=2, add=TRUE)
contour(u,v,z10,nlev = 0, xlab="u", ylab="v", nlevels=0, col="green3",lty=2, add=TRUE)
points(mydata$u[mydata$y == 0], mydata$v[mydata$y == 0])
points(mydata$u[mydata$y == 1], mydata$v[mydata$y == 1], col="blue", pch=3)
legend("topright",  c(expression(lambda==0), expression(lambda==1),expression(lambda==10)), 
       lty=1, col=c("black", "red","green3"),bty="n" )


