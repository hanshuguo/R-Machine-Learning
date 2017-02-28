g = function (z) {
  return (1 / (1 + exp(-z) ))
} 

#plot(g(c(1,2,3,4,5,6)), type="l")

# build hight order feature vector
# for 2 features, for a given degree
hi.features = function (f1,f2,deg) {
  n = ncol(f1)
  ma = matrix(rep(1,length(f1)))
  for (i in 1:deg) {
    for (j in 0:i)    
      ma = cbind(ma, f1^(i-j) * f2^j)
  }
  return(ma)
} 
hi.features(c(1,2), c(3,4),2)
# creates: 1 u v u^2 uv v^2 ...

h = function (x,th) {
  return(g(x %*% th))
} 

#1 %*% 1

# derivative of J 
grad = function (x,y,th,m,la) {
  G = (la/m * th)
  G[1,] = 0
  return((1/m * t(x) %*% (h(x,th) - y)) +  G)
} 

#grad(x,y,th,m,la)

H = function (x,y,th,m,la) {
  n = length(th)
  L = la/m * diag(n)
  L[1,] = 0
  return((1/m * t(x) %*% x * diag(h(x,th)) * diag(1 - h(x,th))) + L)
} 
#H(x,y,th,m,la)

# cost function
J = function (x,y,th,m,la) {
  pt = th
  pt[1] = 0
  A = (la/(2*m))* t(pt) %*% pt
  return((1/m * sum(-y * log(h(x,th)) - (1 - y) * log(1 - h(x,th)))) + A)
} 
# J(x,y,th,m,la)
