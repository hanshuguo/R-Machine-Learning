
#---------------------------Try out lapply and sapply
1:3
lapply(1:3, function(x) c(x, x^2, x^3))

sapply(1:3, function(x) c(x, x^2, x^3))

lapply(1:3/3, round, digits=3)
sapply(1:3/3, round, digits=3)



#--------------------Try out parallel packages
#install.packages("parallel")

library(parallel)
#====Calculate the number of cores
no_cores <- detectCores() - 1
no_cores

cl <- makeCluster(no_cores)

parSapply(cl, 2:4, function(exponent) 2^exponent)
stopCluster(cl)


#-----------------------------foreach and doParallel
#install.packages("foreach")
#install.packages("doParallel")
library(foreach)
library(doParallel)

no_cores <- detectCores() - 1
cl<-makeCluster(no_cores)
registerDoParallel(cl)

base<-2
foreach(exponent = 2:4, .combine = c)  %dopar%  base^exponent


foreach(exponent = 2:4, .combine = rbind)  %dopar%  base^exponent


foreach(exponent = 2:4,  .combine = list, .multicombine = TRUE)  %dopar% base^exponent


foreach(exponent = 2:4,  .combine = list)  %dopar%   base^exponent
#list(list(result.1, result.2), result.3):

stopImplicitCluster()



############Try same things=========
base <- 2

cl<-makeCluster(2)
registerDoParallel(cl)

foreach(exponent = 2:4,    .combine = c)  %dopar%   base^exponent

stopCluster(cl)


base <- 2
cl<-makeCluster(2)
registerDoParallel(cl)


base <- 2
test <- function (exponent) {
  foreach(exponent = 2:4,  .combine = c,   .export = "base")  %dopar%  
    base^exponent
}
test()

stopCluster(cl)



#-------------------------- memory handling----------------
#install.packages("pryr")
library(pryr) # Used for memory analyses

cl<-makeCluster(no_cores)
clusterExport(cl, "a")
clusterEvalQ(cl, library(pryr))

parSapply(cl, X = 1:10, function(x) {address(a)}) == address(a)


cl<-makeCluster(no_cores, type="FORK")
parSapply(cl, X = 1:10, function(x) address(a)) == address(a)

