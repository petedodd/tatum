## synthetic data for testing

## make some data
T <- 20
A <- 81
NT_obs <- 4
NAge_obs <- 16
b <- rep(0.02,T)
AgeTops <- seq(from=5,by=5,len=NAge_obs)

## true parms
lambda0 <- 0.05
alpha <- 0.01
rho <- 0.05
R <- rep(1,A)

## means
f <- matrix(nrow=T,ncol=A)
for(i in 1:A)
  f[1,i] = (1-exp(-(R[i] * lambda0 + rho) * (i-0.5))) * rho / (R[i] * lambda0 + rho)
for(i in 2:T){
  f[i,1] = b[i] * exp(-lambda0*exp(-alpha*(i-1))) +
    (rho / (R[1] * lambda0*exp(-alpha*(i-1))+rho) ) * (1-exp(-lambda0*exp(-alpha*(i-1))))
  for(j in 2:A){
    f[i,j] = f[i-1,j-1] * exp(-lambda0*exp(-alpha*(i-1))) +
      (rho / (R[j] * lambda0*exp(-alpha*(i-1))+rho) ) * (1-exp(-lambda0*exp(-alpha*(i-1))))
  }
}

## data
fullN <- matrix(10,nrow=T,ncol=A)
fullK <- matrix(rbinom(n=length(fullN),size=c(fullN),prob=c(f)),
                nrow=T,ncol=A)
K <- N <- matrix(nrow=NT_obs,ncol=NAge_obs)#observation denominators
for(i in 1:NT_obs){
  for(j in 1:NAge_obs){
    if(j==1){
      N[i,j] <- sum(fullN[test_data$ObsT[i],1:test_data$AgeTops[j]])
      K[i,j] <- sum(fullK[test_data$ObsT[i],1:test_data$AgeTops[j]])
    } else {
      N[i,j] <- sum(fullN[test_data$ObsT[i],test_data$AgeTops[j-1]:test_data$AgeTops[j]])
      K[i,j] <- sum(fullK[test_data$ObsT[i],test_data$AgeTops[j-1]:test_data$AgeTops[j]])
    }
  }
}

## add to stan data
test_data$N <- N
test_data$K <- K

## make data
test_data <- list(N=N,#denominator matrix NT_obs x NAge_obs
                  K=K,#numerator matrix NT_obs x NAge_obs
                  b=b,  #per capita birth rate (length T)
                  ObsT = c(5,10,15,20), #times of observations
                  AgeTops=AgeTops,#locations of age tops
                  R=R #relative foi by age (length A)
                  )

## test_data <- list(T = T, #number of times
##                   A = A,   #number of ages = 81
##                   b = b,  #per capita birth rate
##                   NT_obs = NT_obs, #number of time observations
##                   ObsT = c(5,10,15,20), #times of observations
##                   NAge_obs = NAge_obs, # number of age cats
##                   AgeTops = seq(from=5,by=5,len=NAge_obs),#locations of age tops
##                   R = rep(1,A)#relative foi by age
##                   )
