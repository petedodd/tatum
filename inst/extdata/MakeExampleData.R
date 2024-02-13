## synthetic data for testing

## make some data
T <- 20
A <- 81
NT_obs <- 4
NAge_obs <- 16
AgeTops <- seq(from=5,by=5,len=NAge_obs)

## true parms
lambda0 <- 0.05
alpha <- 0.01
rho <- 0.05
R <- rep(1,A)

## start the object (N & K & T added below)
test_data <- list(ObsT = c(5,10,15,20), #times of observations
                  AgeTops=AgeTops,       #locations of age tops
                  ## N=N,#denominator matrix NT_obs x NAge_obs
                  ## K=K,#numerator matrix NT_obs x NAge_obs
                  ## T=T,#number of times 
                  R=R, #relative foi by age (length A)
                  ari_mu=-4, ari_sig=0.7, #prior for ARI TODO update
                  rho_mu=-3,rho_sig=0.2,  #prior for regression TODO look up
                  alpha_mu=0,alpha_sig=5e-2 #prior for trend TODO update
                  )


## means
## i,j = t,a
h <- f <- matrix(nrow=T,ncol=A)

## create h
for(j in 1:A){
  for(i in 1:T){
    h[i,j] <- R[j] * lambda0 * exp(-alpha*(i-1)) + rho
  }
}

## f(t,0)=1
for(i in 1:T){
  f[i,1] = 1.0
}
## a>0
for(j in 2:A){
  ## t=0 eqm
  DH <- h[1,j]
  f[1,j] = f[1,j-1] * exp(-DH) + rho * (1-exp(-DH))/DH
  ## t>0
  for(i in 2:T){
    DH <- h[i,j]
    f[i,j] = f[i-1,j-1] * exp(-DH) + rho * (1-exp(-DH))/DH
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
test_data$T <- T
