#' Bayesian linear regression with Stan
#'
#' @export
#' 
#' @param N denominator matrix NT_obs x NAge_obs
#' @param K numerator matrix NT_obs x NAge_obs
#' @param b per capita birth rate (length T=number of times)
#' @param ObsT times of observations (length NAge_obs)
#' @param AgeTops locations of age tops
#' @param R relative foi by age (length A=number of underlying ages)
#' @param ari_mu ARI prior log normal mu
#' @param ari_sig ARI prior log normal sigma
#' @param rho_mu Reversion prior log normal mu
#' @param rho_sig Reversion prior log normal sigma
#' @param alpha_mu ARI trend prior normal mu
#' @param alpha_sig ARI trend prior normal sigma
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @examples
#'
#' # run inference on test_data using 4 chains in parallel
#' out <- ari_fixed_agerel(N=test_data$N,#denominator matrix NT_obs x NAge_obs
#' K=test_data$K,#numerator matrix NT_obs x NAge_obs
#' b=test_data$b,  #per capita birth rate (length T)
#' ObsT=test_data$ObsT, #times of observations
#' AgeTops=test_data$AgeTops,#locations of age tops
#' R=test_data$R,            #relative ARI by age
#' iter=1e3,chains=4,cores=4)
#'
#' # inspect parameters of interest
#' prz <- c('lambda0','rho','alpha')
#' summary(samps,pars=prz)
#'
#'
ari_fixed_agerel <- function(N,#denominator matrix NT_obs x NAge_obs
                             K,#numerator matrix NT_obs x NAge_obs
                             ## T, #number of times [get from b]
                             ## A,   #number of ages = 81 [get from R]
                             b,  #per capita birth rate (length T)
                             ## NT_obs, #number of time observations [get from K]
                             ObsT, #times of observations
                             ## NAge_obs, # number of age cats [get from K]
                             AgeTops,#locations of age tops [length NAge_obs]
                             R, #relative foi by age (length A)
                             ari_mu=1e-2, ari_sig=1e-3, #prior for ARI TODO update
                             rho_mu=1e-2,rho_sig=1e-3,  #prior for regression TODO look up
                             alpha_mu=-1e-2,alpha_sig=5e-3, #prior for trend TODO update
                             ...) {
  T <- length(b) #number of times [get from b]
  A <- length(R) #number of ages = 81 [get from R]
  NT_obs <- nrow(N) #number of time observations [get from K]
  NAge_obs <- ncol(N) # number of age cats [get from K]

  ## safeties
  if(ncol(N)!=ncol(K)) stop('N and K need the same number of columns!')
  if(nrow(N)!=nrow(K)) stop('N and K need the same number of rows!')
  if(length(AgeTops)!=NAge_obs) stop('AgeTops needs to have same length as N or K has columns!')

  SD <- list(N = N,                  #denominators
             K = K,                  #numerators
             T = T, #number of times
             A = A,   #number of ages = 81
             b = b,  #per capita birth rate
             NT_obs = NT_obs, #number of time observations
             ObsT= ObsT, #times of observations
             NAge_obs = NAge_obs, # number of age cats
             AgeTops = AgeTops,#locations of age tops
             R = R, #relative foi by age
             ari_mu=ari_mu, ari_sig=ari_sig, #prior for ARI
             rho_mu=rho_mu,rho_sig=rho_sig,  #prior for regression
             alpha_mu=alpha_mu,alpha_sig=alpha_sig #prior for trend
             )
  out <- rstan::sampling(stanmodels$ariest, data = SD, ...)
  return(out)
}
